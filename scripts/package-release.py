# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

"""Create a deterministic release archive from a verified Dune install tree."""

from __future__ import annotations

import argparse
import datetime as dt
import gzip
import os
from pathlib import Path, PurePosixPath
import re
import stat
import tarfile
import zipfile


SAFE_COMPONENT = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]*$")


def validate_entry(path: Path) -> os.stat_result:
    metadata = path.lstat()
    reparse_flag = getattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0)
    file_attributes = getattr(metadata, "st_file_attributes", 0)
    if stat.S_ISLNK(metadata.st_mode) or file_attributes & reparse_flag:
        raise SystemExit(
            f"install tree contains a symbolic link or reparse point: {path}"
        )
    if not (stat.S_ISDIR(metadata.st_mode) or stat.S_ISREG(metadata.st_mode)):
        raise SystemExit(f"unsupported install-tree entry: {path}")
    return metadata


def entries(root: Path) -> list[Path]:
    found: list[Path] = []

    def visit(directory: Path) -> None:
        with os.scandir(directory) as scanned:
            children = sorted(scanned, key=lambda entry: entry.name)
        for child in children:
            path = Path(child.path)
            metadata = validate_entry(path)
            found.append(path)
            if stat.S_ISDIR(metadata.st_mode):
                visit(path)

    visit(root)
    return found


def archive_name(version: str, target: str) -> str:
    for label, value in (("version", version), ("target", target)):
        if not SAFE_COMPONENT.fullmatch(value):
            raise SystemExit(f"unsafe {label}: {value!r}")
    return f"ocaml-mutants-{version}-{target}"


def normalized_mode(path: Path, metadata: os.stat_result) -> int:
    if stat.S_ISDIR(metadata.st_mode):
        return 0o755
    return 0o755 if os.access(path, os.X_OK) else 0o644


def add_tar_entry(
    tar: tarfile.TarFile, root: Path, path: Path, prefix: str, epoch: int
) -> None:
    metadata = validate_entry(path)
    relative = path.relative_to(root).as_posix()
    name = str(PurePosixPath(prefix, relative))
    info = tar.gettarinfo(str(path), arcname=name)
    info.uid = 0
    info.gid = 0
    info.uname = "root"
    info.gname = "root"
    info.mtime = epoch
    info.mode = normalized_mode(path, metadata)
    if stat.S_ISREG(metadata.st_mode) and info.isreg():
        with path.open("rb") as source:
            tar.addfile(info, source)
    elif stat.S_ISDIR(metadata.st_mode) and info.isdir():
        tar.addfile(info)
    else:
        raise SystemExit(f"install-tree entry changed while packaging: {path}")


def create_tar_gz(
    root: Path, archive_entries: list[Path], output: Path, prefix: str, epoch: int
) -> None:
    with output.open("wb") as raw:
        with gzip.GzipFile(filename="", mode="wb", fileobj=raw, mtime=epoch) as compressed:
            with tarfile.open(fileobj=compressed, mode="w", format=tarfile.PAX_FORMAT) as tar:
                top = tarfile.TarInfo(prefix)
                top.type = tarfile.DIRTYPE
                top.mode = 0o755
                top.uid = top.gid = 0
                top.uname = top.gname = "root"
                top.mtime = epoch
                tar.addfile(top)
                for path in archive_entries:
                    add_tar_entry(tar, root, path, prefix, epoch)


def zip_timestamp(epoch: int) -> tuple[int, int, int, int, int, int]:
    earliest = int(dt.datetime(1980, 1, 1, tzinfo=dt.timezone.utc).timestamp())
    value = dt.datetime.fromtimestamp(max(epoch, earliest), tz=dt.timezone.utc)
    # ZIP timestamps have a two-second resolution.
    return (
        value.year,
        value.month,
        value.day,
        value.hour,
        value.minute,
        value.second // 2 * 2,
    )


def create_zip(
    root: Path, archive_entries: list[Path], output: Path, prefix: str, epoch: int
) -> None:
    timestamp = zip_timestamp(epoch)
    with zipfile.ZipFile(
        output, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=9
    ) as archive:
        top = zipfile.ZipInfo(prefix + "/", timestamp)
        top.create_system = 3
        top.external_attr = (stat.S_IFDIR | 0o755) << 16
        archive.writestr(top, b"")
        for path in archive_entries:
            metadata = validate_entry(path)
            relative = path.relative_to(root).as_posix()
            name = str(PurePosixPath(prefix, relative))
            if stat.S_ISDIR(metadata.st_mode):
                name += "/"
            info = zipfile.ZipInfo(name, timestamp)
            info.create_system = 3
            mode = normalized_mode(path, metadata)
            if stat.S_ISDIR(metadata.st_mode):
                info.external_attr = (stat.S_IFDIR | mode) << 16
                archive.writestr(info, b"")
            elif stat.S_ISREG(metadata.st_mode):
                info.external_attr = (stat.S_IFREG | mode) << 16
                info.compress_type = zipfile.ZIP_DEFLATED
                archive.writestr(info, path.read_bytes())
            else:
                raise SystemExit(f"unsupported install-tree entry: {path}")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--install-root", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--version", required=True)
    parser.add_argument("--target", required=True)
    parser.add_argument("--format", required=True, choices=("tar.gz", "zip"))
    args = parser.parse_args()

    validate_entry(args.install_root)
    root = args.install_root.resolve(strict=True)
    if not root.is_dir():
        raise SystemExit(f"install root is not a directory: {root}")
    archive_entries = entries(root)
    output_directory = args.output_dir.resolve(strict=False)
    if output_directory == root or root in output_directory.parents:
        raise SystemExit(
            f"output directory must be outside the install root: {output_directory}"
        )
    epoch_text = os.environ.get("SOURCE_DATE_EPOCH", "0")
    try:
        epoch = int(epoch_text)
    except ValueError as error:
        raise SystemExit(f"invalid SOURCE_DATE_EPOCH: {epoch_text!r}") from error
    if epoch < 0:
        raise SystemExit("SOURCE_DATE_EPOCH must not be negative")

    output_directory.mkdir(parents=True, exist_ok=True)
    stem = archive_name(args.version, args.target)
    output = output_directory / f"{stem}.{args.format}"
    if args.format == "tar.gz":
        create_tar_gz(root, archive_entries, output, stem, epoch)
    else:
        create_zip(root, archive_entries, output, stem, epoch)
    print(output)


if __name__ == "__main__":
    main()
