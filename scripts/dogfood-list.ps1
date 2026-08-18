# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

[CmdletBinding()]
param()

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot "..")).Path
$tempPrefix = "ocaml-mutants-dogfood-list-"
$tempDirectory = [System.IO.Directory]::CreateTempSubdirectory($tempPrefix)
$tempPath = $tempDirectory.FullName
$ownerToken = [Guid]::NewGuid().ToString("N")
$markerPath = Join-Path $tempPath ".ocaml-mutants-dogfood-list-owner"
$markerContents = "owner=ocaml-mutants-dogfood-list`ntoken=$ownerToken`n"
$utf8WithoutBom = [System.Text.UTF8Encoding]::new($false)
[System.IO.File]::WriteAllText($markerPath, $markerContents, $utf8WithoutBom)

function Remove-OwnedTempDirectory {
    if (-not (Test-Path -LiteralPath $tempPath -PathType Container)) {
        return
    }

    $resolvedTemp = [System.IO.Path]::GetFullPath($tempPath).TrimEnd('\', '/')
    $systemTemp = [System.IO.Path]::GetFullPath(
        [System.IO.Path]::GetTempPath()
    ).TrimEnd('\', '/')
    $parent = [System.IO.Directory]::GetParent($resolvedTemp)
    if ($null -eq $parent -or
        -not [String]::Equals(
            $parent.FullName.TrimEnd('\', '/'),
            $systemTemp,
            [StringComparison]::OrdinalIgnoreCase
        ) -or
        -not [System.IO.Path]::GetFileName($resolvedTemp).StartsWith(
            $tempPrefix,
            [StringComparison]::Ordinal
        )) {
        throw "refusing to remove unverified temporary directory: $resolvedTemp"
    }

    if (-not (Test-Path -LiteralPath $markerPath -PathType Leaf) -or
        -not [String]::Equals(
            [System.IO.File]::ReadAllText($markerPath),
            $markerContents,
            [StringComparison]::Ordinal
        )) {
        throw "refusing to remove temporary directory without its ownership marker: $resolvedTemp"
    }

    Remove-Item -LiteralPath $resolvedTemp -Recurse -Force
}

function Invoke-Catalog([string] $outputPath, [string] $errorPath) {
    Push-Location -LiteralPath $repoRoot
    try {
        & opam exec -- $stageZero `
            list "." --profile balanced --json --no-color `
            1> $outputPath 2> $errorPath
        $exitCode = $LASTEXITCODE
    }
    finally {
        Pop-Location
    }
    if ($exitCode -ne 0) {
        $diagnostic = [System.IO.File]::ReadAllText($errorPath)
        if (-not [String]::IsNullOrWhiteSpace($diagnostic)) {
            [Console]::Error.Write($diagnostic)
        }
        throw "stage-0 catalog generation failed with exit code $exitCode"
    }
}

try {
    & opam exec -- dune build bin/main.exe
    if ($LASTEXITCODE -ne 0) {
        throw "could not build the stage-0 CLI (exit code $LASTEXITCODE)"
    }

    $builtCli = Join-Path $repoRoot "_build/default/bin/main.exe"
    if (-not (Test-Path -LiteralPath $builtCli -PathType Leaf)) {
        throw "dune did not produce the expected stage-0 CLI: $builtCli"
    }

    $stageZero = Join-Path $tempPath "ocaml-mutants-stage0.exe"
    Copy-Item -LiteralPath $builtCli -Destination $stageZero

    $firstCatalog = Join-Path $tempPath "catalog-1.json"
    $secondCatalog = Join-Path $tempPath "catalog-2.json"
    Invoke-Catalog $firstCatalog (Join-Path $tempPath "catalog-1.stderr")
    Invoke-Catalog $secondCatalog (Join-Path $tempPath "catalog-2.stderr")

    $python = Get-Command python -ErrorAction SilentlyContinue
    if ($null -eq $python) {
        $python = Get-Command python3 -ErrorAction SilentlyContinue
    }
    if ($null -eq $python) {
        throw "Python 3 is required to canonicalize and verify catalog JSON"
    }

    & $python.Source `
        (Join-Path $repoRoot "scripts/verify-dogfood-catalog.py") `
        $firstCatalog `
        $secondCatalog
    if ($LASTEXITCODE -ne 0) {
        throw "catalog determinism verification failed (exit code $LASTEXITCODE)"
    }
}
finally {
    Remove-OwnedTempDirectory
}
