# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

[CmdletBinding()]
param()

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot "..")).Path
$tempPrefix = "ocaml-mutants-dogfood-"
$tempDirectory = [System.IO.Directory]::CreateTempSubdirectory($tempPrefix)
$tempPath = $tempDirectory.FullName
$ownerToken = [Guid]::NewGuid().ToString("N")
$markerPath = Join-Path $tempPath ".ocaml-mutants-dogfood-owner"
$markerContents = "owner=ocaml-mutants-dogfood`ntoken=$ownerToken`n"
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

$python = Get-Command python -ErrorAction SilentlyContinue
if ($null -eq $python) {
    $python = Get-Command python3 -ErrorAction SilentlyContinue
}

$failures = [System.Collections.Generic.List[string]]::new()
$manifestCreated = $false
$manifestPath = Join-Path $tempPath "workspace-manifest.json"

try {
    if ($null -eq $python) {
        throw "Python 3 is required for dogfood report and workspace verification"
    }

    Push-Location -LiteralPath $repoRoot
    try {
        & opam exec -- dune build bin/main.exe
        if ($LASTEXITCODE -ne 0) {
            throw "could not build the stage-0 CLI (exit code $LASTEXITCODE)"
        }
    }
    finally {
        Pop-Location
    }

    $builtCli = Join-Path $repoRoot "_build/default/bin/main.exe"
    if (-not (Test-Path -LiteralPath $builtCli -PathType Leaf)) {
        throw "dune did not produce the expected stage-0 CLI: $builtCli"
    }
    $stageZero = Join-Path $tempPath "ocaml-mutants-stage0.exe"
    Copy-Item -LiteralPath $builtCli -Destination $stageZero

    & $python.Source `
        (Join-Path $repoRoot "scripts/verify-workspace-manifest.py") `
        create $repoRoot $manifestPath `
        --exclude-root _build `
        --exclude-root _opam
    if ($LASTEXITCODE -ne 0) {
        throw "could not create the pre-run workspace manifest"
    }
    $manifestCreated = $true

    $reportPath = Join-Path $tempPath "run-report.json"
    $errorPath = Join-Path $tempPath "run.stderr"
    $runExitCode = $null
    Push-Location -LiteralPath $repoRoot
    try {
        & opam exec -- $stageZero `
            run "." --profile balanced --cache-mode on --json --no-color `
            1> $reportPath 2> $errorPath
        $runExitCode = $LASTEXITCODE
    }
    finally {
        Pop-Location
    }

    if (Test-Path -LiteralPath $errorPath -PathType Leaf) {
        $diagnostic = [System.IO.File]::ReadAllText($errorPath)
        if (-not [String]::IsNullOrWhiteSpace($diagnostic)) {
            [Console]::Error.Write($diagnostic)
        }
    }
    if ($null -eq $runExitCode) {
        throw "stage-0 dogfood process did not return an exit code"
    }
    & $python.Source `
        (Join-Path $repoRoot "scripts/verify-dogfood-run.py") `
        $reportPath `
        --schema (Join-Path $repoRoot "schema/run-report-v1.schema.json") `
        --exit-code $runExitCode
    if ($LASTEXITCODE -ne 0) {
        throw "Balanced dogfood report failed acceptance"
    }
}
catch {
    $failures.Add("execution: $($_.Exception.Message)")
}

if ($manifestCreated) {
    try {
        & $python.Source `
            (Join-Path $repoRoot "scripts/verify-workspace-manifest.py") `
            verify $repoRoot $manifestPath
        if ($LASTEXITCODE -ne 0) {
            throw "source workspace differs from its pre-run manifest"
        }
    }
    catch {
        $failures.Add("workspace verification: $($_.Exception.Message)")
    }
}

try {
    Remove-OwnedTempDirectory
}
catch {
    $failures.Add("temporary cleanup: $($_.Exception.Message)")
}

if ($failures.Count -ne 0) {
    foreach ($failure in $failures) {
        [Console]::Error.WriteLine("dogfood: $failure")
    }
    exit 1
}
