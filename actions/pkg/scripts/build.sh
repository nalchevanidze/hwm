#!/usr/bin/env bash
set -euo pipefail

# ---- SETUP ------------------------

# Colors 
if [[ -t 1 ]]; then
  ALERT='\033[0;31m'
  STD='\033[0m'
  INFO='\033[1;36m'
  WARN='\033[1;33m'
  SUCCESS='\033[1;32m'
else
  ALERT='' STD='' INFO='' WARN='' SUCCESS=''
fi

say() { printf "%b\n" "$*"; }

# Platform

# - OS tag
case "${RUNNER_OS:-}" in
  Windows) OS_TAG="windows" ;;
  macOS)   OS_TAG="macos" ;;
  Linux)   OS_TAG="linux" ;;
  "")
    case "$(uname -s)" in
      Darwin) OS_TAG="macos" ;;
      Linux)  OS_TAG="linux" ;;
      *)      OS_TAG="linux" ;;
    esac
    ;;
  *) OS_TAG="$(echo "${RUNNER_OS}" | tr '[:upper:]' '[:lower:]')" ;;
esac

# - Arch tag (prefer Actions envs; otherwise detect locally)
ARCH_RAW="${RUNNER_ARCH:-${PROCESSOR_ARCHITECTURE:-}}"
if [[ -z "${ARCH_RAW}" || "${ARCH_RAW}" == "unknown" ]]; then
  ARCH_RAW="$(uname -m 2>/dev/null || echo unknown)"
fi

ARCH_TAG="$(echo "$ARCH_RAW" | tr '[:upper:]' '[:lower:]')"
case "$ARCH_TAG" in
  x86_64|x64|amd64) ARCH_TAG="x64" ;;
  arm64|aarch64)    ARCH_TAG="arm64" ;;
  *)                ARCH_TAG="unknown" ;;
esac

PLATFORM_ID="${OS_TAG}-${ARCH_TAG}"
PLATFORM_SUMMARY="OS=$OS_TAG ARCH=$ARCH_TAG (raw: $ARCH_RAW)"

# Extension for executables
EXT=""
if [[ "$OS_TAG" == "windows" ]]; then
  EXT=".exe"
fi

# ----------------------------

PACKAGE=""
APP_NAME=""        # base name, e.g. "awesome-tool"; the actual binary will be "awesome-tool(.exe)"
ZIP_NAME=""          # optional override; if empty, defaults to APP_NAME (base)
STACK_BUILD_ARGS=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --package) PACKAGE="$2"; shift 2;;
    --app) APP_NAME="$2"; shift 2;;
    --zip-name) ZIP_NAME="$2"; shift 2;;
    --stack-build-args) STACK_BUILD_ARGS="$2"; shift 2;;
    *) echo "Unknown arg: $1" >&2; exit 1;;
  esac
done

if [[ -z "$PACKAGE" || -z "$APP_NAME" ]]; then
  echo "Missing required inputs: --package and --app" >&2
  exit 1
fi

# OS-specific binary filename (only this part gets .exe on Windows)
BIN_FILE="${APP_NAME}${EXT}"

OUT_DIR="out"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

# Build
# shellcheck disable=SC2086
stack build "$PACKAGE" $STACK_BUILD_ARGS

# Copy built executable
cp "$(stack exec -- which "$BIN_FILE")" "./$OUT_DIR/$BIN_FILE"

if [[ "$OS_TAG" != "windows" ]]; then
  chmod +x "./$OUT_DIR/$BIN_FILE"
fi

# Zip file: ALWAYS base app name by default
ZIP_FILE="${ZIP_NAME:-$APP_NAME}-${PLATFORM_ID}.zip"

pushd "$OUT_DIR" >/dev/null
7z a "../$ZIP_FILE" .
popd >/dev/null

rm -rf "$OUT_DIR"

echo "Produced: $ZIP_FILE"

# Outputs for composite action
{
  echo "zip_file=$ZIP_FILE"
  echo "bin_file=$BIN_FILE"
} >> "$GITHUB_OUTPUT"
