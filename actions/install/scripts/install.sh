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

usage() {
  cat >&2 <<'EOF'
Usage:
  install.sh --repo <owner/repo> --app <name> --version <tag> [--bin-dir <dir>]

Notes:
  - Downloads: https://github.com/<repo>/releases/download/<tag>/<app>-<os>-<arch>.zip
  - If tag doesn't work, it automatically retries with/without leading 'v'.
EOF
}

REPO=""
APP_NAME=""
VERSION=""
BIN_DIR=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo)    REPO="${2:-}"; shift 2;;
    --app)     APP_NAME="${2:-}"; shift 2;;
    --version) VERSION="${2:-}"; shift 2;;
    --bin-dir) BIN_DIR="${2:-}"; shift 2;;
    -h|--help) usage; exit 0;;
    *) echo "Unknown arg: $1" >&2; usage; exit 2;;
  esac
done

if [[ -z "$REPO" || -z "$APP_NAME" || -z "$VERSION" ]]; then
  echo "ERROR: --repo, --app, and --version are required." >&2
  usage
  exit 2
fi

# Binary name inside zip
BIN_FILE="$APP_NAME${EXT}"

# Default install dir if not provided
if [[ -z "$BIN_DIR" ]]; then
  if [[ -d "$HOME/bin" ]]; then
    BIN_DIR="$HOME/bin"
  else
    BIN_DIR="$HOME/.local/bin"
  fi
fi
mkdir -p "$BIN_DIR"

# Temp workdir + cleanup
WORKDIR="$(mktemp -d 2>/dev/null || (rm -rf .pkg-local && mkdir -p .pkg-local && echo ".pkg-local"))"
cleanup() { rm -rf "$WORKDIR" 2>/dev/null || true; }
trap cleanup EXIT

ASSET="${APP_NAME}-${PLATFORM_ID}.zip"

url_for_tag() {
  local tag="$1"
  echo "https://github.com/$REPO/releases/download/$tag/$ASSET"
}

download() {
  local url="$1"
  curl -fSLsS "$url" -o "$WORKDIR/$APP_NAME.zip"
}

say "\n${INFO}Installing ${APP_NAME} (tag: ${VERSION})${STD}"
say " - repo: $REPO"
say " - detected: $PLATFORM_SUMMARY"
say " - asset: $ASSET"

URL="$(url_for_tag "$VERSION")"
say " - source: $URL"

# Download (auto fallback v-prefix / non-v prefix)
if ! download "$URL"; then
  if [[ "$VERSION" == v* ]]; then
    ALT="${VERSION#v}"
    URL="$(url_for_tag "$ALT")"
    say "${WARN}First download failed; retrying with tag $ALT${STD}"
    say " - source: $URL"
    download "$URL"
  else
    ALT="v$VERSION"
    URL="$(url_for_tag "$ALT")"
    say "${WARN}First download failed; retrying with tag $ALT${STD}"
    say " - source: $URL"
    download "$URL"
  fi
fi

say " - extracting"
cd "$WORKDIR"

if command -v unzip >/dev/null 2>&1; then
  unzip -q "$APP_NAME.zip"
elif command -v 7z >/dev/null 2>&1; then
  7z x "$APP_NAME.zip" -y >/dev/null
else
  say "${ALERT}Neither 'unzip' nor '7z' found. Please install one of them.${STD}" >&2
  exit 1
fi

if [[ ! -f "./$BIN_FILE" ]]; then
  say "${ALERT}Expected binary ./$BIN_FILE not found after extraction.${STD}" >&2
  say "Contents:" >&2
  ls -la >&2 || true
  exit 1
fi

# Ensure executable bit on non-Windows
if [[ "$OS_TAG" != "windows" ]]; then
  chmod 755 "./$BIN_FILE"
fi

say " - copying binary to $BIN_DIR"
if command -v install >/dev/null 2>&1; then
  install -m 755 "./$BIN_FILE" "$BIN_DIR/$BIN_FILE"
else
  cp "./$BIN_FILE" "$BIN_DIR/$BIN_FILE"
  [[ "$OS_TAG" != "windows" ]] && chmod 755 "$BIN_DIR/$BIN_FILE" || true
fi

say ""
if [[ "$OS_TAG" == "windows" ]]; then
  # In bash on Windows, `command -v app` won't find app.exe unless PATHEXT is honored;
  # just check the file exists.
  if [[ -f "$BIN_DIR/$BIN_FILE" ]]; then
    say "${SUCCESS}Installation succeeded: $BIN_DIR/$BIN_FILE${STD}"
  fi
else
  if command -v "$APP_NAME" >/dev/null 2>&1; then
    say "${SUCCESS}Installation succeeded: $("$APP_NAME" --version 2>/dev/null || "$APP_NAME" about 2>/dev/null || echo "$APP_NAME installed")${STD}"
  else
    say "${WARN}Installed to ${BIN_DIR}${STD}"
    say "Add it to PATH to run '$APP_NAME'."
  fi
fi
say ""
