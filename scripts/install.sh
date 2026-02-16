#!/usr/bin/env bash
set -euo pipefail


DEFAULT_VERSION="v0.0.3" # update this on release
VERSION="${1:-$DEFAULT_VERSION}"

curl -fsSL "https://raw.githubusercontent.com/nalchevanidze/hwm/main/actions/cli/scripts/install.sh" \
  | bash -s -- --version "$VERSION"
