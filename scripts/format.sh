set -euo pipefail

REPO="tweag/ormolu"
VERSION="0.8.0.2"
INSTALL_DIR=".ormolu-temp-bin"

# Parse arguments
MODE="check"
if [ "${1:-}" = "--fix" ]; then
    MODE="inplace"
fi

# Detect OS and architecture
OS="$(uname -s)"
ARCH="$(uname -m)"

case "${OS}" in
    Linux*)
        ASSET="ormolu-x86_64-linux.zip"
        ;;
    Darwin*)
        if [ "${ARCH}" = "arm64" ]; then
            ASSET="ormolu-aarch64-darwin.zip"
        else
            ASSET="ormolu-x86_64-darwin.zip"
        fi
        ;;
    CYGWIN*|MINGW*|MSYS*)
        ASSET="ormolu-x86_64-windows.zip"
        ;;
    *)
        echo "Unsupported OS: ${OS}"
        exit 1
        ;;
esac

# Create install directory if it doesn't exist
mkdir -p "${INSTALL_DIR}"

# Download and extract
echo "Downloading Ormolu ${VERSION} (${ASSET})..."
TEMP_DIR="$(mktemp -d)"
trap "rm -rf ${TEMP_DIR}" EXIT

URL="https://github.com/${REPO}/releases/download/${VERSION}/${ASSET}"
curl -fsSL "${URL}" -o "${TEMP_DIR}/ormolu.zip"

echo "Extracting..."
unzip -q "${TEMP_DIR}/ormolu.zip" -d "${TEMP_DIR}"

# Find and install the ormolu binary
find "${TEMP_DIR}" -name "ormolu" -type f -exec cp {} "${INSTALL_DIR}/ormolu" \;

# Copy any shared libraries (.dylib or .so files)
find "${TEMP_DIR}" \( -name "*.dylib" -o -name "*.so" -o -name "*.so.*" \) -type f -exec cp {} "${INSTALL_DIR}/" \;

# Make it executable
chmod +x "${INSTALL_DIR}/ormolu"

echo "✓ Ormolu ${VERSION} installed to ${INSTALL_DIR}/ormolu"
echo ""

# Format Haskell files
if [ "${MODE}" = "inplace" ]; then
    echo "Formatting Haskell files..."
else
    echo "Checking Haskell files..."
fi

find . -name "*.hs" -not -path "./.ormolu/*" -not -path "*/.stack-work/*" -exec "${INSTALL_DIR}/ormolu" --mode "${MODE}" {} \;

if [ "${MODE}" = "inplace" ]; then
    echo "✓ Formatting complete"
else
    echo "✓ Check complete"
fi
echo ""

# Clean up
echo "Cleaning up..."
rm -rf "${INSTALL_DIR}"
echo "✓ Ormolu removed"
