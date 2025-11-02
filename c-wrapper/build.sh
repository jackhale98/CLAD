#!/bin/bash
# build.sh - Build the OCCT C wrapper library

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Building OCCT C Wrapper Library${NC}"
echo "================================"

# Create build directory
mkdir -p build
cd build

# Run CMake
echo -e "\n${YELLOW}Running CMake...${NC}"
cmake .. -DCMAKE_BUILD_TYPE=Release

# Build
echo -e "\n${YELLOW}Building library...${NC}"
cmake --build . -j$(nproc)

echo -e "\n${GREEN}Build complete!${NC}"
echo "Library location: $(pwd)/libocct-wrapper.so"
echo ""
echo "To install system-wide, run:"
echo "  sudo cmake --build . --target install"
