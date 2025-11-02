cmake_minimum_required(VERSION 3.10)
project(occt-wrapper VERSION 1.0 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find OpenCASCADE
find_package(OpenCASCADE REQUIRED)

if(NOT OpenCASCADE_FOUND)
    message(FATAL_ERROR "OpenCASCADE not found. Please install OCCT or set OpenCASCADE_DIR")
endif()

message(STATUS "Found OpenCASCADE ${OpenCASCADE_VERSION}")
message(STATUS "OpenCASCADE include dir: ${OpenCASCADE_INCLUDE_DIR}")
message(STATUS "OpenCASCADE library dir: ${OpenCASCADE_LIBRARY_DIR}")

# Create shared library
add_library(occt-wrapper SHARED
    occt-wrapper.cpp
    occt-wrapper.h
)

# Include directories
target_include_directories(occt-wrapper
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${OpenCASCADE_INCLUDE_DIR}
)

# Find TBB (required by OCCT)
find_package(TBB QUIET)
if(TBB_FOUND)
    message(STATUS "Found TBB: ${TBB_VERSION}")
    set(TBB_LIBS TBB::tbb TBB::tbbmalloc)
else()
    # Try to find TBB libraries manually with versioned names
    find_library(TBB_LIBRARY NAMES tbb tbb12 libtbb.so.12)
    find_library(TBBMALLOC_LIBRARY NAMES tbbmalloc tbbmalloc2 libtbbmalloc.so.2)
    if(TBB_LIBRARY)
        message(STATUS "Found TBB library: ${TBB_LIBRARY}")
        set(TBB_LIBS ${TBB_LIBRARY})
        if(TBBMALLOC_LIBRARY)
            message(STATUS "Found TBBMalloc library: ${TBBMALLOC_LIBRARY}")
            list(APPEND TBB_LIBS ${TBBMALLOC_LIBRARY})
        endif()
    else()
        message(WARNING "TBB not found - some OCCT features may not work")
        set(TBB_LIBS "")
    endif()
endif()

# Link OpenCASCADE libraries
# Core libraries
# TKernel, TKMath, TKBRep, TKPrim, TKTopAlgo, TKBO, TKG3d, TKGeomBase
# Advanced modeling
# TKFillet (fillets and chamfers)
# TKOffset (sweeps, lofts, offsets, shelling)
# Export libraries
# TKDESTEP, TKDESTL (STEP/STL export)
# TKRWMesh (mesh I/O including glTF)
# TKDEGLTF (glTF format support)
# CAF/XCAF libraries (document framework for glTF export)
# TKCDF (CAD Document Framework), TKLCAF (Lite CAF), TKXCAF (Extended CAF)
target_link_libraries(occt-wrapper
    PRIVATE
        TKernel
        TKMath
        TKBRep
        TKPrim
        TKTopAlgo
        TKBO
        TKG3d
        TKGeomBase
        TKGeomAlgo
        TKFillet
        TKOffset
        TKDESTEP
        TKDESTL
        TKMesh
        TKRWMesh
        TKDEGLTF
        TKCDF
        TKLCAF
        TKXCAF
        TKVCAF
        ${TBB_LIBS}
)

# Set library properties
set_target_properties(occt-wrapper PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    PUBLIC_HEADER occt-wrapper.h
)

# Installation rules
include(GNUInstallDirs)

install(TARGETS occt-wrapper
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

# Print build information
message(STATUS "Building occt-wrapper version ${PROJECT_VERSION}")
message(STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")
