cmake_minimum_required (VERSION 3.13)

# Project Version
project(SuperLU_MT C)
set(VERSION_MAJOR "4")
set(VERSION_MINOR "0")
set(VERSION_BugFix "0")
set(PROJECT_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BugFix})

# Compatiblitly to xSDK standard
# (Extreme-scale Scientific Software Development Kit)
set(USE_XSDK_DEFAULTS TRUE)

# The XSDK standard does not allow using internally built BLAS
if (NOT "${enable_internal_blaslib}" STREQUAL "")
  if (USE_XSDK_DEFAULTS)
    set(enable_blaslib_xSDK OFF)
  else()
    set(enable_blaslib_xSDK ON)
  endif()
else()
  set(enable_blaslib_xSDK ${enable_internal_blaslib})
endif()

if (NOT "${enable_fortran}" STREQUAL "")
  if (XSDK_ENABLE_Fortran)
    set(enable_fortran_xSDK ON)
  else()
    set(enable_fortran_xSDK OFF)
  endif()
else()
  set(enable_fortran_xSDK ${enable_fortran})
endif()

set (CMAKE_BUILD_TYPE Release CACHE STRING "Build type")

# set up options
option(enable_internal_blaslib  "Build the CBLAS library" ${enable_blaslib_xSDK})
option(enable_single    "Enable single precision library" ON)
option(enable_double    "Enable double precision library" ON)
option(enable_complex   "Enable complex precision library" ON)
option(enable_complex16 "Enable complex16 precision library" ON)
option(enable_matlabmex "Build the Matlab mex library" OFF)
option(enable_doc       "Add target 'doc' to build Doxygen documentation" OFF)
option(enable_examples  "Build examples" ON)
option(enable_fortran   "Build Fortran interface" ${enable_fortran_xSDK})
option(enable_tests     "Build tests" ON)
option (BUILD_SHARED_LIBS "shared/static" OFF)

include (GNUInstallDirs)
include (CheckLibraryExists)
check_library_exists(m sin "" HAVE_LIB_M)

######################################################################
#
# Find packages
#
######################################################################
#
###find_package (BLAS REQUIRED)
#-- BLAS
option(TPL_ENABLE_INTERNAL_BLASLIB  "Build the CBLAS library" ${enable_internal_blaslib})
option(TPL_BLAS_LIBRARIES "List of absolute paths to blas libraries [].")

if(NOT enable_internal_blaslib)
  if (TPL_BLAS_LIBRARIES)
    set(BLAS_FOUND TRUE)
  else()
    find_package(BLAS)
    if (BLAS_FOUND)
      set(TPL_BLAS_LIBRARIES "${BLAS_LIBRARIES}" CACHE FILEPATH
        "Set from FindBLAS.cmake BLAS_LIBRARIES." FORCE)
    endif()
  endif()
endif()

if(BLAS_FOUND)
  message("-- Using TPL_BLAS_LIBRARIES='${TPL_BLAS_LIBRARIES}'")
  set(CMAKE_C_FLAGS "-DUSE_VENDOR_BLAS ${CMAKE_C_FLAGS}")
  set(BLAS_LIB ${TPL_BLAS_LIBRARIES})
  # fix up BLAS library name
  string (REPLACE ";" " " BLAS_LIB_STR "${BLAS_LIB}")
  set(BLAS_LIB_EXPORT ${BLAS_LIB_STR})
else()
  message("-- Did not find or specify BLAS so configure to build internal CBLAS ...")
  add_subdirectory(CBLAS)
  set(BLAS_LIB blas)
  if (BUILD_SHARED_LIBS)  # export to be referenced by downstream makefile
      set(BLAS_LIB_EXPORT ${CMAKE_INSTALL_PREFIX}/CBLAS/libblas.so)
  else()
      set(BLAS_LIB_EXPORT ${CMAKE_INSTALL_PREFIX}/CBLAS/libblas.a)
  endif()
endif()


set (PLAT "_OPENMP" CACHE STRING "threading flavor _PTHREAD/_OPENMP")
if (PLAT STREQUAL "_PTHREAD")
  find_package (Threads REQUIRED)
elseif (PLAT STREQUAL "_OPENMP")
  find_package (OpenMP REQUIRED)
else ()
  message (SEND_ERROR "invalid PLAT setting")
endif ()

option (LONGINT "use 64-bit integers for indexing sparse matrices (default is 32-bit)" OFF)

enable_language(C)
if (enable_fortran)
  enable_language(Fortran)
endif()
set(SUPERLU_MT_VERSION "${PROJECT_VERSION}")


set (SUPERLUMT_INSTALL_INCLUDEDIR ${CMAKE_INSTALL_INCLUDEDIR}/superlu_mt CACHE STRING "include dir")

add_subdirectory(SRC)
add_subdirectory(EXAMPLE)

if(enable_tests)
  enable_testing()
  add_subdirectory(TESTING)
endif()

if (enable_doc)
   add_subdirectory(DOC)
endif()
