# CMakeLists.txt for compiling MYSTRAN with gfortran in a GNU environment
# based on an older CMakeLists.txt by ceanwang@gmail.com
# made to work again by Bruno Borges Paschoalinoto (2020)
# added some compatibility with Windows compiler Intel and GCC equation
# win intel with mkl   : cmake -G "NMake Makefiles JOM"  -D"CMAKE_FORTRAN_COMPILER=ifort.exe" -D"CMAKE_C_COMPILER=icx.exe" -D"CMAKE_CXX_COMPILER=icx.exe"  -D"TPL_ENABLE_BLAS=TRUE" -DBLA_VENDOR=Intel10_64lp -D"CMAKE_BUILD_TYPE=RELEASE" ..
# win gcc with openblas: cmake -G "MinGW Makefiles" -D"CMAKE_MAKE_PROGRAM:PATH=c:\gcc\bin\make.exe" -DWIN32=TRUE -D"CMAKE_Fortran_COMPILER:PATH=c:\gcc\bin\gfortran.exe" -D"TPL_ENABLE_BLAS=TRUE" -D"TPL_BLAS_LIBRARIES:PATH=c:\gcc\openblas\libopenblas.DLL" -D"BLAS_LIBRARIES:PATH=c:\gcc\openblas\libopenblas.DLL" -D"CMAKE_BUILD_TYPE=RELEASE" -DBLA_VENDOR=OpenBlas  ..
# linux gcc with openblas: cmake -G "Unix Makefiles" -DBLA_VENDOR=OpenBLAS  ..  


# set up basic project info
cmake_minimum_required(VERSION 3.18)
include(CheckFunctionExists)

project(Mystran)
enable_language(Fortran)


# basic compiler and output options
set(CMAKE_SOURCE_DIR "${PROJECT_SOURCE_DIR}/Stage3")
set(PROJECT_BINARY_DIR "${PROJECT_SOURCE_DIR}/Binaries")
set(CMAKE_FLAGS "-c -fbacktrace")
# set(CMAKE_BUILD_TYPE Debug)

# set some dirs
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod")
include_directories("${CMAKE_SOURCE_DIR}/INCLUDE")

# uncomment this to debug
# set(CMAKE_VERBOSE_MAKEFILE true)

# suppress cmake warnings for superlu
if(NOT DEFINED CMAKE_SUPPRESS_DEVELOPER_WARNINGS)
  set(CMAKE_SUPPRESS_DEVELOPER_WARNINGS 1 CACHE INTERNAL "No dev warnings")
endif()

# recommend the appropriate make command
if(WIN32)
  if(WIN32 AND ((CMAKE_C_COMPILER_ID STREQUAL "IntelLLVM") OR (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel") OR (CMAKE_C_COMPILER_ID STREQUAL "Intel")))
   set(RECOMMENDED_MAKE "nmake")
  else() 
   #mustbe gfortran gcc
   set(RECOMMENDED_MAKE "mingw32-make")
  endif()
else()
  set(RECOMMENDED_MAKE "make")
endif()

#correct cmake linking with windows intel compiler to use lib instead icx 
#set to call c from fortran with upcase incase intel

if(WIN32 AND ((CMAKE_C_COMPILER_ID STREQUAL "IntelLLVM") OR (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel") OR (CMAKE_C_COMPILER_ID STREQUAL "Intel")))
    # Force CMake to use lib.exe for creating static libraries on Windows with IntelLLVM
    set(CMAKE_AR "lib.exe")
    set(CMAKE_CXX_CREATE_STATIC_LIBRARY "<CMAKE_AR> /OUT:<TARGET> <OBJECTS>")
    set(CMAKE_C_CREATE_STATIC_LIBRARY "<CMAKE_AR> /OUT:<TARGET> <OBJECTS>")

    # Define a preprocessor macro to indicate that upcase is required for intel fortran
    add_definitions(-DUpCase)

    # Set release flags for Intel compiler dndebug problem and stack overflow 

    if(CMAKE_BUILD_TYPE STREQUAL "RELEASE")
    set(CMAKE_C_FLAGS_RELEASE "/Ox /arch:COFFEELAKE /tune:coffeelake /Qm64 /F200000000")
    set(CMAKE_CXX_FLAGS_RELEASE "/Ox /QaxCOFFEELAKE /arch:COFFEELAKE /tune:coffeelake /Qm64 /Qpar /heap-arrays:0 /F200000000")
    set(CMAKE_Fortran_FLAGS_RELEASE "/Ox /QaxCOFFEELAKE /arch:COFFEELAKE /tune:coffeelake /Qm64 /Qpar /heap-arrays:0 /F200000000")
    else()
    set(CMAKE_C_FLAGS_DEBUG "  /Qm64  /heap-arrays:0 /F200000000")
    set(CMAKE_CXX_FLAGS_DEBUG " /Qm64  /heap-arrays:0 /F200000000")
    set(CMAKE_Fortran_FLAGS_DEBUG " /Qm64 /heap-arrays:0 /F200000000")
    endif(CMAKE_BUILD_TYPE STREQUAL "RELEASE")
endif()


#check blas vendor if given
if(DEFINED BLA_VENDOR)
  set(USE_EXTERNAL_BLAS ON)
else()
  set(USE_EXTERNAL_BLAS OFF)
  message (STATUS "BLA_VENDOR not stated, use internal BLAS" )
  set(enable_internal_blaslib ON)  # Enable this to use the internal BLAS library
endif()

if (enable_internal_blaslib)
  set(USE_EXTERNAL_BLAS OFF)
  message(STATUS "Internal BLAS is enabled.")
else()
  set(USE_EXTERNAL_BLAS ON)
  message(STATUS "External BLAS will be used.")
endif()


option(TPL_ENABLE_INTERNAL_BLASLIB "Build the CBLAS library" ${enable_internal_blaslib})
option(TPL_BLAS_LIBRARIES "List of absolute paths to blas libraries [].")

# try to find BLAS
#--------------------- BLAS ---------------------
if(NOT enable_internal_blaslib)


  if (TPL_BLAS_LIBRARIES)
    set(BLAS_FOUND TRUE)
    message(STATUS "Using user-specified BLAS libraries: ${TPL_BLAS_LIBRARIES}")
  else()
    find_package(BLAS REQUIRED)
    if(BLAS_FOUND)
      message(STATUS "Found BLAS: ${BLAS_LIBRARIES}")
    else()
      message(FATAL_ERROR "BLAS not found! Please install a BLAS library or use the internal BLAS.")
    endif()
    
    if(${BLA_VENDOR} MATCHES "OpenBLAS")
    
    
      set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
        if(UNIX AND NOT APPLE)
        # in Linux, find cmake OpenBLAS is missing  so manually set the path to OpenBLAS
        set(TPL_BLAS_LIBRARIES "/usr/lib/x86_64-linux-gnu/libopenblas.so")
        set(BLAS_FOUND TRUE)
        message(STATUS "Manually set BLAS library: ${TPL_BLAS_LIBRARIES}")
        else()
        find_package(OpenBLAS REQUIRED)
        endif()
    elseif(${BLA_VENDOR} MATCHES "Intel10_64lp" OR ${BLA_VENDOR} MATCHES "Intel")
      find_package(LAPACK REQUIRED)
    endif()

    if (BLAS_FOUND)
      set(TPL_BLAS_LIBRARIES "${BLAS_LIBRARIES}" CACHE FILEPATH
          "Set from FindBLAS.cmake BLAS_LIBRARIES." FORCE)
    else()
      set(enable_internal_blaslib TRUE)
      set(USE_EXTERNAL_BLAS OFF)
    endif()
  endif()
endif()

if(BLAS_FOUND)
  message(STATUS "Using TPL_BLAS_LIBRARIES='${TPL_BLAS_LIBRARIES}'")
  set(CMAKE_C_FLAGS "-DUSE_VENDOR_BLAS ${CMAKE_C_FLAGS}")
  set(BLAS_LIB ${TPL_BLAS_LIBRARIES})
  # fix up BLAS library name
  string(REPLACE ";" " " BLAS_LIB_STR "${BLAS_LIB}")
  set(BLAS_LIB_EXPORT ${BLAS_LIB_STR})
else()
  message(STATUS "Did not find or specify BLAS so superlu cmake will build internal CBLAS ...")
  set(USE_EXTERNAL_BLAS OFF)
endif()

message(STATUS "Disabled support for METIS.")
set(TPL_ENABLE_METISLIB FALSE)  
set(METIS_FOUND FALSE)


# submodules (i.e. SuperLU)
find_package(Git QUIET)
if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
  option(GIT_SUBMODULE "Check submodules during build" ON)
  if(GIT_SUBMODULE)
    message(STATUS "Updating submodules")
    execute_process(
      COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive --force
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      RESULT_VARIABLE GIT_SUBMOD_RESULT
    )
    if(NOT GIT_SUBMOD_RESULT EQUAL "0")
      message(FATAL_ERROR "git submodule update --init --recursive --force failed")
    endif()
  endif()
endif()

# check for SuperLU
set(SUPERLU_DIR "${PROJECT_SOURCE_DIR}/superlu")
include_directories("${SUPERLU_DIR}/SRC")
if(NOT EXISTS "${SUPERLU_DIR}/CMakeLists.txt")
  message(FATAL_ERROR "The submodules were not downloaded. You have to do it manually!")
endif()


# f2c stuff needed if you compile cblas into superlu
if (enable_internal_blaslib)
set(F2C_DIR "${PROJECT_SOURCE_DIR}/f2c")
set(F2C_INCLUDE_DIR "${F2C_DIR}/include")
set(F2C_FN "${F2C_DIR}/libf2c.zip")
set(F2C_URL "https://www.netlib.org/f2c/libf2c.zip")

# download f2c
if(NOT EXISTS ${F2C_DIR})
  message(STATUS "Downloading libf2c source from ${F2C_URL}...")
  make_directory("${F2C_DIR}")
  file(DOWNLOAD ${F2C_URL} ${F2C_FN} TIMEOUT 60 STATUS DOWNLOAD_STATUS)
  # Check if download was successful.
  list(GET DOWNLOAD_STATUS 0 STATUS_CODE)
  list(GET DOWNLOAD_STATUS 1 ERROR_MESSAGE)
  if(${STATUS_CODE} EQUAL 0)
    message(STATUS "Done downloading libf2c.")
  else()
    # Exit CMake if the download failed, printing the error message.
    file(REMOVE_RECURSE ${F2C_DIR})
    message(FATAL_ERROR "Error downloading libf2c: ${ERROR_MESSAGE}")
  endif()
endif()

# extract libf2c source
file(ARCHIVE_EXTRACT INPUT ${F2C_FN} DESTINATION ${F2C_DIR})

# prepare libf2c header files
file(GLOB_RECURSE F2C_PREHEADERS "${F2C_DIR}/*.h0")
foreach(H0 ${F2C_PREHEADERS})
  string(REGEX REPLACE "[.]h0$" ".h" H0_R ${H0})
  file(RENAME "${H0}" "${H0_R}")
  file(COPY "${H0_R}" DESTINATION "${F2C_INCLUDE_DIR}")
endforeach()

# get a load of this: f2c generates its own "arith.h" on the fly
# so we gotta compile arithchk and run it
set(F2C_ARITHCHK_SRC "${F2C_DIR}/arithchk.c")
set(F2C_ARITHCHK_BIN "${F2C_DIR}/arithchk")

if (WIN32)
  set(F2C_ARITHCHK_BIN "${F2C_ARITHCHK_BIN}.exe")
endif()

set(F2C_ARITH_H "${F2C_INCLUDE_DIR}/arith.h")
set_source_files_properties(
  ${F2C_ARITHCHK_SRC} PROPERTIES COMPILE_FLAGS "-DNO_LONG_LONG -DNO_FPINIT"
)
add_executable(arithchk ${F2C_ARITHCHK_SRC})
target_link_libraries(arithchk m)
set_target_properties(
  arithchk PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${F2C_DIR}
)
add_custom_command(
  OUTPUT ${F2C_ARITH_H}
  COMMAND ${F2C_ARITHCHK_BIN} > ${F2C_ARITH_H}
  DEPENDS ${F2C_ARITHCHK_BIN}
)

# add libf2c to the compilation procedures
include_directories(${F2C_INCLUDE_DIR})
file(GLOB_RECURSE F2C_CFILES "${F2C_DIR}/*.c")
add_definitions(-DINTEGER_STAR_8)
add_library(f2c ${F2C_CFILES} ${F2C_ARITH_H})

# add some extra win32 flags for libf2c
if (WIN32)
  add_definitions(-DUSE_CLOCK -DMSDOS)
endif()

endif (enable_internal_blaslib)

# set some extra vars for MSYS builds to make the binary portable
if (WIN32)
  set(CMAKE_EXE_LINKER_FLAGS "-static")
endif()

# build the SuperLU (and CBLAS if needed) static libs!
message(STATUS "Set examples superlu and testing is disabled")
# Set options for the SuperLU subproject
set(enable_tests OFF CACHE BOOL "Build tests")
set(enable_matlabmex OFF CACHE BOOL "Build the Matlab mex library")
set(enable_examples OFF CACHE BOOL "Build examples")

message(STATUS "Set superlu is built as static library")
set(BUILD_SHARED_LIBS OFF)

message(STATUS "Set superlu is only need double superlu library for mystran")
# Set precision and complex number options for SuperLU
set(enable_double ON CACHE BOOL "Enable double precision library")
set(enable_complex16 OFF CACHE BOOL "Enable double precision complex (complex16) library")
set(enable_single OFF CACHE BOOL "Enable single precision library")
set(enable_complex OFF CACHE BOOL "Enable single precision complex library")

add_subdirectory(${SUPERLU_DIR})

# collect modules and interfaces into a module called MODULES
# the overuse of the word MODULE is brain-twisting, but bear with me
set(Modules_DIR "${CMAKE_SOURCE_DIR}/Modules")
file(GLOB_RECURSE Modules_ARPACK_FILES "${Modules_DIR}/ARPACK/*.f*")
file(GLOB_RECURSE Modules_LAPACK_FILES "${Modules_DIR}/LAPACK/*.f*")
file(GLOB_RECURSE Modules_BANDIT_FILES "${Modules_DIR}/BANDIT/*.f*")
file(GLOB_RECURSE Modules_FILES "${Modules_DIR}/*.f*")
file(GLOB_RECURSE Interfaces_FILES "${CMAKE_SOURCE_DIR}/Interfaces/*.f*")
file(GLOB_RECURSE USE_IFs_FILES "${CMAKE_SOURCE_DIR}/USE_IFs/*.f*")

# MODULES_ALL_FILES shall contain all the sources for the base modules lib
set(MODULES_ALL_FILES 
  ${USE_IFs_FILES} ${Interfaces_FILES} ${Modules_FILES}
  ${Modules_LAPACK_FILES} ${Modules_ARPACK_FILES}
)
# Remove specific file if BLAS_FOUND is true
if (BLAS_FOUND)
  message(STATUS "BLAS Unresolved_Externals_Problem removed")
  list(REMOVE_ITEM MODULES_ALL_FILES "${Modules_DIR}/LAPACK/Unresolved_Externals_Problem.f")
  file(GLOB_RECURSE Unresolved_Externals_FILES "${Modules_DIR}/LAPACK/Unresolved_Externals_Problem/*.f*")
  list(REMOVE_ITEM MODULES_ALL_FILES ${Unresolved_Externals_FILES})
endif(BLAS_FOUND)
# Built modules
add_library(MODULES OBJECT ${MODULES_ALL_FILES})

# collect modules
list(APPEND modules_names ARPACK BANDIT LK1 LK2 LK3 LK4 LK5 LK6 LK9 EMG)
foreach (modname IN LISTS modules_names)
  file(GLOB_RECURSE TMP_MOD_FILES "${CMAKE_SOURCE_DIR}/${modname}/*.f*")
  add_library(${modname} OBJECT ${TMP_MOD_FILES} ${TMP_MOD_FILES_PP})
  target_link_libraries(${modname} MODULES)
endforeach()

# add the DGSSV C module from SuperLU and link it against SuperLU's BLAS
add_library(dgssv "${SUPERLU_DIR}/FORTRAN/c_fortran_dgssv.c")

if (NOT BLAS_FOUND)
  target_link_libraries(dgssv blas)
else()
  target_link_libraries(dgssv)
endif()

# if (and only if) SuperLU is compiled with its internal CBLAS, some subrs will
# be missing, so we compile our own.
# first, we check for the missing subroutines
if (NOT BLAS_FOUND)
  list(
    APPEND blas_fns dgemm dgemv dlamch dlanst dscal dsteqr dsterf dswap dtrsm
    dtrtri ilaenv lsame xerbla
  )
  foreach (fname IN LISTS blas_fns)
    check_function_exists(${fname} BLAS_FN_EXISTS)
    if (NOT BLAS_FN_EXISTS)
      string(TOUPPER ${fname} fname_upper)
      list(APPEND missing_blas_src "${CMAKE_SOURCE_DIR}/BLAS/${fname_upper}.f")
      list(APPEND missing_blas_fns ${fname})
    endif()
  endforeach()
  # if any subroutines have been found, create an inner blas library
  list(LENGTH missing_blas_fns MISSING_FNS_TOTAL)
  if (MISSING_FNS_TOTAL GREATER 0)
    if (MISSING_FNS_TOTAL GREATER 1)
      message(
        STATUS
        "BLAS subrs (${missing_blas_fns}) are absent and will be built locally."
      )
    else()
      message(
        STATUS
        "BLAS subr ${missing_blas_fns} is absent and will be built locally."
      )
    endif()
    add_library(my_blas OBJECT ${missing_blas_src})
    target_link_libraries(my_blas MODULES)
  endif()
endif()

# prepare the main executable, linked against the specifics and the m
# it appears utils used to be a module, but that is no longer the case?
file(GLOB_RECURSE UTIL_FILES "${CMAKE_SOURCE_DIR}/UTIL/*.f*")
file(GLOB_RECURSE MAIN_FILES "${CMAKE_SOURCE_DIR}/MAIN/*.[fF]*")
add_executable(mystran ${MAIN_FILES} ${MODULES_ALL_FILES} ${UTIL_FILES})

if (NOT BLAS_FOUND)
  target_link_libraries(mystran ${modules_names})
  target_link_libraries(mystran dgssv superlu f2c blas)
  message(STATUS "the configuration: target_link_libraries mystran dgssv superlu f2c blas")
  if (MISSING_FNS_TOTAL GREATER 0)
    target_link_libraries(mystran my_blas)
    message(STATUS "the configuration: target_link_libraries mystran dgssv superlu f2c blas myblas")
  endif()
else()
  target_link_libraries(mystran ${modules_names})
  if(TPL_ENABLE_BLAS)
    message(STATUS "the configuration: target_link_libraries mystran dgssv superlu BLAS_LIBRARIES")
    target_link_libraries(mystran dgssv superlu ${BLAS_LIBRARIES})
  else(TPL_ENABLE_BLAS)
    message(STATUS "the configuration: target_link_libraries mystran dgssv superlu blas")
    target_link_libraries(mystran dgssv superlu blas)
  endif(TPL_ENABLE_BLAS)
endif()

set_target_properties(
  mystran PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}
)
if (CMAKE_COMPILER_IS_GNUCC)
  target_compile_options(
    mystran PRIVATE
    -Wall -Wextra
    -Wno-unused-variable -Wno-unused-label -Wno-unused-parameter -Wno-tabs
    -Wno-compare-reals -Wno-character-truncation -Wno-unused-dummy-argument
    -Wmaybe-uninitialized -Wrealloc-lhs -fcheck=all
  )
endif()

# issue a couple messages about compilation
include(ProcessorCount)
ProcessorCount(NCPU)
message(STATUS "You can now compile MYSTRAN with ${RECOMMENDED_MAKE}.")
if (NOT NCPU EQUAL 0)
  if (NOT (WIN32 AND ((CMAKE_C_COMPILER_ID STREQUAL "IntelLLVM") OR (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel") OR (CMAKE_C_COMPILER_ID STREQUAL "Intel"))))
  message(STATUS "Compile faster by passing -j${NCPU} to ${RECOMMENDED_MAKE}.")
  else()
  message(STATUS "Compile faster by passing /j${NCPU} to JOM. but must using NMake Makefiles JOM")
  endif()
endif()
