# CMakeLists.txt for compiling MYSTRAN with gfortran in a GNU environment
# based on an older CMakeLists.txt by ceanwang@gmail.com
# made to work again by Bruno Borges Paschoalinoto (2020)

# set up basic project info
cmake_minimum_required(VERSION 3.18)
include(CheckFunctionExists)

project(Mystran)
enable_language(Fortran)

# basic compiler and output options
set(CMAKE_SOURCE_DIR "${PROJECT_SOURCE_DIR}/Source")
set(BLAS_SOURCE_DIR "${PROJECT_SOURCE_DIR}/BLAS")
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
  set(RECOMMENDED_MAKE "mingw32-make")
else()
  set(RECOMMENDED_MAKE "make")
endif()

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
      message(
        FATAL_ERROR
        "git submodule update --init --recursive --force failed"
      )
    endif()
  endif()
endif()

# check for SuperLU
set(SUPERLU_DIR "${PROJECT_SOURCE_DIR}/superlu")
if(NOT EXISTS "${SUPERLU_DIR}/CMakeLists.txt")
  message(
    FATAL_ERROR
    "The submodules were not downloaded. You have to do it manually!"
  )
endif()

# set some SuperLU settings
set(enable_examples OFF)
set(enable_tests OFF)
#set(enable_internal_blaslib "yes")


# include & build it
include_directories("${SUPERLU_DIR}/SRC")
add_subdirectory(${SUPERLU_DIR})


# f2c stuff
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


# set some extra vars for MSYS builds to make the binary portable
if (WIN32)
  set(CMAKE_EXE_LINKER_FLAGS "-static")
endif()

# collect all fortran source files
file(GLOB_RECURSE ALL_FORTRAN_FILES "${CMAKE_SOURCE_DIR}/*.f*")

# collect superlu dgssv
add_library(dgssv "${SUPERLU_DIR}/FORTRAN/c_fortran_dgssv.c")

# same BLAS-finding subroutine as SuperLU
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

if (NOT BLAS_FOUND)
  message(STATUS "BLAS not found, building local BLAS")
  list(
    APPEND blas_fns dgemm dgemv dlamch dlanst dscal dsteqr dsterf dswap dtrsm
    dtrtri ilaenv lsame xerbla
  )
  foreach (fname IN LISTS blas_fns)
    #message(STATUS "Checking for BLAS subr ${fname}")
    check_function_exists("${fname}" BLAS_FN_EXISTS)
    set(BLAS_FN_EXISTS CACHE "1" STRING)
    if (NOT BLAS_FN_EXISTS)
      #message(STATUS "BLAS subr ${fname} not found")
      string(TOUPPER ${fname} fname_upper)
      list(APPEND missing_blas_src "${BLAS_SOURCE_DIR}/${fname_upper}.f")
      list(APPEND missing_blas_fns ${fname})
    endif()
    unset(BLAS_FN_EXISTS CACHE)
  endforeach()
  # if any subroutines have bene found, create an inner blas library
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
    target_link_libraries(dgssv my_blas)
  endif()
else()
  target_link_libraries(dgssv blas)
  message(STATUS "Using system BLAS (${TPL_BLAS_LIBRARIES})")
endif()


# prepare the main executable, linked against the specifics and the m
# it appears utils used to be a module, but that is no longer the case?
# file(GLOB UTIL_FILES "${CMAKE_SOURCE_DIR}/UTIL/*.f*")
# file(GLOB MAIN_FILES "${CMAKE_SOURCE_DIR}/MAIN/*.[fF]*")
add_executable(
  mystran
  ${ALL_FORTRAN_FILES}
)
#target_link_libraries(mystran MODULES ${modules_names})
target_link_libraries(mystran dgssv superlu f2c)
if (MISSING_FNS_TOTAL GREATER 0)
  message(STATUS "Linking against local BLAS")
  target_link_libraries(mystran my_blas)
endif()
#if (MISSING_FNS_TOTAL GREATER 0)
#  target_link_libraries(mystran my_blas)
#endif()
set_target_properties(
  mystran PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}
)
if (CMAKE_COMPILER_IS_GNUCC)
  set(CMAKE_Fortran_FLAGS "-Wall -Wextra \
    -Wno-unused-variable -Wno-unused-label -Wno-unused-parameter -Wno-tabs \
    -Wno-compare-reals -Wno-character-truncation -Wno-unused-dummy-argument \
    -Wmaybe-uninitialized -Wrealloc-lhs -fcheck=all")
endif()

# issue a couple messages about compilation
include(ProcessorCount)
ProcessorCount(NCPU)
message(STATUS "You can now compile MYSTRAN with `cmake --build .`")
if (NOT NCPU EQUAL 0)
  message(STATUS "Compile faster by passing the -j${NCPU} flag.")
endif()
