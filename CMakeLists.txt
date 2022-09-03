# CMakeLists.txt for compiling MYSTRAN with gfortran in a GNU environment
# based on an older CMakeLists.txt by ceanwang@gmail.com
# made to work again by Bruno Borges Paschoalinoto (2020)

# set up basic project info
cmake_minimum_required(VERSION 3.18)
enable_language(Fortran)
project(Mystran)

# basic compiler and output options
set(CMAKE_SOURCE_DIR "Source")
set(PROJECT_BINARY_DIR "Binaries")
set(CMAKE_FLAGS "-c -g -fbacktrace")
set(CMAKE_BUILD_TYPE Debug)

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

# SuperLU stuff
set(SUPERLU_VERSION "5.2.2")
set(SUPERLU_DIR "superlu-${SUPERLU_VERSION}")
SET(SUPERLU_FN "superlu_${SUPERLU_VERSION}.tar.gz")
set(SUPERLU_URL "https://www.mystran.com/SuperLU/${SUPERLU_FN}")
include_directories("${SUPERLU_DIR}/SRC")

# download the superlu source if needed
if(NOT EXISTS ${SUPERLU_FN})
  message(
    STATUS
    "Downloading SuperLU ${SUPERLU_VERSION} source from ${SUPERLU_URL}..."
  )
  file(DOWNLOAD ${SUPERLU_URL} ${SUPERLU_FN} TIMEOUT 60 STATUS DOWNLOAD_STATUS)
  # Check if download was successful.
  list(GET DOWNLOAD_STATUS 0 STATUS_CODE)
  list(GET DOWNLOAD_STATUS 1 ERROR_MESSAGE)
  if(${STATUS_CODE} EQUAL 0)
    message(STATUS "Done downloading SuperLU.")
  else()
    # Exit CMake if the download failed, printing the error message.
    file(REMOVE ${SUPERLU_FN})
    message(FATAL_ERROR "Error occurred during download of SuperLU: ${ERROR_MESSAGE}")
  endif()
endif()

# extract superlu source
file(ARCHIVE_EXTRACT INPUT ${SUPERLU_FN})

# f2c stuff
set(F2C_DIR "f2c")
set(F2C_INCLUDE_DIR "${F2C_DIR}/include")
set(F2C_FN "${F2C_DIR}/libf2c.zip")
set(F2C_URL "https://www.netlib.org/${F2C_FN}")

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
    message(FATAL_ERROR "Error occurred during download of libf2c: ${ERROR_MESSAGE}")
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

# build the SuperLU and BLAS static libs!
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
set(
  MODULES_ALL_FILES ${USE_IFs_FILES} ${Interfaces_FILES} ${Modules_FILES}
  ${Modules_LAPACK_FILES} ${Modules_ARPACK_FILES}
)
add_library(MODULES ${MODULES_ALL_FILES})

# collect modules
list(APPEND modules_names ARPACK BANDIT LK1 LK2 LK3 LK4 LK5 LK6 LK9 EMG)
foreach (modname IN LISTS modules_names)
  file(GLOB_RECURSE TMP_MOD_FILES "${CMAKE_SOURCE_DIR}/${modname}/*.f*")
  add_library(${modname} ${TMP_MOD_FILES} ${TMP_MOD_FILES_PP})
  target_link_libraries(${modname} MODULES)
endforeach()

# add the DTRSV and DGSSV C modules.
add_library(dgssv "${SUPERLU_DIR}/FORTRAN/c_fortran_dgssv.c")
#add_library(dtrsv "${SUPERLU_DIR}/SRC/dtrsv.c")
target_link_libraries(dgssv blas)


# prepare the main executable, linked against the specifics and the m
# it appears utils used to be a module, but that is no longer the case?
file(GLOB_RECURSE UTIL_FILES "${CMAKE_SOURCE_DIR}/UTIL/*.f*")
file(GLOB_RECURSE MAIN_FILES "${CMAKE_SOURCE_DIR}/MAIN/*.[fF]*")
add_executable(mystran ${MAIN_FILES} ${MODULES_ALL_FILES} ${UTIL_FILES})
target_link_libraries(mystran ${modules_names})
target_link_libraries(mystran dgssv superlu f2c)
set_target_properties(
  mystran PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}
)

# issue a couple messages about compilation
include(ProcessorCount)
ProcessorCount(NCPU)
message(STATUS "You can now compile MYSTRAN with make.")
if (NOT NCPU EQUAL 0)
  message(STATUS "Compile faster by passing -j${NCPU} to make.")
endif()
