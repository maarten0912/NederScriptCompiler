project (matrix)
cmake_minimum_required(VERSION 2.8)
set (matrix_VERSION_MAJOR 0)
set (matrix_VERSION_MINOR 1)

include(CheckLibraryExists)
include(CheckCSourceCompiles)

find_package( OpenCL REQUIRED )

if(BUILD_FOR STREQUAL teacher)
else()
    set(BUILD_FOR student)
endif()

message("Building configuration: ${CMAKE_BUILD_TYPE}")
message("Building for: ${BUILD_FOR}")
if(CMAKE_BUILD_TYPE STREQUAL Release)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE} -DNDEBUG -O3")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE} -DNDEBUG -O3")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_DEBUG} -g -UNDEBUG -O0 -DYYDEBUG=1")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_DEBUG} -g -UNDEBUG -O0 -DYYDEBUG=1")
endif()

if(CMAKE_BUILD_TYPE STREQUAL Profile)
    set(CMAKE_CXX_FLAGS "-pg")
    set(CMAKE_C_FLAGS "-pg")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11")

CHECK_LIBRARY_EXISTS(rt clock_gettime "time.h" HAVE_CLOCK_GETTIME)
if (NOT HAVE_CLOCK_GETTIME)
   set(CMAKE_EXTRA_INCLUDE_FILES time.h)
   CHECK_FUNCTION_EXISTS(clock_gettime HAVE_CLOCK_GETTIME)
   SET(CMAKE_EXTRA_INCLUDE_FILES)
endif()

FIND_LIBRARY(HAVE_LIBRT NAMES rt)
if(HAVE_LIBRT)
    set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} rt)
endif()
check_c_source_compiles(
    "
    #define _POSIX_C_SOURCE 199309L
    #include <time.h>
    int main() { return clock_gettime(CLOCK_MONOTONIC, NULL); }
    "
    HAS_POSIX_CLOCK_MONOTONIC
)
set(CMAKE_REQUIRED_LIBRARIES)

add_definitions(-Wall -pedantic -Wextra -rdynamic -fopenmp)

if(HAS_POSIX_CLOCK_MONOTONIC)
    add_definitions(-DHAS_POSIX_CLOCK_MONOTONIC)
endif()

include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${OpenCL_INCLUDE_DIR}
)

set(SRCS
    main.cpp
    tests.cpp
    matrix.c
    vector.c
    kernels_sequential.c
)

if(BUILD_FOR STREQUAL teacher)
    set(SRCS ${SRCS}
        kernels_openmp_teacher.c
        kernels_opencl_teacher.c
    )
else()
    set(SRCS ${SRCS}
        kernels_openmp.c
        kernels_opencl.c
    )
endif()

add_executable(matrix
    ${SRCS}
)

target_link_libraries(matrix
#-Wl,-E
m
gomp
${OpenCL_LIBRARY}
)

install (TARGETS matrix DESTINATION bin)
