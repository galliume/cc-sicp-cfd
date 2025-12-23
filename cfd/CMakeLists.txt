##################### CFD Project ##############

cmake_minimum_required(VERSION 3.31.0)

if(CMAKE_VERSION VERSION_GREATER_EQUAL "4.0")
  set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD
    "d0edc3af-4c50-42ea-a356-e2862fe7a444")
else()
  set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD
    "d0edc3af-4c50-42ea-a356-e2862fe7a444")
endif()

set(CMAKE_CXX_MODULE_STD 1)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pthread")
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++ -pthread")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stdlib=libc++ -lc++abi -pthread")
endif()

project(CFD
  VERSION 0.0.1
  DESCRIPTION "CFD solver with SICP mindset"
  HOMEPAGE_URL "https://github.com/galliume/cc-sicp-cfd"
  LANGUAGES C CXX)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  list(APPEND EXTRA_LIBS
    Ws2_32 user32 gdi32 winspool shell32 ole32 oleaut32 uuid comdlg32 advapi32)
endif()

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)
list(APPEND EXTRA_LIBS Threads::Threads)

set(CFD_LIB cfd_lib)
set(CFD_TEST cfd_tests)
set(CFD_BENCHMARK cfd_benchmark)

add_executable(${PROJECT_NAME} src/CFD.cpp)

add_subdirectory(src)

include(CheckIPOSupported)
check_ipo_supported(RESULT ipo_supported OUTPUT ipo_error)
if(ipo_supported)
  message(STATUS "ipo_supported")
  set_target_properties(${PROJECT_NAME} PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  message(STATUS "Found ccache: ${CCACHE_PROGRAM}")
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif()

message(STATUS "Generated with config types: ${CMAKE_CONFIGURATION_TYPES}")
message(STATUS "CMAKE_CXX_STANDARD: ${CMAKE_CXX_STANDARD}")
message(STATUS "CMAKE_CXX_STANDARD_REQUIRED: ${CMAKE_CXX_STANDARD_REQUIRED}")
message(STATUS "CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")

set(CMAKE_BUILD_TYPE "${CMAKE_BUILD_TYPE}" CACHE STRING "Build type" FORCE)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules ${CMAKE_MODULE_PATH})

configure_file(CFD.h.in CFD.h)

target_include_directories(${PROJECT_NAME}
PRIVATE
  ${PROJECT_SOURCE_DIR}/src)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  target_compile_definitions(${PROJECT_NAME} PRIVATE
      _CRT_SECURE_NO_WARNINGS
      WIN32_LEAN_AND_MEAN)
endif()

target_compile_features(${PROJECT_NAME}
  PRIVATE   cxx_std_23
  INTERFACE cxx_std_23)

target_include_directories(${PROJECT_NAME} PRIVATE ${PROJECT_BINARY_DIR})

target_compile_options(${PROJECT_NAME} PRIVATE
  $<$<CONFIG:DEBUG>:-g;-O0;-Werror>
  $<$<CONFIG:RELEASE>:-O3;-DNDEBUG>
  -Wno-c++98-compat
  -Weverything)

# target_link_options no longer needed - set globally above

# target_compile_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-fsanitize=address,undefined>)
# target_link_options(${PROJECT_NAME} PRIVATE $<$<CONFIG:Debug>:-fsanitize=address,undefined>)

#target_compile_definitions(${PROJECT_NAME} PRIVATE $<$<CONFIG:DEBUG>:CC_DEBUG_BUILD>)

message(STATUS "EXTRA_LIBS: ${EXTRA_LIBS}")
target_link_libraries(${PROJECT_NAME} PRIVATE ${EXTRA_LIBS})
target_link_libraries(${PROJECT_NAME} PRIVATE ${CFD_LIB})

message(STATUS "Fetching dependancies")

include(fetchDeps)

set(CC_LIB_DIR "$<TARGET_FILE_DIR:${PROJECT_NAME}>/lib")
set(CC_BIN_DIR "$<TARGET_FILE_DIR:${PROJECT_NAME}>/bin")

install(TARGETS ${PROJECT_NAME}
    EXPORT CFDTargets
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    FILE_SET cxx_headers DESTINATION include
    FILE_SET cxx_modules DESTINATION include)

add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:${PROJECT_NAME}> $<TARGET_RUNTIME_DLLS:${PROJECT_NAME}>
  COMMAND_EXPAND_LISTS
)

option(BUILD_TESTING "Build the tests" ON)

if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(tests)
  add_subdirectory(benchmarks)
endif()

##################### Install ###########################

# install(TARGETS
#   ${PROJECT_NAME}
# RUNTIME
#   COMPONENT Runtime
# LIBRARY
#   COMPONENT Runtime
#   NAMELINK_COMPONENT Development
# ARCHIVE
#   COMPONENT Development
#   DESTINATION lib/static
# FILE_SET HEADERS
#   COMPONENT Development
# )

# find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
# if(CLANG_TIDY_EXE)
#   set_target_properties(${PROJECT_NAME} PROPERTIES
#     CXX_CLANG_TIDY "${CLANG_TIDY_EXE};-checks=-*,modernize-*,performance-*")
# endif()

include(graphVizOptions)
