# set required cmake version
cmake_minimum_required(VERSION 3.19...3.28)

project(
  mqt-core
  LANGUAGES CXX
  DESCRIPTION "MQT Core - The Backbone of the Munich Quantum Toolkit")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(cmake/StandardProjectSettings.cmake)
include(cmake/PreventInSourceBuilds.cmake)
include(cmake/PackageAddTest.cmake)
include(cmake/Cache.cmake)

option(BUILD_MQT_CORE_TESTS "Also build tests for the MQT Core project" ON)
option(BUILD_MQT_CORE_BENCHMARKS "Also build benchmarks for the MQT Core project")
option(BUILD_MQT_CORE_BINDINGS "Build the MQT Core Python bindings" OFF)

if(BUILD_MQT_CORE_BINDINGS)
  # ensure that the BINDINGS option is set
  set(BINDINGS
      ON
      CACHE BOOL "Enable settings related to Python bindings" FORCE)
  set(Python_FIND_VIRTUALENV
      FIRST
      CACHE STRING "Give precedence to virtualenvs when searching for Python")
  set(Python_ARTIFACTS_INTERACTIVE
      ON
      CACHE BOOL "Prevent multiple searches for Python and instead cache the results.")
  # top-level call to find Python
  find_package(
    Python 3.8 REQUIRED
    COMPONENTS Interpreter Development.Module
    OPTIONAL_COMPONENTS Development.SABIModule)
endif()

if(NOT TARGET project_warnings)
  # Use the warnings specified in CompilerWarnings.cmake
  add_library(project_warnings INTERFACE)

  # Standard compiler warnings
  include(cmake/CompilerWarnings.cmake)
  set_project_warnings(project_warnings)
endif()

if(NOT TARGET project_options)
  # Use the options specified in CompilerOptions.cmake
  add_library(project_options INTERFACE)

  # Standard compiler options
  include(cmake/CompilerOptions.cmake)
  enable_project_options(project_options)

  # Sanitizer options if supported by compiler
  include(cmake/Sanitizers.cmake)
  enable_sanitizers(project_options)
endif()

include(cmake/ExternalDependencies.cmake)

# add main library code
add_subdirectory(src)

# add test code
if(BUILD_MQT_CORE_TESTS)
  enable_testing()
  include(GoogleTest)
  add_subdirectory(test)
endif()

if(BUILD_MQT_CORE_BENCHMARKS)
  add_subdirectory(eval)
endif()
