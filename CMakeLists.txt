# set minimum cmake version
cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

# project name
project(sec_quant_F90_autocmake)

# do not rebuild if rules (compiler flags) change
set(CMAKE_SKIP_RULE_DEPENDENCY TRUE)

# if CMAKE_BUILD_TYPE undefined, we set it to Debug
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Debug")
endif()

# directory which holds enabled cmake modules
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH}
    ${PROJECT_SOURCE_DIR}/cmake/modules)

# included cmake modules
include(autocmake_cc)
include(autocmake_code_coverage)
include(autocmake_cxx)
include(autocmake_default_build_paths)
include(autocmake_fc)
include(autocmake_int64)
include(autocmake_math_libs)
include(autocmake_safeguards)
include(autocmake_static_linking)
include(autocmake_src)
