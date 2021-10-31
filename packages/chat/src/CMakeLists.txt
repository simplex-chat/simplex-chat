cmake_minimum_required(VERSION 3.10) # set min version of cmake

project(SimpleXChatLib) # create chat lib

set(CMAKE_CXX_STANDARD 17)
# set(CMAKE_CXX_CLANG_TIDY "clang-tidy;-header-filter=../include;-checks=*;")

include_directories ("${PROJECT_SOURCE_DIR}/../include")
file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/*.cpp)
add_library(SimpleXChatLib STATIC ${SRC_FILES})
