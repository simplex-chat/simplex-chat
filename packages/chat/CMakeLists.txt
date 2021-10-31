cmake_minimum_required(VERSION 3.11) # set min version of cmake

project(SimpleXChat)

enable_testing()

include_directories(include)

add_subdirectory( src build/src )
add_subdirectory( test build/test )