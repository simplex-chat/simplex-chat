cmake_minimum_required(VERSION 3.11)
project(SimpleXChatTest)

include(FetchContent)
FetchContent_Declare(
        googletest
        # Specify the commit you depend on and update it regularly.
        URL https://github.com/google/googletest/archive/609281088cfefc76f9d0ce82e1ff6c30cc3591e5.zip
)
FetchContent_MakeAvailable(googletest)

# Now simply link against gtest or gtest_main as needed. Eg
file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/*.cpp)
add_executable(SimpleXChatTest ${SRC_FILES})

target_link_libraries(SimpleXChatTest gtest_main SimpleXChatLib)
add_test(NAME SimpleXChatTest COMMAND SimpleXChatTest)
