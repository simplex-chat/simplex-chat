
#include "protocol.h"
#include "gtest/gtest.h"

namespace {

TEST(ProtocolTest, SuccessfulCommand) {

    EXPECT_STREQ(executeCommand("Test1"), "> Test1\nSuccessful");
    EXPECT_STREQ(executeCommand("Test2"), "> Test2\nSuccessful");
    EXPECT_STREQ(executeCommand("Test3"), "> Test3\nSuccessful");
}

TEST(ProtocolTest, NullCommand) {
    EXPECT_STREQ(executeCommand(nullptr), "> \nFailed");
}

}  // namespace