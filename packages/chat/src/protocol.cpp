#include "protocol.h"

#include <string>

using ::std::string;

const char* executeCommand(const char *command) {
    if (command == nullptr) {
        auto *output = new string("> \nFailed");
        return output->c_str();
    }
    string cmd(command);
    string result = "Successful";
    auto *output = new string("> " + cmd + "\n" + result);
    return output->c_str();
}