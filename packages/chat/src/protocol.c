#include "protocol.h"

extern void commandSend(Message m) {

}

Message getMessage() {
    Message m;
    m.message = "123";
    m.date = 1000;
    return m;
}

