#include <jni.h>
#include <string>

#include "../../../../../chat/include/protocol.h"

extern "C" JNIEXPORT jstring JNICALL
Java_chat_simplex_app_Protocol_executeCommand(
        JNIEnv* env,
        jclass clazz,
        jstring command) {
    const char *utf8command = env->GetStringUTFChars(command, JNI_FALSE);
    jstring result = env->NewStringUTF(executeCommand(utf8command));
    env->ReleaseStringUTFChars(command, utf8command);
    return result;
}