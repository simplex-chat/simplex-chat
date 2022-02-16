#include <jni.h>

// from the RTS
void hs_init(int * argc, char **argv[]);

// from android-support
void setLineBuffering(void);
int pipe_std_to_socket(const char * name);

JNIEXPORT jint JNICALL
Java_chat_simplex_app_SimplexAppKt_pipeStdOutToSocket(JNIEnv *env, __unused jclass clazz, jstring socket_name) {
    const char *name = (*env)->GetStringUTFChars(env, socket_name, JNI_FALSE);
    int ret = pipe_std_to_socket(name);
    (*env)->ReleaseStringUTFChars(env, socket_name, name);
    return ret;
}

JNIEXPORT void JNICALL
Java_chat_simplex_app_SimplexAppKt_initHS(__unused JNIEnv *env, __unused jclass clazz) {
    hs_init(NULL, NULL);
    setLineBuffering();
}

// from simplex-chat
typedef void* chat_ctrl;

extern chat_ctrl chat_init(const char * path);
extern char *chat_send_cmd(chat_ctrl ctrl, const char *cmd);
extern char *chat_recv_msg(chat_ctrl ctrl);

JNIEXPORT jlong JNICALL
Java_chat_simplex_app_SimplexAppKt_chatInit(JNIEnv *env, __unused jclass clazz, jstring datadir) {
    const char *_data = (*env)->GetStringUTFChars(env, datadir, JNI_FALSE);
    jlong res = (jlong)chat_init(_data);
    (*env)->ReleaseStringUTFChars(env, datadir, _data);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_app_SimplexAppKt_chatSendCmd(JNIEnv *env, __unused jclass clazz, jlong controller, jstring msg) {
    const char *_msg = (*env)->GetStringUTFChars(env, msg, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_send_cmd((void*)controller, _msg));
    (*env)->ReleaseStringUTFChars(env, msg, _msg);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_app_SimplexAppKt_chatRecvMsg(JNIEnv *env, __unused jclass clazz, jlong controller) {
    return (*env)->NewStringUTF(env, chat_recv_msg((void*)controller));
}
