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
typedef long* chat_ctrl;

extern char *chat_migrate_init(const char *path, const char *key, chat_ctrl *ctrl);
extern chat_ctrl chat_init_key(const char *path, const char *key);
extern chat_ctrl chat_init(const char *path); // deprecated
extern char *chat_send_cmd(chat_ctrl ctrl, const char *cmd);
extern char *chat_recv_msg(chat_ctrl ctrl); // deprecated
extern char *chat_recv_msg_wait(chat_ctrl ctrl, const int wait);
extern char *chat_parse_markdown(const char *str);

JNIEXPORT jobjectArray JNICALL
Java_chat_simplex_app_SimplexAppKt_chatMigrateInit(JNIEnv *env, __unused jclass clazz, jstring dbPath, jstring dbKey) {
    const char *_dbPath = (*env)->GetStringUTFChars(env, dbPath, JNI_FALSE);
    const char *_dbKey = (*env)->GetStringUTFChars(env, dbKey, JNI_FALSE);
    jlong _ctrl = (jlong) 0;
    jstring res = (*env)->NewStringUTF(env, chat_migrate_init(_dbPath, _dbKey, &_ctrl));
    (*env)->ReleaseStringUTFChars(env, dbPath, _dbPath);
    (*env)->ReleaseStringUTFChars(env, dbKey, _dbKey);
    jobjectArray ret = (jobjectArray)(*env)->NewObjectArray(env, 2, (*env)->FindClass(env, "java/lang/String"), (*env)->NewStringUTF(env, ""));
    (*env)->SetObjectArrayElement(env, ret, 0, res);
    (*env)->SetObjectArrayElement(env, ret, 1, (jstring*)_ctrl);
    return ret;
}

JNIEXPORT jlong JNICALL
Java_chat_simplex_app_SimplexAppKt_chatInitKey(JNIEnv *env, __unused jclass clazz, jstring dbPath, jstring dbKey) {
    const char *_dbPath = (*env)->GetStringUTFChars(env, dbPath, JNI_FALSE);
    const char *_dbKey = (*env)->GetStringUTFChars(env, dbKey, JNI_FALSE);
    jlong ctrl = (jlong)chat_init_key(_dbPath, _dbKey);
    (*env)->ReleaseStringUTFChars(env, dbPath, _dbPath);
    (*env)->ReleaseStringUTFChars(env, dbKey, _dbKey);
    return ctrl;
}

JNIEXPORT jlong JNICALL
Java_chat_simplex_app_SimplexAppKt_chatInit(JNIEnv *env, __unused jclass clazz, jstring dbPath) {
    const char *_dbPath = (*env)->GetStringUTFChars(env, dbPath, JNI_FALSE);
    jlong ctrl = (jlong)chat_init(_dbPath);
    (*env)->ReleaseStringUTFChars(env, dbPath, _dbPath);
    return ctrl;
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

JNIEXPORT jstring JNICALL
Java_chat_simplex_app_SimplexAppKt_chatRecvMsgWait(JNIEnv *env, __unused jclass clazz, jlong controller, jint wait) {
    return (*env)->NewStringUTF(env, chat_recv_msg_wait((void*)controller, wait));
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_app_SimplexAppKt_chatParseMarkdown(JNIEnv *env, __unused jclass clazz, jstring str) {
    const char *_str = (*env)->GetStringUTFChars(env, str, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_parse_markdown(_str));
    (*env)->ReleaseStringUTFChars(env, str, _str);
    return res;
}
