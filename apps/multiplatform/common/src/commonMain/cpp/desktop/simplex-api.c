#include <jni.h>
#include <string.h>

// from the RTS
void hs_init(int * argc, char **argv[]);

//extern void __svfscanf(void){};
//extern void __vfwscanf(void){};
//extern void __memset_chk_fail(void){};
//extern void __strcpy_chk_generic(void){};
//extern void __strcat_chk_generic(void){};
//extern void __libc_globals(void){};
//extern void __rel_iplt_start(void){};

// Android 9 only, not 13
//extern void reallocarray(void){};

JNIEXPORT void JNICALL
Java_chat_simplex_common_platform_CoreKt_initHS(JNIEnv *env, jclass clazz) {
    hs_init(NULL, NULL);
}

// from simplex-chat
typedef long* chat_ctrl;

extern char *chat_migrate_init(const char *path, const char *key, const char *confirm, chat_ctrl *ctrl);
extern char *chat_send_cmd(chat_ctrl ctrl, const char *cmd);
extern char *chat_recv_msg(chat_ctrl ctrl); // deprecated
extern char *chat_recv_msg_wait(chat_ctrl ctrl, const int wait);
extern char *chat_parse_markdown(const char *str);
extern char *chat_parse_server(const char *str);
extern char *chat_password_hash(const char *pwd, const char *salt);


// As a reference: https://stackoverflow.com/a/60002045
jstring correct_string_utf8(JNIEnv *env, char *string) {
    jobject bb = (*env)->NewDirectByteBuffer(env, (void *)string, strlen(string));
    jclass cls_charset = (*env)->FindClass(env, "java/nio/charset/Charset");
    jmethodID mid_charset_forName = (*env)->GetStaticMethodID(env, cls_charset, "forName", "(Ljava/lang/String;)Ljava/nio/charset/Charset;");
    jobject charset = (*env)->CallStaticObjectMethod(env, cls_charset, mid_charset_forName, (*env)->NewStringUTF(env, "UTF-8"));

    jmethodID mid_decode = (*env)->GetMethodID(env, cls_charset, "decode", "(Ljava/nio/ByteBuffer;)Ljava/nio/CharBuffer;");
    jobject cb = (*env)->CallObjectMethod(env, charset, mid_decode, bb);

    jclass cls_char_buffer = (*env)->FindClass(env, "java/nio/CharBuffer");
    jmethodID mid_to_string = (*env)->GetMethodID(env, cls_char_buffer, "toString", "()Ljava/lang/String;");
    jstring res = (*env)->CallObjectMethod(env, cb, mid_to_string);

    (*env)->DeleteLocalRef(env, bb);
    (*env)->DeleteLocalRef(env, charset);
    (*env)->DeleteLocalRef(env, cb);
    return res;
}


JNIEXPORT jobjectArray JNICALL
Java_chat_simplex_common_platform_CoreKt_chatMigrateInit(JNIEnv *env, jclass clazz, jstring dbPath, jstring dbKey, jstring confirm) {
    const char *_dbPath = (*env)->GetStringUTFChars(env, dbPath, JNI_FALSE);
    const char *_dbKey = (*env)->GetStringUTFChars(env, dbKey, JNI_FALSE);
    const char *_confirm = (*env)->GetStringUTFChars(env, confirm, JNI_FALSE);
    jlong _ctrl = (jlong) 0;
    jstring res = correct_string_utf8(env, chat_migrate_init(_dbPath, _dbKey, _confirm, &_ctrl));
    (*env)->ReleaseStringUTFChars(env, dbPath, _dbPath);
    (*env)->ReleaseStringUTFChars(env, dbKey, _dbKey);
    (*env)->ReleaseStringUTFChars(env, dbKey, _confirm);

    // Creating array of Object's (boxed values can be passed, eg. Long instead of long)
    jobjectArray ret = (jobjectArray)(*env)->NewObjectArray(env, 2, (*env)->FindClass(env, "java/lang/Object"), NULL);
    // Java's String
    (*env)->SetObjectArrayElement(env, ret, 0, res);
    // Java's Long
    (*env)->SetObjectArrayElement(env, ret, 1,
        (*env)->NewObject(env, (*env)->FindClass(env, "java/lang/Long"),
        (*env)->GetMethodID(env, (*env)->FindClass(env, "java/lang/Long"), "<init>", "(J)V"),
        _ctrl));
    return ret;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatSendCmd(JNIEnv *env, jclass clazz, jlong controller, jstring msg) {
    const char *_msg = (*env)->GetStringUTFChars(env, msg, JNI_FALSE);
    jstring res = correct_string_utf8(env, chat_send_cmd((void*)controller, _msg));
    (*env)->ReleaseStringUTFChars(env, msg, _msg);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatRecvMsg(JNIEnv *env, jclass clazz, jlong controller) {
    return correct_string_utf8(env, chat_recv_msg((void*)controller));
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatRecvMsgWait(JNIEnv *env, jclass clazz, jlong controller, jint wait) {
    return correct_string_utf8(env, chat_recv_msg_wait((void*)controller, wait));
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatParseMarkdown(JNIEnv *env, jclass clazz, jstring str) {
    const char *_str = (*env)->GetStringUTFChars(env, str, JNI_FALSE);
    jstring res = correct_string_utf8(env, chat_parse_markdown(_str));
    (*env)->ReleaseStringUTFChars(env, str, _str);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatParseServer(JNIEnv *env, jclass clazz, jstring str) {
    const char *_str = (*env)->GetStringUTFChars(env, str, JNI_FALSE);
    jstring res = correct_string_utf8(env, chat_parse_server(_str));
    (*env)->ReleaseStringUTFChars(env, str, _str);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatPasswordHash(JNIEnv *env, jclass clazz, jstring pwd, jstring salt) {
    const char *_pwd = (*env)->GetStringUTFChars(env, pwd, JNI_FALSE);
    const char *_salt = (*env)->GetStringUTFChars(env, salt, JNI_FALSE);
    jstring res = correct_string_utf8(env, chat_password_hash(_pwd, _salt));
    (*env)->ReleaseStringUTFChars(env, pwd, _pwd);
    (*env)->ReleaseStringUTFChars(env, salt, _salt);
    return res;
}
