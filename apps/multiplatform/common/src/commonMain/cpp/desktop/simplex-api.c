#include <jni.h>
#include <string.h>
#include <stdlib.h>

// from the RTS
void hs_init(int * argc, char **argv[]);

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
jstring decode_to_utf8_string(JNIEnv *env, char *string) {
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

char * encode_to_utf8_chars(JNIEnv *env, jstring string) {
    if (!string) return "";

    const jclass cls_string = (*env)->FindClass(env, "java/lang/String");
    const jmethodID mid_getBytes = (*env)->GetMethodID(env, cls_string, "getBytes", "(Ljava/lang/String;)[B");
    const jbyteArray jbyte_array = (jbyteArray) (*env)->CallObjectMethod(env, string, mid_getBytes, (*env)->NewStringUTF(env, "UTF-8"));
    jint length = (jint) (*env)->GetArrayLength(env, jbyte_array);
    jbyte *jbytes = malloc(length + 1);
    (*env)->GetByteArrayRegion(env, jbyte_array, 0, length, jbytes);
    // char * should be null terminated but jbyte * isn't. Terminate it with \0. Otherwise, Haskell will not see the end of string
    jbytes[length] = '\0';

    //for (int i = 0; i < length; ++i)
    //    fprintf(stderr, "%d: %02x\n", i, jbytes[i]);

    (*env)->DeleteLocalRef(env, jbyte_array);
    (*env)->DeleteLocalRef(env, cls_string);
    return (char *) jbytes;
}

JNIEXPORT jobjectArray JNICALL
Java_chat_simplex_common_platform_CoreKt_chatMigrateInit(JNIEnv *env, jclass clazz, jstring dbPath, jstring dbKey, jstring confirm) {
    const char *_dbPath = encode_to_utf8_chars(env, dbPath);
    const char *_dbKey = encode_to_utf8_chars(env, dbKey);
    const char *_confirm = encode_to_utf8_chars(env, confirm);
    long int *_ctrl = (long) 0;
    jstring res = decode_to_utf8_string(env, chat_migrate_init(_dbPath, _dbKey, _confirm, &_ctrl));
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
    const char *_msg = encode_to_utf8_chars(env, msg);
    jstring res = decode_to_utf8_string(env, chat_send_cmd((void*)controller, _msg));
    (*env)->ReleaseStringUTFChars(env, msg, _msg);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatRecvMsg(JNIEnv *env, jclass clazz, jlong controller) {
    return decode_to_utf8_string(env, chat_recv_msg((void*)controller));
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatRecvMsgWait(JNIEnv *env, jclass clazz, jlong controller, jint wait) {
    return decode_to_utf8_string(env, chat_recv_msg_wait((void*)controller, wait));
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatParseMarkdown(JNIEnv *env, jclass clazz, jstring str) {
    const char *_str = encode_to_utf8_chars(env, str);
    jstring res = decode_to_utf8_string(env, chat_parse_markdown(_str));
    (*env)->ReleaseStringUTFChars(env, str, _str);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatParseServer(JNIEnv *env, jclass clazz, jstring str) {
    const char *_str = encode_to_utf8_chars(env, str);
    jstring res = decode_to_utf8_string(env, chat_parse_server(_str));
    (*env)->ReleaseStringUTFChars(env, str, _str);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatPasswordHash(JNIEnv *env, jclass clazz, jstring pwd, jstring salt) {
    const char *_pwd = encode_to_utf8_chars(env, pwd);
    const char *_salt = encode_to_utf8_chars(env, salt);
    jstring res = decode_to_utf8_string(env, chat_password_hash(_pwd, _salt));
    (*env)->ReleaseStringUTFChars(env, pwd, _pwd);
    (*env)->ReleaseStringUTFChars(env, salt, _salt);
    return res;
}
