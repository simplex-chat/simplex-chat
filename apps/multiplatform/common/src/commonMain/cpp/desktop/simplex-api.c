#include <jni.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

// from the RTS
void hs_init_with_rtsopts(int * argc, char **argv[]);

JNIEXPORT void JNICALL
Java_chat_simplex_common_platform_CoreKt_initHS(JNIEnv *env, jclass clazz) {
#ifdef _WIN32
    int argc = 4;
    char *argv[] = {"simplex", "+RTS", "-A16m", "-H64m", NULL}; // non-moving GC is broken on windows with GHC 9.4-9.6.3
#else
    int argc = 5;
    char *argv[] = {"simplex", "+RTS", "-A16m", "-H64m", "-xn", NULL}; // see android/simplex-api.c for details
#endif
    char **pargv = argv;
    hs_init_with_rtsopts(&argc, &pargv);
}

// from simplex-chat
typedef long* chat_ctrl;

/*
   When you start using any new function from Haskell libraries,
   you have to add the function name to the file libsimplex.dll.def in the root directory.
   And do the same by adding it into flake.nix file in the root directory,
   Otherwise, Windows and Android libraries cannot be built.
*/

extern char *chat_migrate_init(const char *path, const char *key, const char *confirm, chat_ctrl *ctrl);
extern char *chat_send_cmd(chat_ctrl ctrl, const char *cmd);
extern char *chat_send_remote_cmd(chat_ctrl ctrl, const int rhId, const char *cmd);
extern char *chat_recv_msg(chat_ctrl ctrl); // deprecated
extern char *chat_recv_msg_wait(chat_ctrl ctrl, const int wait);
extern char *chat_parse_markdown(const char *str);
extern char *chat_parse_server(const char *str);
extern char *chat_password_hash(const char *pwd, const char *salt);
extern char *chat_valid_name(const char *name);
extern char *chat_write_file(chat_ctrl ctrl, const char *path, char *ptr, int length);
extern char *chat_read_file(const char *path, const char *key, const char *nonce);
extern char *chat_encrypt_file(chat_ctrl ctrl, const char *from_path, const char *to_path);
extern char *chat_decrypt_file(const char *from_path, const char *key, const char *nonce, const char *to_path);

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
    (*env)->ReleaseStringUTFChars(env, confirm, _confirm);

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
Java_chat_simplex_common_platform_CoreKt_chatSendRemoteCmd(JNIEnv *env, jclass clazz, jlong controller, jint rhId, jstring msg) {
    const char *_msg = encode_to_utf8_chars(env, msg);
    jstring res = decode_to_utf8_string(env, chat_send_remote_cmd((void*)controller, rhId, _msg));
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

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatValidName(JNIEnv *env, jclass clazz, jstring name) {
    const char *_name = encode_to_utf8_chars(env, name);
    jstring res = decode_to_utf8_string(env, chat_valid_name(_name));
    (*env)->ReleaseStringUTFChars(env, name, _name);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatWriteFile(JNIEnv *env, jclass clazz, jlong controller, jstring path, jobject buffer) {
    const char *_path = encode_to_utf8_chars(env, path);
    jbyte *buff = (jbyte *) (*env)->GetDirectBufferAddress(env, buffer);
    jlong capacity = (*env)->GetDirectBufferCapacity(env, buffer);
    jstring res = decode_to_utf8_string(env, chat_write_file((void*)controller, _path, buff, capacity));
    (*env)->ReleaseStringUTFChars(env, path, _path);
    return res;
}

JNIEXPORT jobjectArray JNICALL
Java_chat_simplex_common_platform_CoreKt_chatReadFile(JNIEnv *env, jclass clazz, jstring path, jstring key, jstring nonce) {
    const char *_path = encode_to_utf8_chars(env, path);
    const char *_key = encode_to_utf8_chars(env, key);
    const char *_nonce = encode_to_utf8_chars(env, nonce);

    jbyte *res = chat_read_file(_path, _key, _nonce);
    (*env)->ReleaseStringUTFChars(env, path, _path);
    (*env)->ReleaseStringUTFChars(env, key, _key);
    (*env)->ReleaseStringUTFChars(env, nonce, _nonce);

    jint status = (jint)res[0];
    jbyteArray arr;
    if (status == 0) {
        union {
            uint32_t w;
            uint8_t b[4];
        } len;
        len.b[0] = (uint8_t)res[1];
        len.b[1] = (uint8_t)res[2];
        len.b[2] = (uint8_t)res[3];
        len.b[3] = (uint8_t)res[4];
        arr = (*env)->NewByteArray(env, len.w);
        (*env)->SetByteArrayRegion(env, arr, 0, len.w, res + 5);
    } else {
        int len = strlen(res + 1); // + 1 offset here is to not include status byte
        arr = (*env)->NewByteArray(env, len);
        (*env)->SetByteArrayRegion(env, arr, 0, len, res + 1);
    }

    jobjectArray ret = (jobjectArray)(*env)->NewObjectArray(env, 2, (*env)->FindClass(env, "java/lang/Object"), NULL);
    jobject statusObj = (*env)->NewObject(env, (*env)->FindClass(env, "java/lang/Integer"),
                                           (*env)->GetMethodID(env, (*env)->FindClass(env, "java/lang/Integer"), "<init>", "(I)V"),
                                           status);
    (*env)->SetObjectArrayElement(env, ret, 0, statusObj);
    (*env)->SetObjectArrayElement(env, ret, 1, arr);
    return ret;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatEncryptFile(JNIEnv *env, jclass clazz, jlong controller, jstring from_path, jstring to_path) {
    const char *_from_path = encode_to_utf8_chars(env, from_path);
    const char *_to_path = encode_to_utf8_chars(env, to_path);
    jstring res = decode_to_utf8_string(env, chat_encrypt_file((void*)controller, _from_path, _to_path));
    (*env)->ReleaseStringUTFChars(env, from_path, _from_path);
    (*env)->ReleaseStringUTFChars(env, to_path, _to_path);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatDecryptFile(JNIEnv *env, jclass clazz, jstring from_path, jstring key, jstring nonce, jstring to_path) {
    const char *_from_path = encode_to_utf8_chars(env, from_path);
    const char *_key = encode_to_utf8_chars(env, key);
    const char *_nonce = encode_to_utf8_chars(env, nonce);
    const char *_to_path = encode_to_utf8_chars(env, to_path);
    jstring res = decode_to_utf8_string(env, chat_decrypt_file(_from_path, _key, _nonce, _to_path));
    (*env)->ReleaseStringUTFChars(env, from_path, _from_path);
    (*env)->ReleaseStringUTFChars(env, key, _key);
    (*env)->ReleaseStringUTFChars(env,  nonce, _nonce);
    (*env)->ReleaseStringUTFChars(env, to_path, _to_path);
    return res;
}
