#include <jni.h>
#include <string.h>
#include <stdint.h>
//#include <stdlib.h>
//#include <android/log.h>

// from the RTS
void hs_init_with_rtsopts(int * argc, char **argv[]);

// from android-support
void setLineBuffering(void);
int pipe_std_to_socket(const char * name);

extern void __svfscanf(void){};
extern void __vfwscanf(void){};
extern void __memset_chk_fail(void){};
extern void __strcpy_chk_generic(void){};
extern void __strcat_chk_generic(void){};
extern void __libc_globals(void){};
extern void __rel_iplt_start(void){};

// Android 9 only, not 13
extern void reallocarray(void){};

JNIEXPORT jint JNICALL
Java_chat_simplex_common_platform_CoreKt_pipeStdOutToSocket(JNIEnv *env, __unused jclass clazz, jstring socket_name) {
    const char *name = (*env)->GetStringUTFChars(env, socket_name, JNI_FALSE);
    int ret = pipe_std_to_socket(name);
    (*env)->ReleaseStringUTFChars(env, socket_name, name);
    return ret;
}

JNIEXPORT void JNICALL
Java_chat_simplex_common_platform_CoreKt_initHS(__unused JNIEnv *env, __unused jclass clazz) {
    int argc = 5;
    char *argv[] = {
        "simplex",
        "+RTS", // requires `hs_init_with_rtsopts`
        "-A16m", // chunk size for new allocations
        "-H64m", // initial heap size
        "-xn", // non-moving GC
        NULL
    };
    char **pargv = argv;
    hs_init_with_rtsopts(&argc, &pargv);
    setLineBuffering();
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

JNIEXPORT jobjectArray JNICALL
Java_chat_simplex_common_platform_CoreKt_chatMigrateInit(JNIEnv *env, __unused jclass clazz, jstring dbPath, jstring dbKey, jstring confirm) {
    const char *_dbPath = (*env)->GetStringUTFChars(env, dbPath, JNI_FALSE);
    const char *_dbKey = (*env)->GetStringUTFChars(env, dbKey, JNI_FALSE);
    const char *_confirm = (*env)->GetStringUTFChars(env, confirm, JNI_FALSE);
    jlong _ctrl = (jlong) 0;
    jstring res = (*env)->NewStringUTF(env, chat_migrate_init(_dbPath, _dbKey, _confirm, &_ctrl));
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
Java_chat_simplex_common_platform_CoreKt_chatSendCmd(JNIEnv *env, __unused jclass clazz, jlong controller, jstring msg) {
    const char *_msg = (*env)->GetStringUTFChars(env, msg, JNI_FALSE);
    //jint length = (jint) (*env)->GetStringUTFLength(env, msg);
    //for (int i = 0; i < length; ++i)
    //    __android_log_print(ANDROID_LOG_ERROR, "simplex", "%d: %02x\n", i, _msg[i]);
    jstring res = (*env)->NewStringUTF(env, chat_send_cmd((void*)controller, _msg));
    (*env)->ReleaseStringUTFChars(env, msg, _msg);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatSendRemoteCmd(JNIEnv *env, __unused jclass clazz, jlong controller, jint rhId, jstring msg) {
    const char *_msg = (*env)->GetStringUTFChars(env, msg, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_send_remote_cmd((void*)controller, rhId, _msg));
    (*env)->ReleaseStringUTFChars(env, msg, _msg);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatRecvMsg(JNIEnv *env, __unused jclass clazz, jlong controller) {
    return (*env)->NewStringUTF(env, chat_recv_msg((void*)controller));
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatRecvMsgWait(JNIEnv *env, __unused jclass clazz, jlong controller, jint wait) {
    return (*env)->NewStringUTF(env, chat_recv_msg_wait((void*)controller, wait));
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatParseMarkdown(JNIEnv *env, __unused jclass clazz, jstring str) {
    const char *_str = (*env)->GetStringUTFChars(env, str, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_parse_markdown(_str));
    (*env)->ReleaseStringUTFChars(env, str, _str);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatParseServer(JNIEnv *env, __unused jclass clazz, jstring str) {
    const char *_str = (*env)->GetStringUTFChars(env, str, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_parse_server(_str));
    (*env)->ReleaseStringUTFChars(env, str, _str);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatPasswordHash(JNIEnv *env, __unused jclass clazz, jstring pwd, jstring salt) {
    const char *_pwd = (*env)->GetStringUTFChars(env, pwd, JNI_FALSE);
    const char *_salt = (*env)->GetStringUTFChars(env, salt, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_password_hash(_pwd, _salt));
    (*env)->ReleaseStringUTFChars(env, pwd, _pwd);
    (*env)->ReleaseStringUTFChars(env, salt, _salt);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatValidName(JNIEnv *env, jclass clazz, jstring name) {
    const char *_name = (*env)->GetStringUTFChars(env, name, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_valid_name(_name));
    (*env)->ReleaseStringUTFChars(env, name, _name);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatWriteFile(JNIEnv *env, jclass clazz, jlong controller, jstring path, jobject buffer) {
    const char *_path = (*env)->GetStringUTFChars(env, path, JNI_FALSE);
    jbyte *buff = (jbyte *) (*env)->GetDirectBufferAddress(env, buffer);
    jlong capacity = (*env)->GetDirectBufferCapacity(env, buffer);
    jstring res = (*env)->NewStringUTF(env, chat_write_file((void*)controller, _path, buff, capacity));
    (*env)->ReleaseStringUTFChars(env, path, _path);
    return res;
}

JNIEXPORT jobjectArray JNICALL
Java_chat_simplex_common_platform_CoreKt_chatReadFile(JNIEnv *env, jclass clazz, jstring path, jstring key, jstring nonce) {
    const char *_path = (*env)->GetStringUTFChars(env, path, JNI_FALSE);
    const char *_key = (*env)->GetStringUTFChars(env, key, JNI_FALSE);
    const char *_nonce = (*env)->GetStringUTFChars(env, nonce, JNI_FALSE);

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
    const char *_from_path = (*env)->GetStringUTFChars(env, from_path, JNI_FALSE);
    const char *_to_path = (*env)->GetStringUTFChars(env, to_path, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_encrypt_file((void*)controller, _from_path, _to_path));
    (*env)->ReleaseStringUTFChars(env, from_path, _from_path);
    (*env)->ReleaseStringUTFChars(env, to_path, _to_path);
    return res;
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_CoreKt_chatDecryptFile(JNIEnv *env, jclass clazz, jstring from_path, jstring key, jstring nonce, jstring to_path) {
    const char *_from_path = (*env)->GetStringUTFChars(env, from_path, JNI_FALSE);
    const char *_key = (*env)->GetStringUTFChars(env, key, JNI_FALSE);
    const char *_nonce = (*env)->GetStringUTFChars(env, nonce, JNI_FALSE);
    const char *_to_path = (*env)->GetStringUTFChars(env, to_path, JNI_FALSE);
    jstring res = (*env)->NewStringUTF(env, chat_decrypt_file(_from_path, _key, _nonce, _to_path));
    (*env)->ReleaseStringUTFChars(env, from_path, _from_path);
    (*env)->ReleaseStringUTFChars(env, key, _key);
    (*env)->ReleaseStringUTFChars(env,  nonce, _nonce);
    (*env)->ReleaseStringUTFChars(env, to_path, _to_path);
    return res;
}
