#include "CoreBridge.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef void (*hs_init_with_rtsopts_fn)(int *, char ***);
typedef char *(*chat_migrate_init_fn)(const char *, const char *, const char *, void **);
typedef char *(*chat_send_cmd_retry_fn)(void *, const char *, int);
typedef char *(*chat_recv_msg_wait_fn)(void *, int);
typedef char *(*chat_encrypt_file_fn)(void *, const char *, const char *);
typedef char *(*chat_decrypt_file_fn)(const char *, const char *, const char *, const char *);
typedef char *(*chat_close_store_fn)(void *);

static void *simplex_handle = NULL;
static hs_init_with_rtsopts_fn hs_initialize = NULL;
static chat_migrate_init_fn migrate_init = NULL;
static chat_send_cmd_retry_fn send_cmd_retry = NULL;
static chat_recv_msg_wait_fn recv_msg_wait = NULL;
static chat_encrypt_file_fn encrypt_file = NULL;
static chat_decrypt_file_fn decrypt_file = NULL;
static chat_close_store_fn close_store = NULL;
static bool runtime_initialized = false;

static void set_error(char *buffer, size_t size, const char *message) {
    if (buffer == NULL || size == 0) return;
    snprintf(buffer, size, "%s", message == NULL ? "Unknown SimpleX core error" : message);
}

bool sx_core_load(const char *library_directory, char *error_buffer, size_t error_buffer_size) {
    if (simplex_handle != NULL) return true;
    if (library_directory == NULL || library_directory[0] == '\0') {
        set_error(error_buffer, error_buffer_size, "SimpleX core library directory is missing");
        return false;
    }

    char library_path[4096];
    snprintf(library_path, sizeof(library_path), "%s/libsimplex.dylib", library_directory);
    simplex_handle = dlopen(library_path, RTLD_NOW | RTLD_GLOBAL);
    if (simplex_handle == NULL) {
        set_error(error_buffer, error_buffer_size, dlerror());
        return false;
    }

    hs_initialize = (hs_init_with_rtsopts_fn)dlsym(RTLD_DEFAULT, "hs_init_with_rtsopts");
    migrate_init = (chat_migrate_init_fn)dlsym(simplex_handle, "chat_migrate_init");
    send_cmd_retry = (chat_send_cmd_retry_fn)dlsym(simplex_handle, "chat_send_cmd_retry");
    recv_msg_wait = (chat_recv_msg_wait_fn)dlsym(simplex_handle, "chat_recv_msg_wait");
    encrypt_file = (chat_encrypt_file_fn)dlsym(simplex_handle, "chat_encrypt_file");
    decrypt_file = (chat_decrypt_file_fn)dlsym(simplex_handle, "chat_decrypt_file");
    close_store = (chat_close_store_fn)dlsym(simplex_handle, "chat_close_store");

    if (hs_initialize == NULL || migrate_init == NULL || send_cmd_retry == NULL || recv_msg_wait == NULL || encrypt_file == NULL || decrypt_file == NULL || close_store == NULL) {
        set_error(error_buffer, error_buffer_size, "The SimpleX core is missing a required exported function");
        dlclose(simplex_handle);
        simplex_handle = NULL;
        return false;
    }
    return true;
}

bool sx_core_initialize(char *error_buffer, size_t error_buffer_size) {
    if (runtime_initialized) return true;
    if (hs_initialize == NULL) {
        set_error(error_buffer, error_buffer_size, "The SimpleX core must be loaded before initialization");
        return false;
    }

    int argc = 5;
    char *argv[] = {"simplex", "+RTS", "-A64m", "-H64m", "-xn", NULL};
    char **arguments = argv;
    hs_initialize(&argc, &arguments);
    runtime_initialized = true;
    return true;
}

const char *sx_core_migrate_init(const char *path, const char *key, const char *confirmation, void **controller) {
    return migrate_init == NULL ? NULL : migrate_init(path, key, confirmation, controller);
}

const char *sx_core_send_cmd(void *controller, const char *command, int retry_count) {
    return send_cmd_retry == NULL ? NULL : send_cmd_retry(controller, command, retry_count);
}

const char *sx_core_recv_msg_wait(void *controller, int timeout_microseconds) {
    return recv_msg_wait == NULL ? NULL : recv_msg_wait(controller, timeout_microseconds);
}

const char *sx_core_encrypt_file(void *controller, const char *from_path, const char *to_path) {
    return encrypt_file == NULL ? NULL : encrypt_file(controller, from_path, to_path);
}

const char *sx_core_decrypt_file(const char *from_path, const char *key, const char *nonce, const char *to_path) {
    return decrypt_file == NULL ? NULL : decrypt_file(from_path, key, nonce, to_path);
}

const char *sx_core_close_store(void *controller) {
    return close_store == NULL ? NULL : close_store(controller);
}

void sx_core_free(const char *value) {
    free((void *)value);
}
