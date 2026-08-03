#ifndef SIMPLEX_CORE_BRIDGE_H
#define SIMPLEX_CORE_BRIDGE_H

#include <stdbool.h>
#include <stddef.h>

bool sx_core_load(const char *library_directory, char *error_buffer, size_t error_buffer_size);
bool sx_core_initialize(char *error_buffer, size_t error_buffer_size);
const char *sx_core_migrate_init(const char *path, const char *key, const char *confirmation, void **controller);
const char *sx_core_send_cmd(void *controller, const char *command, int retry_count);
const char *sx_core_recv_msg_wait(void *controller, int timeout_microseconds);
const char *sx_core_encrypt_file(void *controller, const char *from_path, const char *to_path);
const char *sx_core_decrypt_file(const char *from_path, const char *key, const char *nonce, const char *to_path);
const char *sx_core_close_store(void *controller);
void sx_core_free(const char *value);
bool sx_try_lock_file(int file_descriptor);
void sx_unlock_file(int file_descriptor);

#endif
