//
//  simplex.h
//  SimpleX
//
//  Created by Evgeny on 30/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

#ifndef SimpleX_h
#define SimpleX_h

typedef long* chat_ctrl;

typedef void (*hs_init_with_rtsopts_fn)(int *argc, char **argv[]);
typedef void (*hs_thread_done_fn)(void);
// the last parameter is used to return the pointer to chat controller
typedef char *(*chat_migrate_init_fn)(const char *path, const char *key, const char *confirm, chat_ctrl *ctrl);
typedef char *(*chat_migrate_init_queue_fn)(const char *path, const char *key, const char *confirm, int queueSize, chat_ctrl *ctrl);
typedef char *(*chat_close_store_fn)(chat_ctrl ctrl);
typedef char *(*chat_send_cmd_fn)(chat_ctrl ctrl, const char *cmd);
typedef char *(*chat_recv_msg_wait_fn)(chat_ctrl ctrl, int wait);

// chat_write_file returns null-terminated string with JSON of WriteFileResult
typedef char *(*chat_write_file_fn)(chat_ctrl ctrl, const char *path, const char *data, int len);

// chat_read_file returns a buffer with:
// result status (1 byte), then if
//   status == 0 (success): buffer length (uint32, 4 bytes), buffer of specified length.
//   status == 1 (error): null-terminated error message string.
typedef char *(*chat_read_file_fn)(const char *path, const char *key, const char *nonce);

// chat_encrypt_file returns null-terminated string with JSON of WriteFileResult
typedef char *(*chat_encrypt_file_fn)(chat_ctrl ctrl, const char *fromPath, const char *toPath);

// chat_decrypt_file returns null-terminated string with the error message
typedef char *(*chat_decrypt_file_fn)(const char *fromPath, const char *key, const char *nonce, const char *toPath);

#endif /* simplex_h */