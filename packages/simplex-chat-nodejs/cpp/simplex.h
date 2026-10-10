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

namespace simplex {

inline void (*hs_init_with_rtsopts)(int * argc, char **argv[]);
inline void (*hs_thread_done)(void);

// the last parameter is used to return the pointer to chat controller
inline char *(*chat_migrate_init)(const char *path, const char *key, const char *confirm, chat_ctrl *ctrl);
inline char *(*chat_migrate_init_queue)(const char *path, const char *key, const char *confirm, const int queueSize, chat_ctrl *ctrl);
inline char *(*chat_close_store)(chat_ctrl ctrl);
inline char *(*chat_send_cmd)(chat_ctrl ctrl, const char *cmd);
inline char *(*chat_recv_msg_wait)(chat_ctrl ctrl, const int wait);

// chat_write_file returns null-terminated string with JSON of WriteFileResult
inline char *(*chat_write_file)(chat_ctrl ctrl, const char *path, const char *data, const int len);

// chat_read_file returns a buffer with:
// result status (1 byte), then if
//   status == 0 (success): buffer length (uint32, 4 bytes), buffer of specified length.
//   status == 1 (error): null-terminated error message string.
inline char *(*chat_read_file)(const char *path, const char *key, const char *nonce);

// chat_encrypt_file returns null-terminated string with JSON of WriteFileResult
inline char *(*chat_encrypt_file)(chat_ctrl ctrl, const char *fromPath, const char *toPath);

// chat_decrypt_file returns null-terminated string with the error message
inline char *(*chat_decrypt_file)(const char *fromPath, const char *key, const char *nonce, const char *toPath);

}

#endif /* simplex_h */