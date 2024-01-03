//
//  SimpleX.h
//  SimpleX
//
//  Created by Evgeny on 30/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

#ifndef SimpleX_h
#define SimpleX_h

#include "hs_init.h"

extern void hs_init(int argc, char **argv[]);

typedef void* chat_ctrl;

// the last parameter is used to return the pointer to chat controller
extern char *chat_migrate_init_key(char *path, char *key, int keepKey, char *confirm, int backgroundMode, chat_ctrl *ctrl);
extern char *chat_close_store(chat_ctrl ctl);
extern char *chat_reopen_store(chat_ctrl ctl);
extern char *chat_send_cmd(chat_ctrl ctl, char *cmd);
extern char *chat_recv_msg_wait(chat_ctrl ctl, int wait);
extern char *chat_parse_markdown(char *str);
extern char *chat_parse_server(char *str);
extern char *chat_password_hash(char *pwd, char *salt);
extern char *chat_valid_name(char *name);
extern char *chat_encrypt_media(chat_ctrl ctl, char *key, char *frame, int len);
extern char *chat_decrypt_media(char *key, char *frame, int len);

// chat_write_file returns null-terminated string with JSON of WriteFileResult
extern char *chat_write_file(chat_ctrl ctl, char *path, char *data, int len);

// chat_read_file returns a buffer with:
// result status (1 byte), then if
//   status == 0 (success): buffer length (uint32, 4 bytes), buffer of specified length.
//   status == 1 (error): null-terminated error message string.
extern char *chat_read_file(char *path, char *key, char *nonce);

// chat_encrypt_file returns null-terminated string with JSON of WriteFileResult
extern char *chat_encrypt_file(chat_ctrl ctl, char *fromPath, char *toPath);

// chat_decrypt_file returns null-terminated string with the error message
extern char *chat_decrypt_file(char *fromPath, char *key, char *nonce, char *toPath);

#endif /* SimpleX_h */
