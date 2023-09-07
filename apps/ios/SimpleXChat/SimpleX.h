//
//  SimpleX.h
//  SimpleX
//
//  Created by Evgeny on 30/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

#ifndef SimpleX_h
#define SimpleX_h

#endif /* SimpleX_h */

extern void hs_init(int argc, char **argv[]);

typedef void* chat_ctrl;

// the last parameter is used to return the pointer to chat controller
extern char *chat_migrate_init(char *path, char *key, char *confirm, chat_ctrl *ctrl);
extern char *chat_send_cmd(chat_ctrl ctl, char *cmd);
extern char *chat_recv_msg(chat_ctrl ctl);
extern char *chat_recv_msg_wait(chat_ctrl ctl, int wait);
extern char *chat_parse_markdown(char *str);
extern char *chat_parse_server(char *str);
extern char *chat_password_hash(char *pwd, char *salt);
extern char *chat_encrypt_media(char *key, char *frame, int len);
extern char *chat_decrypt_media(char *key, char *frame, int len);

// chat_write_file returns NUL-terminated string with JSON of WriteFileResult
extern char *chat_write_file(char *path, char *data, int len);

// chat_read_file returns a buffer with:
// 1. NUL-terminated C string with JSON of ReadFileResult, followed by
// 2. file data, the length is defined in ReadFileResult
extern char *chat_read_file(char *path, char *key, char *nonce);

// chat_encrypt_file returns NUL-terminated string with JSON of WriteFileResult
extern char *chat_encrypt_file(char *fromPath, char *toPath);

// chat_decrypt_file returns NUL-terminated string with the error message
extern char *chat_decrypt_file(char *fromPath, char *key, char *nonce, char *toPath);
