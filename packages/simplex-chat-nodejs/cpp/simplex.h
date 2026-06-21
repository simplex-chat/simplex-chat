//
//  simplex.h
//  SimpleX
//
//  Created by Evgeny on 30/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

#ifndef SimpleX_h
#define SimpleX_h

extern "C" void hs_init(int argc, char **argv[]);
extern "C" void hs_init_with_rtsopts(int * argc, char **argv[]);

typedef long* chat_ctrl;

// the last parameter is used to return the pointer to chat controller
extern "C" char *chat_migrate_init(const char *path, const char *key, const char *confirm, chat_ctrl *ctrl);
extern "C" char *chat_close_store(chat_ctrl ctrl);
extern "C" char *chat_reopen_store(chat_ctrl ctrl);
extern "C" char *chat_send_cmd(chat_ctrl ctrl, const char *cmd);
extern "C" char *chat_recv_msg_wait(chat_ctrl ctrl, const int wait);
extern "C" char *chat_parse_markdown(const char *str);
extern "C" char *chat_parse_server(const char *str);
extern "C" char *chat_password_hash(const char *pwd, const char *salt);
extern "C" char *chat_valid_name(const char *name);
extern "C" int chat_json_length(const char *str);
extern "C" char *chat_encrypt_media(chat_ctrl ctrl, const char *key, const char *frame, const int len);
extern "C" char *chat_decrypt_media(const char *key, const char *frame, const int len);

// chat_write_file returns null-terminated string with JSON of WriteFileResult
extern "C" char *chat_write_file(chat_ctrl ctrl, const char *path, const char *data, const int len);

// chat_read_file returns a buffer with:
// result status (1 byte), then if
//   status == 0 (success): buffer length (uint32, 4 bytes), buffer of specified length.
//   status == 1 (error): null-terminated error message string.
extern "C" char *chat_read_file(const char *path, const char *key, const char *nonce);

// chat_encrypt_file returns null-terminated string with JSON of WriteFileResult
extern "C" char *chat_encrypt_file(chat_ctrl ctrl, const char *fromPath, const char *toPath);

// chat_decrypt_file returns null-terminated string with the error message
extern "C" char *chat_decrypt_file(const char *fromPath, const char *key, const char *nonce, const char *toPath);

#endif /* simplex_h */