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
extern char *chat_migrate_init(char *path, char *key, chat_ctrl *ctrl);
extern char *chat_send_cmd(chat_ctrl ctl, char *cmd);
extern char *chat_recv_msg(chat_ctrl ctl);
extern char *chat_recv_msg_wait(chat_ctrl ctl, int wait);
extern char *chat_parse_markdown(char *str);
extern char *chat_parse_server(char *str);
