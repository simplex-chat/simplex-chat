//
//  Use this file to import your target's public headers that you would like to expose to Swift.
//

extern void hs_init(int argc, char **argv[]);

typedef void* chat_ctrl;

extern chat_ctrl chat_init(char *path);
extern char *chat_send_cmd(chat_ctrl ctl, char *cmd);
extern char *chat_recv_msg(chat_ctrl ctl);
