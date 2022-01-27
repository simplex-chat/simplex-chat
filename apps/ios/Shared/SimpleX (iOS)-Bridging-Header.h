//
//  Use this file to import your target's public headers that you would like to expose to Swift.
//

extern void hs_init(int argc, char **argv[]);

typedef void* chat_store;
typedef void* chat_ctrl;

extern chat_store chat_init_store(char *path);
extern char *chat_get_user(chat_store store);
extern char *chat_create_user(chat_store store, char *data);
extern chat_ctrl chat_start(chat_store store);
extern char *chat_send_cmd(chat_ctrl ctl, char *cmd);
extern char *chat_recv_msg(chat_ctrl ctl);
