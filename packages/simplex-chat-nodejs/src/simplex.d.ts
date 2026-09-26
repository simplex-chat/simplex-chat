// These functions are defined in CPP add-on ../cpp/simplex.cc

export function load(libPath: string): void
export function chat_migrate_init(dbPath: string, dbKey: string, confirm: string): Promise<[bigint, string]>
export function chat_migrate_init_queue(dbPath: string, dbKey: string, confirm: string, queueSize: number): Promise<[bigint, string]>
export function chat_close_store(ctrl: bigint): Promise<string>
export function chat_send_cmd(ctrl: bigint, cmd: string): Promise<string>
export function chat_recv_msg_wait(ctrl: bigint, wait: number): Promise<string>
export function chat_write_file(ctrl: bigint, path: string, buffer: ArrayBuffer | Uint8Array): Promise<string>
export function chat_read_file(path: string, key: string, nonce: string): Promise<Buffer>
export function chat_encrypt_file(ctrl: bigint, fromPath: string, toPath: string): Promise<string>
export function chat_decrypt_file(fromPath: string, key: string, nonce: string, toPath: string): Promise<string>
