import {ChatEvent, ChatResponse} from "@simplex-chat/types"
import {CryptoArgs, MigrationConfirmation} from "./types"

// initialize chat controller
export function chatMigrateInit(dbPath: string, dbKey: string, confirm: MigrationConfirmation): Promise<bigint>

// close chat store
export function chatCloseStore(ctrl: bigint): Promise<void>

// send chat command as string
export function chatSendCmd(ctrl: bigint, cmd: string): Promise<ChatResponse>

// receive chat event
export function chatRecvMsgWait(ctrl: bigint, wait: number): Promise<ChatEvent | undefined>

// write buffer to encrypted file
export function chatWriteFile(ctrl: bigint, path: string, buffer: ArrayBuffer): Promise<CryptoArgs>

// read buffer from encrypted file
export function chatReadFile(path: string, key: string, nonce: string): Promise<ArrayBuffer>

// encrypt file
export function chatEncryptFile(ctrl: bigint, fromPath: string, toPath: string): Promise<CryptoArgs>

// decrypt file
export function chatDecryptFile(fromPath: string, key: string, nonce: string, toPath: string): Promise<void>
