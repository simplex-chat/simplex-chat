import {ChatEvent, ChatResponse, T} from "@simplex-chat/types"
import * as simplex from "./simplex"

// initialize chat controller
export async function chatMigrateInit(dbPath: string, dbKey: string, confirm: MigrationConfirmation): Promise<bigint> {
  const [ctrl, res] = await simplex.chat_migrate_init(dbPath, dbKey, confirm)
  const json = JSON.parse(res)
  if (json.type === 'ok') return ctrl
  throw new ChatInitError("Database or migration error (see dbMigrationError property)", json as DBMigrationError)
}

// close chat store
export async function chatCloseStore(ctrl: bigint): Promise<void> {
  const res = await simplex.chat_close_store(ctrl)
  if (res !== "") throw new Error(res)
}

// send chat command as string
export async function chatSendCmd(ctrl: bigint, cmd: string): Promise<ChatResponse> {
  const res = await simplex.chat_send_cmd(ctrl, cmd)
  const json = JSON.parse(res) as APIResult<ChatResponse>
  // console.log(cmd.slice(0, 16), json.result?.type || json.error)
  if (typeof json.result === 'object') return json.result
  if (typeof json.error === 'object') throw new ChatAPIError("Chat command error (see chatError property)", json.error as T.ChatError)
  throw new ChatAPIError("Invalid chat command result")
}

// receive chat event
export async function chatRecvMsgWait(ctrl: bigint, wait: number): Promise<ChatEvent | undefined> {
  const res = await simplex.chat_recv_msg_wait(ctrl, wait)
  if (res === "") return undefined
  const json = JSON.parse(res) as APIResult<ChatEvent>
  // if (json.result) console.log("event", json.result.type)
  if (typeof json.result === 'object') return json.result
  if (typeof json.error === 'object') throw new ChatAPIError("Chat event error (see chatError property)", json.error as T.ChatError)
  throw new ChatAPIError("Invalid chat event")  
}

// write buffer to encrypted file
export async function chatWriteFile(ctrl: bigint, path: string, buffer: ArrayBuffer): Promise<CryptoArgs> {
  const res = await simplex.chat_write_file(ctrl, path, buffer)
  return cryptoArgsResult(res)
}

// read buffer from encrypted file
export async function chatReadFile(path: string, {fileKey, fileNonce}: CryptoArgs): Promise<ArrayBuffer> {
  return await simplex.chat_read_file(path, fileKey, fileNonce)
}

// encrypt file
export async function chatEncryptFile(ctrl: bigint, fromPath: string, toPath: string): Promise<CryptoArgs> {
  const res = await simplex.chat_encrypt_file(ctrl, fromPath, toPath)
  return cryptoArgsResult(res)
}

// decrypt file
export async function chatDecryptFile(fromPath: string, {fileKey, fileNonce}: CryptoArgs, toPath: string): Promise<void> {
  const res = await simplex.chat_decrypt_file(fromPath, fileKey, fileNonce, toPath)
  if (res !== "") throw new Error(res)
}

function cryptoArgsResult(res: string): CryptoArgs {
  const json = JSON.parse(res)
  switch (json.type) {
    case "result": return json.cryptoArgs as CryptoArgs
    case "error": throw Error(json.writeError)
    default: throw Error("unexpected chat_write_file result: " + res)
  }  
}

export interface APIResult<R> {
  result?: R
  error?: T.ChatError
}

export class ChatAPIError extends Error {
  constructor(public message: string, public chatError: T.ChatError | undefined = undefined) {
    super(message)
  }
}

export enum MigrationConfirmation {
  YesUp = "yesUp",
  YesUpDown = "yesUpDown",
  Console = "console",
  Error = "error"
}

export interface CryptoArgs {
  fileKey: string
  fileNonce: string
}

export class ChatInitError extends Error {
  constructor(public message: string, public dbMigrationError: DBMigrationError) {
    super(message)
  }
}

export type DBMigrationError = 
  | DBMigrationError.InvalidConfirmation
  | DBMigrationError.ErrorNotADatabase // invalid/corrupt database file or incorrect encryption key
  | DBMigrationError.ErrorMigration
  | DBMigrationError.ErrorSQL

export namespace DBMigrationError {
  export type Tag = "invalidConfirmation" | "errorNotADatabase" | "errorMigration" | "errorSQL"

  interface Interface {
    type: Tag
  }

  export interface InvalidConfirmation extends Interface {
    type: "invalidConfirmation"
  }

  export interface ErrorNotADatabase extends Interface {
    type: "errorNotADatabase"
    dbFile: string
  }

  export interface ErrorMigration extends Interface {
    type: "errorMigration"
    dbFile: string
    migrationError: MigrationError
  }

  export interface ErrorSQL extends Interface {
    type: "errorSQL"
    dbFile: string
    migrationSQLError: string
  }
}

export type MigrationError =
  | MigrationError.MEUpgrade
  | MigrationError.MEDowngrade
  | MigrationError.MigrationError

export namespace MigrationError {
  export type Tag = "upgrade" | "downgrade" | "migrationError"

  interface Interface {
    type: Tag
  }

  export interface MEUpgrade extends Interface {
    type: "upgrade"
    upMigrations: UpMigration
  }

  export interface MEDowngrade extends Interface {
    type: "downgrade"
    downMigrations: string[]
  }

  export interface MigrationError extends Interface {
    type: "migrationError"
    mtrError: MTRError
  }  
}

export interface UpMigration {
  upName: string
  withDown: boolean
}

export type MTRError =
  | MTRError.MTRENoDown
  | MTRError.MTREDifferent

export namespace MTRError {
  export type Tag = "noDown" | "different"

  interface Interface {
    type: Tag
  }

  export interface MTRENoDown extends Interface {
    type: "noDown"
    upMigrations: UpMigration
  }

  export interface MTREDifferent extends Interface {
    type: "different"
    downMigrations: string[]
  }
}
