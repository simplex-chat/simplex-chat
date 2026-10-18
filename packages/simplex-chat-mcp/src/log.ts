import {T} from "@simplex-chat/types"
import {api, core} from "simplex-chat"

const maxDetails = 500
const causeKey = /(Err|Error|errorType)$/

export class ExitError extends Error {
  constructor(message: string, readonly code: number) {
    super(message)
  }
}

export function shellWord(word: string, plain: RegExp): string {
  if (plain.test(word)) return word
  return process.platform === "win32" ? `"${word}"` : `'${word.replace(/'/g, "'\\''")}'`
}

export function shellPath(path: string): string {
  return shellWord(path, process.platform === "win32" ? /^[A-Za-z0-9_.:\\/-]+$/ : /^[A-Za-z0-9_./-]+$/)
}

export function oneLine(text: string): string {
  return text.replace(/[\p{Cc}\p{Cf}\p{Zl}\p{Zp}]+/gu, " ")
}

export function log(line: string): void {
  process.stdout.write(`${line}\n`)
}

export function logError(context: string, error: unknown): void {
  process.stderr.write(`${context}: ${errorText(error)}\n`)
}

export function errorText(error: unknown): string {
  if (error instanceof core.ChatAPIError && error.chatError) return chatErrorText(error.chatError)
  if (error instanceof api.ChatCommandError) return `SimpleX error: unexpected response ${error.response.type}`
  return error instanceof Error ? error.message : String(error)
}

export function chatErrorText(chatError: T.ChatError): string {
  const {types, details} = errorChain(chatError)
  const chain = types.join(": ")
  const text = JSON.stringify(details)
  return text === "{}" ? `SimpleX error: ${chain}` : `SimpleX error: ${chain}: ${text.slice(0, maxDetails)}`
}

function errorChain({type, ...fields}: {type: string}): {types: string[]; details: object} {
  const entries = Object.entries(fields)
  const cause = entries.find(([key, value]) => causeKey.test(key) && isTyped(value))?.[1] as {type: string} | undefined
  const below = cause === undefined ? {types: [], details: {}} : errorChain(cause)
  return {types: [type, ...below.types], details: {...Object.fromEntries(entries.filter(([, value]) => typeof value !== "object")), ...below.details}}
}

function isTyped(value: unknown): boolean {
  return typeof value === "object" && value !== null && typeof (value as {type?: unknown}).type === "string"
}

export function chatErrorOf(error: unknown): T.ChatError | undefined {
  return error instanceof core.ChatAPIError ? error.chatError : undefined
}
