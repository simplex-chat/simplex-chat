import {T} from "@simplex-chat/types"
import {api, core} from "simplex-chat"

const maxDetails = 500

export class ExitError extends Error {
  constructor(message: string, readonly code: number) {
    super(message)
  }
}

export function shellWord(word: string, plain: RegExp): string {
  if (plain.test(word)) return word
  return process.platform === "win32" ? `"${word}"` : `'${word.replace(/'/g, "'\\''")}'`
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
  const {type, ...details} = chatErrorDetail(chatError)
  const text = JSON.stringify(details)
  return text === "{}" ? `SimpleX error: ${type}` : `SimpleX error: ${type}: ${text.slice(0, maxDetails)}`
}

function chatErrorDetail(chatError: T.ChatError): {type: string} {
  switch (chatError.type) {
    case "error":
      return chatError.errorType
    case "errorAgent":
      return chatError.agentError
    case "errorStore":
      return chatError.storeError
  }
}

export function chatErrorOf(error: unknown): T.ChatError | undefined {
  return error instanceof core.ChatAPIError ? error.chatError : undefined
}
