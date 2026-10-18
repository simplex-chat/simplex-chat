import {T} from "@simplex-chat/types"
import {api, core} from "simplex-chat"
import {fullDomainName} from "./messages"

const maxDetails = 500
const causeKey = /(Err|Error|errorType)$/
const blockingReasons: {[reason in T.BlockingReason]: string} = {
  [T.BlockingReason.Spam]: "Spam",
  [T.BlockingReason.Content]: "Content violates conditions of use",
}

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
  return oneLine(appErrorText(chatError) ?? chainText(chatError))
}

function appErrorText(chatError: T.ChatError): string | undefined {
  switch (chatError.type) {
    case "error":
      return chatErrorTypeText(chatError.errorType)
    case "errorAgent":
      return agentErrorText(chatError.agentError)
    case "errorStore":
      return storeErrorText(chatError.storeError)
  }
}

function chatErrorTypeText(error: T.ChatErrorType): string | undefined {
  switch (error.type) {
    case "invalidConnReq":
      return "Invalid connection link: Please check that you used the correct link or ask your contact to send you another one."
    case "unsupportedConnReq":
      return "Unsupported connection link: This link requires a newer app version. Please upgrade the app or ask your contact to send a compatible link."
    case "simplexDomainNotReady": {
      const name = fullDomainName(error.simplexDomain)
      return error.simplexDomainError.type === "noValidLink"
        ? `No valid link: The SimpleX name ${name} is registered, but it has no valid link.`
        : `Unconfirmed name: The SimpleX name ${name} is registered, but not added to profile. Please add it to your address or channel profile, if you are the owner.`
    }
    default:
      return undefined
  }
}

function agentErrorText(error: T.AgentErrorType): string | undefined {
  switch (error.type) {
    case "NO_NAME_SERVERS":
      return "SimpleX name error: None of your servers are set to resolve SimpleX names. Use a connection link."
    case "SMP":
      return smpErrorText(error.smpErr, error.serverAddress)
    case "BROKER":
      return brokerErrorText(error.brokerErr, serverHostname(error.brokerAddress))
    case "PROXY":
      return error.proxyErr.type === "protocolError" && error.proxyErr.protocolErr.type === "PROXY"
        ? destinationErrorText(error.proxyErr.protocolErr.proxyErr, serverHostname(error.proxyServer), serverHostname(error.relayServer))
        : undefined
    case "AGENT":
      return error.agentErr.type === "A_SERVICE" && error.agentErr.serviceError.type === "timeout" ? "Connection timeout: The service did not respond. Please try again." : undefined
    case "INTERNAL":
      return error.internalErr === "SEUniqueID" ? "Already connected? It seems like you are already connected via this link. If it is not the case, there was an error (SEUniqueID)." : undefined
    default:
      return undefined
  }
}

function smpErrorText(error: T.ErrorType, server: string): string | undefined {
  switch (error.type) {
    case "NAME":
      return nameErrorText(error.nameErr, server)
    case "AUTH":
      return "Connection link removed: Your contact removed this link, or it was a one-time link that was already used. To connect, ask your contact to create a new link."
    case "BLOCKED":
      return `Connection blocked: Connection is blocked by server operator: ${blockingReasons[error.blockInfo.reason]}.`
    case "QUOTA":
      return "Undelivered messages: The connection reached the limit of undelivered messages, your contact may be offline."
    case "PROXY":
      return error.proxyErr.type === "BROKER" ? forwardingErrorText(error.proxyErr.brokerErr, serverHostname(server)) : undefined
    default:
      return undefined
  }
}

function nameErrorText(error: T.NameErrorType, server: string): string {
  switch (error.type) {
    case "NOT_FOUND":
      return "Name not found: This SimpleX name is not registered. Please check the name."
    case "NO_RESOLVER":
      return `SimpleX name error: Server ${server} does not support name resolution. Use a connection link.`
    case "RESOLVER":
      return `SimpleX name error: Resolver error: ${error.resolverErr}`
  }
}

function brokerErrorText(error: T.BrokerErrorType, host: string): string | undefined {
  switch (error.type) {
    case "TIMEOUT":
      return `Connection timeout: Please check your network connection with ${host} and try again.`
    case "NETWORK":
      return error.networkError.type === "unknownCAError"
        ? `Connection error: Fingerprint in server address does not match certificate: ${host}.`
        : `Connection error: Please check your network connection with ${host} and try again.`
    case "HOST":
      return `Connection error: Server address is incompatible with network settings: ${host}.`
    case "TRANSPORT":
      return error.transportErr.type === "version" ? `Connection error: Server version is incompatible with your app: ${host}.` : undefined
    default:
      return undefined
  }
}

function forwardingErrorText(error: T.BrokerErrorType, host: string): string | undefined {
  switch (error.type) {
    case "TIMEOUT":
      return `Private routing error: Error connecting to forwarding server ${host}. Please try later.`
    case "NETWORK":
      return error.networkError.type === "unknownCAError"
        ? `Private routing error: Fingerprint in forwarding server address does not match certificate: ${host}.`
        : `Private routing error: Error connecting to forwarding server ${host}. Please try later.`
    case "HOST":
      return `Private routing error: Forwarding server address is incompatible with network settings: ${host}.`
    case "TRANSPORT":
      return error.transportErr.type === "version" ? `Private routing error: Forwarding server version is incompatible with network settings: ${host}.` : undefined
    default:
      return undefined
  }
}

function destinationErrorText(error: T.ProxyError, proxy: string, relay: string): string | undefined {
  const failed = `Private routing error: Forwarding server ${proxy} failed to connect to destination server ${relay}. Please try later.`
  if (error.type === "NO_SESSION") return failed
  if (error.type !== "BROKER") return undefined
  switch (error.brokerErr.type) {
    case "TIMEOUT":
      return failed
    case "NETWORK":
      return error.brokerErr.networkError.type === "unknownCAError" ? `Private routing error: Fingerprint in destination server address does not match certificate: ${relay}.` : failed
    case "HOST":
      return `Private routing error: Destination server address of ${relay} is incompatible with forwarding server ${proxy} settings.`
    case "TRANSPORT":
      return error.brokerErr.transportErr.type === "version" ? `Private routing error: Destination server version of ${relay} is incompatible with forwarding server ${proxy}.` : undefined
    default:
      return undefined
  }
}

function storeErrorText(error: T.StoreError): string | undefined {
  switch (error.type) {
    case "largeMsg":
      return "Message is too large! Please reduce the message size and send again."
    case "groupNotFound":
      return "Group not found! This group no longer exists."
    default:
      return undefined
  }
}

function serverHostname(server: string): string {
  return /^[a-z]+:\/\/[^@]*@([^,:/]+)/.exec(server)?.[1] ?? server
}

function chainText(chatError: T.ChatError): string {
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
