#!/usr/bin/env node
import {homedir} from "os"
import {join, resolve} from "path"
import {Mode} from "./actions"
import {ExitError, log, logError} from "./log"
import {validName} from "./messages"
import {RegisterOptions, Scope, register} from "./register"
import {OwnerEntry, QrCodeSize, ServeOptions, serve} from "./serve"

export const helpText = `Usage:
  simplex-mcp serve NAME [options]     run agent NAME
  simplex-mcp register NAME [options]  add agent NAME to Claude Code

serve options:
  --data DIR             data folder (default: ~/.simplex-mcp/NAME)
  --display-name TEXT    the agent's name in SimpleX (default at the first start: NAME)
  --approve MODE         what the owner approves in the agent control chat:
                           requests  the agent's connections and messages (default)
                           host      nothing; the MCP host asks
  --owners [ID:NAME,...] only these owners approve; without a list, the link
                         adds an observer and its ID is printed at the join.
                         Without --owners, whoever joins becomes an owner and
                         every owner approves.
  --expire HOURS         pending actions expire after HOURS, 1 to 23 (default: 23)
  --reset-owners         replace the owners: a new agent control chat and link
  --new-token            replace the access token
  --port N               HTTP port on 127.0.0.1 (default: 5226)
  --qr-code SIZE         QR code of the link: small (default) or large

register options:
  --scope SCOPE          user (default) or local
  --data DIR             data folder (default: ~/.simplex-mcp/NAME)

--reset-owners takes --owners without a list.
`

export type Command =
  | {command: "help"}
  | {command: "serve"; options: ServeOptions}
  | {command: "register"; options: RegisterOptions}

export function parseCommand(args: string[], home: string = homedir()): Command {
  const [command, name, ...rest] = args
  if (command === "--help") return {command: "help"}
  if (command !== "serve" && command !== "register") throw usageError("Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.")
  if (name === undefined || !validAgentName(name)) throw usageError("NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.")
  const options = new OptionReader(rest)
  return command === "serve" ? {command, options: serveOptions(name, options, home)} : {command, options: registerOptions(name, options, home)}
}

function serveOptions(name: string, options: OptionReader, home: string): ServeOptions {
  const serve: ServeOptions = {
    name,
    data: dataFolder(name, home),
    approve: "requests",
    expireHours: 23,
    resetOwners: false,
    newToken: false,
    port: 5226,
    qrCode: "small",
    dataGiven: false,
  }
  for (let option = options.next(); option !== undefined; option = options.next()) {
    switch (option) {
      case "--data":
        serve.data = resolve(options.value(option))
        serve.dataGiven = true
        break
      case "--display-name":
        serve.displayName = displayName(options.value(option))
        break
      case "--approve":
        serve.approve = approveMode(options.value(option))
        break
      case "--owners":
        serve.owners = options.optionalValue() === undefined ? [] : options.value(option).split(/,(?=[1-9]\d*:)/).map(ownerEntry)
        break
      case "--expire":
        serve.expireHours = integer(option, options.value(option), 1, 23, "hours from 1 to 23")
        break
      case "--reset-owners":
        serve.resetOwners = true
        break
      case "--new-token":
        serve.newToken = true
        break
      case "--port":
        serve.port = integer(option, options.value(option), 1, 65535, "a port from 1 to 65535")
        break
      case "--qr-code":
        serve.qrCode = qrCodeSize(options.value(option))
        break
      default:
        throw unknownOption(option)
    }
  }
  if (serve.resetOwners && serve.owners !== undefined && serve.owners.length > 0) {
    throw usageError("--reset-owners takes --owners without a list: member IDs change in a new agent control chat.")
  }
  return serve
}

function registerOptions(name: string, options: OptionReader, home: string): RegisterOptions {
  const register: RegisterOptions = {name, data: dataFolder(name, home), scope: "user"}
  for (let option = options.next(); option !== undefined; option = options.next()) {
    switch (option) {
      case "--scope":
        register.scope = scope(options.value(option))
        break
      case "--data":
        register.data = resolve(options.value(option))
        break
      default:
        throw unknownOption(option)
    }
  }
  return register
}

class OptionReader {
  private index = 0

  constructor(private readonly args: string[]) {}

  next(): string | undefined {
    return this.args[this.index++]
  }

  value(option: string): string {
    const value = this.optionalValue()
    if (value === undefined) throw usageError(`${option}: a value expected.`)
    this.index++
    return value
  }

  optionalValue(): string | undefined {
    const value = this.args[this.index]
    return value === undefined || value.startsWith("--") ? undefined : value
  }
}

export function validAgentName(name: string): boolean {
  return name.length <= 32 && /^[a-z0-9](-?[a-z0-9])*-?$/.test(name)
}

function displayName(value: string): string {
  const valid = validName(value)
  if (valid !== value) throw usageError(`--display-name: not a valid SimpleX name; try ${valid}`)
  return value
}

function approveMode(value: string): Mode {
  if (value !== "requests" && value !== "host") throw usageError(`--approve: requests or host expected, got ${value}`)
  return value
}

function qrCodeSize(value: string): QrCodeSize {
  if (value !== "small" && value !== "large") throw usageError(`--qr-code: small or large expected, got ${value}`)
  return value
}

function scope(value: string): Scope {
  if (value !== "user" && value !== "local") throw usageError(`--scope: user or local expected, got ${value}`)
  return value
}

function ownerEntry(entry: string): OwnerEntry {
  const match = /^([1-9]\d*):(.+)$/.exec(entry)
  if (!match) throw usageError(`--owners: ID:NAME expected, got ${entry}`)
  return {memberId: Number(match[1]), name: match[2]}
}

function integer(option: string, value: string, min: number, max: number, expected: string): number {
  const n = Number(value)
  if (!/^\d+$/.test(value) || n < min || n > max) throw usageError(`${option}: ${expected} expected, got ${value}`)
  return n
}

function dataFolder(name: string, home: string): string {
  return join(home, ".simplex-mcp", name)
}

function unknownOption(option: string): ExitError {
  return usageError(`Unknown option ${option}. Run simplex-mcp --help.`)
}

function usageError(message: string): ExitError {
  return new ExitError(message, 2)
}

async function main(args: string[]): Promise<void> {
  const command = parseCommand(args)
  switch (command.command) {
    case "help":
      process.stdout.write(helpText)
      return
    case "serve":
      return serve(command.options)
    case "register":
      return log(register(command.options))
  }
}

if (require.main === module) {
  main(process.argv.slice(2)).catch(error => {
    if (error instanceof ExitError) {
      process.stderr.write(`${error.message}\n`)
      process.exit(error.code)
    }
    logError("simplex-mcp", error)
    process.exit(1)
  })
}
