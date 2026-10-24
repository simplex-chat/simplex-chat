#!/usr/bin/env node
import {runDaemon} from "./daemon"
import {sessionStart} from "./hook"
import {ExitError, logError} from "./log"
import {resetOwners, showLink} from "./owner"
import {startSession} from "./session"

export const helpText = `Usage:
  simplex-mcp session               MCP server of one Claude Code session (run by the plugin)
  simplex-mcp daemon                the shared SimpleX daemon (started on demand)
  simplex-mcp hook session-start    Claude Code SessionStart hook (run by the plugin)
  simplex-mcp link                  show the Main group link, or its owners
  simplex-mcp reset-owners          replace the Main group and its owners

Install in Claude Code:
  /plugin marketplace add https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/.claude-plugin/marketplace.json
  /plugin install simplex@simplex-chat

Environment:
  SIMPLEX_MCP_HOME   data folder (default: ~/.simplex-mcp)
`

export type Command = "session" | "daemon" | "hook session-start" | "link" | "reset-owners" | "help"

const commands: {[args: string]: Command} = {
  "session": "session",
  "daemon": "daemon",
  "hook session-start": "hook session-start",
  "link": "link",
  "reset-owners": "reset-owners",
  "--help": "help",
}

export function parseCommand(args: string[]): Command {
  const command = commands[args.join(" ")]
  if (command === undefined) throw new ExitError("Unknown command. Run simplex-mcp --help.", 2)
  return command
}

async function main(args: string[]): Promise<void> {
  switch (parseCommand(args)) {
    case "help":
      process.stdout.write(helpText)
      return
    case "session":
      return startSession()
    case "daemon":
      return runDaemon()
    case "hook session-start":
      process.stdout.write(`${JSON.stringify(await sessionStart(await readStdin()))}\n`)
      return
    case "link":
      process.stdout.write(`${await showLink()}\n`)
      return
    case "reset-owners":
      process.stdout.write(`${await resetOwners()}\n`)
      return
  }
}

async function readStdin(): Promise<string> {
  let text = ""
  for await (const chunk of process.stdin) text += chunk
  return text
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
