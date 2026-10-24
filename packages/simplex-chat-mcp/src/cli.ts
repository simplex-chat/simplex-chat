#!/usr/bin/env node
import {ExitError, logError} from "./log"

export const helpText = `Usage:
  simplex-mcp --help
`

async function main(args: string[]): Promise<void> {
  if (args[0] === "--help") {
    process.stdout.write(helpText)
    return
  }
  throw new ExitError("Run simplex-mcp --help.", 2)
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
