#!/usr/bin/env node
import {parseArgs} from "util"
import type {Backend} from "./core"
import {resolveLibsDir} from "./libs"

const USAGE = "usage: simplex-chat install [--backend sqlite|postgres]"
const EXIT_INSTALL_FAILED = 1
const EXIT_USAGE = 2

export function parseInstallArgs(argv: string[]): Backend {
  const {positionals, values: {backend = "sqlite"}} = parseArgs({args: argv, allowPositionals: true, options: {backend: {type: "string"}}})
  if (positionals.length !== 1 || positionals[0] !== "install") throw new Error("expected command: install")
  if (backend !== "sqlite" && backend !== "postgres") throw new Error(`invalid backend: ${backend}`)
  return backend
}

export async function main(argv: string[]): Promise<number> {
  if (argv.includes("-h") || argv.includes("--help")) {
    console.log(USAGE)
    return 0
  }
  let backend: Backend
  try {
    backend = parseInstallArgs(argv)
  } catch (e) {
    console.error(`${(e as Error).message}\n${USAGE}`)
    return EXIT_USAGE
  }
  try {
    const dir = await resolveLibsDir(backend)
    console.log(`libsimplex installed at: ${dir}`)
    return 0
  } catch (e) {
    console.error(`install failed: ${(e as Error).message}`)
    return EXIT_INSTALL_FAILED
  }
}

if (require.main === module) {
  main(process.argv.slice(2)).then(code => { process.exitCode = code })
}
