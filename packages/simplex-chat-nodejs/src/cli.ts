#!/usr/bin/env node
import {Backend, resolveLibsDir} from "./libs"

const USAGE = "usage: simplex-chat install [--backend sqlite|postgres]"

export function parseInstallArgs(argv: string[]): Backend {
  if (argv[0] !== "install") throw new Error(USAGE)
  let backend: string | undefined = "sqlite"
  const args = argv.slice(1)
  for (let i = 0; i < args.length; i++) {
    const arg = args[i]
    if (arg === "--backend") backend = args[++i]
    else if (arg.startsWith("--backend=")) backend = arg.slice("--backend=".length)
    else throw new Error(`unknown argument: ${arg}\n${USAGE}`)
  }
  if (backend !== "sqlite" && backend !== "postgres") throw new Error(`invalid backend: ${backend}\n${USAGE}`)
  return backend
}

export async function main(argv: string[]): Promise<number> {
  try {
    const dir = await resolveLibsDir(parseInstallArgs(argv))
    console.log(`libsimplex installed at: ${dir}`)
    return 0
  } catch (e) {
    console.error(`install failed: ${(e as Error).message}`)
    return 1
  }
}

if (require.main === module) {
  main(process.argv.slice(2)).then(code => { process.exitCode = code })
}
