import {ChildProcess, spawn} from "child_process"
import {once} from "events"
import {existsSync, readFileSync} from "fs"
import {join} from "path"
import {LineClient} from "../unit/fixtures"
import {until} from "./helpers"

const cli = join(__dirname, "../../dist/cli.js")

export interface SessionProcess {
  client: LineClient
  call(tool: string, args: object, meta?: object): Promise<any>
  stop(): Promise<void>
}

export function spawnSession(env: NodeJS.ProcessEnv): SessionProcess {
  const child = spawn(process.execPath, [cli, "session"], {env, stdio: ["pipe", "pipe", "inherit"]})
  child.stdout.setEncoding("utf8")
  const client = new LineClient(child.stdin, child.stdout)
  return {
    client,
    call: async (tool, args, meta) => (await client.request("tools/call", {name: tool, arguments: args, ...(meta === undefined ? {} : {_meta: meta})})).result,
    stop: async () => {
      if (child.exitCode !== null || child.signalCode !== null) return
      const exited = once(child, "exit")
      child.stdin.end()
      child.kill()
      await exited
    },
  }
}

export function spawnSessionStart(env: NodeJS.ProcessEnv, input: object): ChildProcess {
  const child = spawn(process.execPath, [cli, "hook", "session-start"], {env, stdio: ["pipe", "ignore", "inherit"]})
  child.stdin.end(JSON.stringify(input))
  return child
}

export function servicePid(root: string): number {
  return Number(readFileSync(join(root, "service.lock"), "utf8").trim())
}

export async function killService(root: string): Promise<void> {
  const lock = join(root, "service.lock")
  if (!existsSync(lock)) return
  process.kill(servicePid(root))
  await until(() => !existsSync(lock))
}
