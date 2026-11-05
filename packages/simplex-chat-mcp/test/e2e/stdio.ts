import {spawn} from "child_process"
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

export function daemonPid(root: string): number {
  return Number(readFileSync(join(root, "lock"), "utf8").trim())
}

// SIGTERM runs the daemon's stop sequence, which removes the lock file
export async function killDaemon(root: string): Promise<void> {
  const lock = join(root, "lock")
  if (!existsSync(lock)) return
  process.kill(daemonPid(root))
  await until(() => !existsSync(lock))
}
