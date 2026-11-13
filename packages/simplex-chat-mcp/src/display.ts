import {dataRoot} from "./paths"
import {claudePid, postToService} from "./service-request"

const requestWaitMs = 1_000

interface DisplayInput {
  session_id?: unknown
  message_id?: unknown
  index?: unknown
  final?: unknown
  delta?: unknown
}

export async function display(stdin: string, env: NodeJS.ProcessEnv = process.env, at = performance.timeOrigin): Promise<void> {
  const {session_id: id, message_id: message, index, final, delta} = inputOf(stdin)
  if (typeof id !== "string" || typeof message !== "string" || !Number.isInteger(index)) return
  const text = {message, index, final: final === true, delta: typeof delta === "string" ? delta : ""}
  await postToService(dataRoot(env), "/turn", {id, pid: claudePid(env), at, text}, requestWaitMs)
}

function inputOf(stdin: string): DisplayInput {
  try {
    return (JSON.parse(stdin) as DisplayInput | null) ?? {}
  } catch {
    return {}
  }
}

if (require.main === module) {
  let stdin = ""
  process.stdin.setEncoding("utf8")
  process.stdin.on("data", chunk => (stdin += chunk))
  process.stdin.on("end", () => display(stdin).then(() => process.stdout.write("{}\n")))
}
