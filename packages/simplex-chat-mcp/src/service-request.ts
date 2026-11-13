import {readFileSync} from "fs"
import {request} from "http"
import {join} from "path"
import {serviceTokenFile, socketPath} from "./paths"

export function postToService(root: string, route: string, body: unknown, waitMs: number): Promise<void> {
  return new Promise(resolve => {
    let token: string
    try {
      token = readFileSync(join(root, serviceTokenFile), "utf8").trim()
    } catch {
      return resolve()
    }
    const req = request({socketPath: socketPath(root), agent: false, method: "POST", path: route, headers: {authorization: `Bearer ${token}`, "content-type": "application/json"}, timeout: waitMs}, res => {
      res.resume()
      res.once("close", () => resolve())
    })
    req.once("timeout", () => req.destroy())
    req.once("error", () => resolve())
    req.end(JSON.stringify(body))
  })
}

export function claudePid(env: NodeJS.ProcessEnv): number | undefined {
  const pid = Number(env.CLAUDE_PID)
  return Number.isInteger(pid) && pid > 0 ? pid : undefined
}
