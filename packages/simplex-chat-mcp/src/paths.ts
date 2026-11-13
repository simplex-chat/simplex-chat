import {createHash} from "crypto"
import {homedir, tmpdir} from "os"
import {join, resolve} from "path"

const maxSocketPath = 103

export const serviceTokenFile = "service.token"

export function dataRoot(env: NodeJS.ProcessEnv = process.env, home: string = homedir()): string {
  const given = env.SIMPLEX_MCP_HOME
  return given ? resolve(given) : join(home, ".simplex-mcp")
}

export function socketPath(root: string, platform: NodeJS.Platform = process.platform): string {
  const hash = createHash("sha256").update(root).digest("hex").slice(0, 16)
  if (platform === "win32") return `\\\\.\\pipe\\simplex-mcp-${hash}`
  const path = join(root, "service.sock")
  return Buffer.byteLength(path) <= maxSocketPath ? path : join(tmpdir(), `simplex-mcp-${hash}.sock`)
}
