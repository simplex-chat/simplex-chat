import {DaemonClient} from "./client"
import {dataRoot} from "./files"
import {ResetReply, StatusReply} from "./protocol"
import {qrCodeText} from "./qr"
import {version} from "./version"

export function linkText({mainGroup, owner}: StatusReply, tty: boolean): string {
  if (owner.owners.length > 0) return `Owners of "${mainGroup}": ${owner.owners.join(", ")}. To replace them, run /simplex:reset-owners.`
  if (owner.link === undefined) return "The Main group has no link. Run /simplex:reset-owners."
  return [`Join "${mainGroup}" in the SimpleX app. The link works once:`, ...qrLines(owner.link, tty), owner.link].join("\n")
}

export function resetOwnersText(link: string, mainGroup: string, tty: boolean): string {
  return [`"${mainGroup}" was replaced. Join the new one in the SimpleX app. The link works once:`, ...qrLines(link, tty), link].join("\n")
}

export async function showLink(env: NodeJS.ProcessEnv = process.env, tty = process.stdout.isTTY === true): Promise<string> {
  return linkText(await DaemonClient.forRoot(dataRoot(env)).ensureDaemon(version), tty)
}

export async function resetOwners(env: NodeJS.ProcessEnv = process.env, tty = process.stdout.isTTY === true): Promise<string> {
  const client = DaemonClient.forRoot(dataRoot(env))
  const {mainGroup} = await client.ensureDaemon(version)
  const {link} = await client.request<ResetReply>("POST", "/reset-owners")
  return resetOwnersText(link, mainGroup, tty)
}

function qrLines(link: string, tty: boolean): string[] {
  return tty ? [qrCodeText(link, "ansi")] : []
}
