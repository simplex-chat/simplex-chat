import {ServiceClient} from "./client"
import {dataRoot} from "./files"
import {OwnerState, ResetReply} from "./protocol"
import {qrCodeText} from "./qr"
import {version} from "./version"

export const linkCommand = `npx -y @simplex-chat/mcp@${version} link`
export const resetCommand = `npx -y @simplex-chat/mcp@${version} reset-owners`

export function linkText({mainGroup, owner}: {mainGroup: string; owner: OwnerState}, tty: boolean): string {
  if (owner.owners.length > 0) return `Owners of "${mainGroup}": ${owner.owners.join(", ")}. To replace them, run ${resetCommand} in a terminal.`
  if (owner.link === undefined) return `The Main group has no link. Run ${resetCommand} in a terminal.`
  if (!tty) return `Run ${linkCommand} in a terminal to see the link to "${mainGroup}".`
  return [`Join "${mainGroup}" in the SimpleX app. The link works once:`, qrCodeText(owner.link, "small"), owner.link].join("\n")
}

export function resetOwnersText(link: string, mainGroup: string, tty: boolean): string {
  if (!tty) return `"${mainGroup}" was replaced. Run ${linkCommand} in a terminal to see the link to the new one.`
  return [`"${mainGroup}" was replaced. Join the new one in the SimpleX app. The link works once:`, qrCodeText(link, "small"), link].join("\n")
}

export async function showLink(env: NodeJS.ProcessEnv = process.env, tty = process.stdout.isTTY === true): Promise<string> {
  const client = ServiceClient.forRoot(dataRoot(env))
  const {mainGroup, owner} = await client.ensureService(version)
  return linkText({mainGroup, owner: owner.owners.length > 0 ? owner : await client.request<OwnerState>("GET", "/link")}, tty)
}

export async function resetOwners(env: NodeJS.ProcessEnv = process.env, tty = process.stdout.isTTY === true): Promise<string> {
  const client = ServiceClient.forRoot(dataRoot(env))
  const {mainGroup} = await client.ensureService(version)
  const {link} = await client.request<ResetReply>("POST", "/reset-owners")
  return resetOwnersText(link, mainGroup, tty)
}
