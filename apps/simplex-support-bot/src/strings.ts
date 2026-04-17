import {readFileSync} from "fs"
import {join} from "path"

export interface StringsFile {
  messages: {
    welcome: string[]
    queue: string
    queueGrokSuffix: string
    grokActivated: string[]
    teamAdded: string
    teamAddedGrokSuffix: string
    teamAlreadyInvited: string
    teamLocked: string
    noTeamMembersWithGrok: string
    noTeamMembersWithoutGrok: string
    grokInviting: string
    grokUnavailable: string
    grokError: string
    grokNoHistory: string
    directReply: string
    teamDm: string
    joinErrorNotBusinessChat: string
    joinErrorFailed: string
  }
  card: {
    stateQueue: string
    stateGrok: string
    stateTeamPending: string
    stateTeam: string
    contentImage: string
    contentVideo: string
    contentVoice: string
    contentFile: string
    truncated: string
    waitDone: string
    waitLessThan1m: string
    msgsSuffix: string
  }
  profiles: {
    botName: string
    grokName: string
  }
  commands: {
    grokLabel: string
    teamLabel: string
    joinLabel: string
    joinParams: string
  }
}

export const S: StringsFile = JSON.parse(readFileSync(join(__dirname, '..', 'strings.json'), 'utf-8'))

export function lines(arr: string[]): string {
  return arr.join("\n")
}

export function sub(template: string, params: Record<string, string>): string {
  return template.replace(/\{(\w+)\}/g, (_, key) => params[key] ?? `{${key}}`)
}

export function matchTemplate(template: string, text: string): boolean {
  const parts = template.split(/\{\w+\}/).filter(Boolean)
  return parts.every(part => text.includes(part))
}
