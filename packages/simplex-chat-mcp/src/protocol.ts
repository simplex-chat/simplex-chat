import {CallToolResult} from "@modelcontextprotocol/server"
import {Config} from "./config"
import {ToolName} from "./tools"

export const protocolVersion = 1

export interface DaemonInfo {
  version: string
  protocol: number
}

export interface OwnerState {
  owners: string[]
  link?: string
}

export interface SessionRequest {
  id: string
  project: string
  config: Config
}

export interface TitleRequest {
  id: string
  name?: string
  config: Config
}

export interface TitleReply extends DaemonInfo {
  mainGroup: string
  owner: OwnerState
}

export interface SessionReply extends DaemonInfo {
  number: number
  displayName: string
  owner: OwnerState
}

export interface AttachRequest {
  id: string
}

export interface CallRequest {
  id: string
  tool: ToolName
  input: unknown
}

export type CallLine = {progress: string} | {result: CallToolResult | null}

export interface StatusReply extends DaemonInfo {
  mainGroup: string
  owner: OwnerState
  sessions: {number: number; name: string; attached: number}[]
}

export interface ResetReply {
  link: string
}
