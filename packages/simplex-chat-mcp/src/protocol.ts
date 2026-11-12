import {CallToolResult} from "@modelcontextprotocol/server"
import {Config} from "./config"
import {ToolName} from "./tools"

export const protocolVersion = 1
export const unattendedStatus = 403

export interface ServiceInfo {
  version: string
  protocol: number
}

export interface Owners {
  owners: string[]
}

export interface OwnerState extends Owners {
  link?: string
}

export interface SessionRequest {
  id: string
  pid?: number
  project: string
  name?: string
  config: Config
}

export interface TitleRequest {
  id: string
  pid?: number
  at?: number
  name?: string
  project: string
  model?: string
  config: Config
  unattended?: boolean
}

export interface PermissionRequest {
  id: string
  requestId: string
  toolName: string
  description: string
  inputPreview: string
}

export interface TurnRequest {
  id: string
  pid?: number
  at?: number
  prompt?: string
  typed?: boolean
  answer?: string
  model?: string
  effort?: string
}

export interface AttachLine {
  prompt?: {text: string; from: string; item: number; file?: string}
  permission?: {requestId: string; behavior: "allow" | "deny"}
}

export interface TitleReply extends ServiceInfo {
  mainGroup: string
  owner: OwnerState
}

export interface SessionReply extends ServiceInfo {
  number: number
  displayName: string
  owner: Owners
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

export interface StatusReply extends ServiceInfo {
  mainGroup: string
  owner: Owners
  sessions: {number: number; name: string; attached: number}[]
}

export interface ResetReply {
  link: string
}
