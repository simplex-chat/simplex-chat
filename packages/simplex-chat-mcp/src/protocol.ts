import {CallToolResult} from "@modelcontextprotocol/server"
import {Config} from "./config"
import {ToolName} from "./tools"

// spec: spec/architecture.md#protocol-protocolversion
export const protocolVersion = 1
// spec: spec/architecture.md#protocol-protocolversion
export const unattendedStatus = 403

export interface ServiceInfo {
  version: string
  protocol: number
}

// spec: spec/architecture.md#protocol-owners
export interface Owners {
  owners: string[]
}

// spec: spec/architecture.md#protocol-owners
export interface OwnerState extends Owners {
  link?: string
}

// spec: spec/architecture.md#protocol-sessionrequest
export interface SessionRequest {
  id: string
  pid?: number
  project: string
  name?: string
  config: Config
}

// spec: spec/architecture.md#protocol-sessionrequest
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

// spec: spec/architecture.md#protocol-permissionrequest
export interface PermissionRequest {
  id: string
  requestId: string
  toolName: string
  description: string
  inputPreview: string
}

// spec: spec/architecture.md#protocol-permissionrequest
export interface TurnRequest {
  id: string
  pid?: number
  at?: number
  prompt?: string
  typed?: boolean
  note?: string
  midTurn?: boolean
  answer?: string
  model?: string
  effort?: string
}

// spec: spec/architecture.md#protocol-attachline
export interface AttachLine {
  prompt?: {text: string; from: string; item: number; file?: string; replyTo?: {item?: number; from?: string}}
  permission?: {requestId: string; behavior: "allow" | "deny"}
}

// spec: spec/architecture.md#protocol-titlereply
export interface TitleReply extends ServiceInfo {
  mainGroup: string
  owner: OwnerState
}

// spec: spec/architecture.md#protocol-titlereply
export interface SessionReply extends ServiceInfo {
  number: number
  displayName: string
  owner: Owners
}

// spec: spec/architecture.md#protocol-titlereply
export interface AttachRequest {
  id: string
}

// spec: spec/architecture.md#protocol-titlereply
export interface CallRequest {
  id: string
  tool: ToolName
  input: unknown
}

// spec: spec/architecture.md#protocol-titlereply
export type CallLine = {progress: string} | {result: CallToolResult | null}

// spec: spec/architecture.md#protocol-statusreply
export interface StatusReply extends ServiceInfo {
  mainGroup: string
  owner: Owners
  sessions: {number: number; name: string; attached: number}[]
}

// spec: spec/architecture.md#protocol-statusreply
export interface ResetReply {
  link: string
}
