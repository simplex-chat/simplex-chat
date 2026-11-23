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
  record?: string
  globalConfig?: string
}

// spec: spec/architecture.md#protocol-sessionrequest
export interface TitleRequest {
  id: string
  pid?: number
  at?: number
  name?: string
  project: string
  model?: string
  transcript?: string
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

// spec: spec/architecture.md#protocol-askrequest
export type AskRequest = {id: string; pid?: number; at?: number} & Prompt

// spec: spec/architecture.md#protocol-askrequest
export type Prompt =
  | {kind: "tool"; tool: string; description: string; preview: string; agent?: string; terminalOnly?: boolean}
  | {kind: "question"; questions: Question[]}
  | {kind: "plan"; plan: string}
  | {kind: "link"; server: string; message: string; url: string}

// spec: spec/architecture.md#protocol-askrequest
export interface Question {
  question: string
  options: {label: string; description: string}[]
  multiSelect: boolean
}

// spec: spec/architecture.md#protocol-askrequest
export type Decision = {behavior: "allow"; answers?: {[question: string]: string}} | {behavior: "deny"; message: string}

// spec: spec/architecture.md#protocol-askrequest
export interface AskLine {
  decision?: Decision
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
  step?: Step
  text?: TextBatch
  usage?: string
  switched?: boolean
  limits?: Limits
  statusLine?: boolean
  failure?: Failure
  waiting?: Waiting
  agent?: AgentEvent
  task?: TaskEnd
  workflow?: WorkflowRun
}

// spec: spec/architecture.md#protocol-failure
export interface Failure {
  error: string
  text: string
  autoContinue: boolean
  menu: boolean
}

// spec: spec/architecture.md#protocol-failure
export interface Waiting {
  kind: string
  text: string
}

// spec: spec/architecture.md#protocol-agentevent
export type AgentEvent = {id: string} & ({start: AgentStart} | {step: Step} | {report: string} | {stop: string})

// spec: spec/architecture.md#protocol-agentevent
export interface AgentStart {
  type: string
  description: string
  toolUse?: string
  transcript: string
}

// spec: spec/architecture.md#protocol-agentevent
export type TaskStatus = "completed" | "failed" | "killed" | "stopped"

// spec: spec/architecture.md#protocol-agentevent
export interface TaskEnd {
  toolUse: string
  status: TaskStatus
  tokens?: number
  steps?: number
  ms?: number
}

// spec: spec/architecture.md#protocol-agentevent
export interface WorkflowRun {
  toolUse: string
  run: string
  name: string
  folder: string
}

// spec: spec/architecture.md#protocol-limits
export interface Limits {
  fiveHour?: LimitWindow
  week?: LimitWindow
}

// spec: spec/architecture.md#protocol-limits
export interface LimitWindow {
  percent: number
  resetsAt: number
}

// spec: spec/architecture.md#protocol-step
export interface Step {
  id: string
  text: string
  end?: boolean
}

// spec: spec/architecture.md#protocol-textbatch
export interface TextBatch {
  message: string
  index: number
  final: boolean
  delta: string
}

// spec: spec/architecture.md#protocol-attachline
export interface AttachLine {
  prompt?: {text: string; from: string; item: number; file?: string; replyTo?: {item?: number; from?: string}; edited?: boolean}
  permission?: {requestId: string; behavior: "allow" | "deny"}
  tools?: ApprovedTool[]
}

// spec: spec/architecture.md#protocol-attachline
export interface ApprovedTool {
  tool: string
  approvedBy: string
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
  pid?: number
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
