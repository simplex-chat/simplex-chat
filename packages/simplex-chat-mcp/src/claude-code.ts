import {constants} from "fs"
import {FileHandle, open} from "fs/promises"

// spec: spec/architecture.md#claude-code-transcriptreader
const tailBytes = 4 * 1_048_576
// spec: spec/architecture.md#claude-code-transcriptreader
const markers = ['"goal_status"', '"away_summary"', '"ai-title"']
// spec: spec/architecture.md#claude-code-transcriptreader
const recapSuffix = /\s*\(disable recaps in \/config\)\s*$/
// spec: spec/architecture.md#claude-code-transcriptreader
const maxRecap = 1_000

// spec: spec/architecture.md#claude-code-transcriptreader
export type GoalState = "set" | "met" | "failed"

// spec: spec/architecture.md#claude-code-transcriptreader
export interface TranscriptState {
  goal?: GoalState
  recap?: string
  title?: string
}

interface Entry {
  type?: unknown
  subtype?: unknown
  isSidechain?: unknown
  content?: unknown
  aiTitle?: unknown
  attachment?: {type?: unknown; met?: unknown; failed?: unknown; sentinel?: unknown}
}

// spec: spec/architecture.md#claude-code-accountreader
export class AccountReader {
  private readAt?: {path: string; mtime: number}

  constructor(public account?: string) {}

  async read(path: string): Promise<string | undefined> {
    try {
      const file = await open(path, constants.O_RDONLY | constants.O_NONBLOCK)
      try {
        const stat = await file.stat()
        if (!stat.isFile()) return (this.account = undefined)
        if (this.readAt?.path === path && this.readAt.mtime === stat.mtimeMs) return this.account
        const {oauthAccount} = JSON.parse(await file.readFile("utf8")) as {oauthAccount?: {emailAddress?: unknown}}
        this.account = typeof oauthAccount?.emailAddress === "string" && oauthAccount.emailAddress !== "" ? oauthAccount.emailAddress : undefined
        this.readAt = {path, mtime: stat.mtimeMs}
      } finally {
        await file.close()
      }
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code === "ENOENT") this.account = undefined
    }
    return this.account
  }
}

// spec: spec/architecture.md#claude-code-transcriptreader
export class TranscriptReader {
  private offset?: number

  constructor(
    private path?: string,
    public state: TranscriptState = {},
  ) {}

  async read(path: string): Promise<TranscriptState> {
    if (path !== this.path) {
      this.path = path
      this.offset = undefined
      this.state = {}
    }
    await this.readAppended(path).catch(() => undefined)
    return this.state
  }

  private async readAppended(path: string): Promise<void> {
    const file = await open(path, constants.O_RDONLY | constants.O_NONBLOCK)
    try {
      const stat = await file.stat()
      if (stat.isFile()) await this.readFrom(file, stat.size)
    } finally {
      await file.close()
    }
  }

  private async readFrom(file: FileHandle, size: number): Promise<void> {
    const {offset} = this
    const start = offset === undefined || size < offset || size - offset > tailBytes ? Math.max(0, size - tailBytes) : offset
    const aligned = start === 0 || start === offset
    const buffer = Buffer.alloc(size - start)
    const {bytesRead} = await file.read(buffer, 0, buffer.length, start)
    const bytes = buffer.subarray(0, bytesRead)
    const first = aligned ? 0 : bytes.indexOf(0x0a) + 1
    if (first === 0 && !aligned) return
    const end = bytes.lastIndexOf(0x0a) + 1
    for (const line of bytes.subarray(first, end).toString("utf8").split("\n")) this.apply(line)
    this.offset = start + end
  }

  private apply(line: string): void {
    if (!markers.some(marker => line.includes(marker))) return
    const entry = entryOf(line)
    if (entry === undefined || entry.isSidechain === true) return
    if (entry.type === "attachment" && entry.attachment?.type === "goal_status") this.state = {...this.state, goal: goalOf(entry.attachment)}
    else if (entry.type === "system" && entry.subtype === "away_summary" && typeof entry.content === "string") this.state = {...this.state, recap: recapOf(entry.content)}
    else if (entry.type === "ai-title" && typeof entry.aiTitle === "string") this.state = {...this.state, title: entry.aiTitle.trim() || undefined}
  }
}

// spec: spec/architecture.md#claude-code-transcriptreader
function goalOf({met, failed, sentinel}: NonNullable<Entry["attachment"]>): GoalState | undefined {
  if (met === true) return sentinel === true ? undefined : "met"
  return failed === true ? "failed" : "set"
}

// spec: spec/architecture.md#claude-code-transcriptreader
function recapOf(content: string): string | undefined {
  const chars = [...content.replace(recapSuffix, "").trim()]
  if (chars.length === 0) return undefined
  return chars.length <= maxRecap ? chars.join("") : `${chars.slice(0, maxRecap - 1).join("")}…`
}

function entryOf(line: string): Entry | undefined {
  try {
    const entry = JSON.parse(line) as Entry | null
    return typeof entry === "object" && entry !== null ? entry : undefined
  } catch {
    return undefined
  }
}
