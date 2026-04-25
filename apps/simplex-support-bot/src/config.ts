import {parseArgs} from "node:util"
import {api} from "simplex-chat"

export interface IdName {
  id: number
  name: string
}

export type Backend = "sqlite" | "postgres"

export interface Config {
  stateFile: string          // local path to the bot's state JSON
  db: api.DbConfig           // passed to ChatApi.init / bot.run
  teamGroup: IdName          // name from CLI, id resolved at startup from state file
  teamMembers: IdName[]      // optional, empty if not provided
  grokContactId: number | null  // resolved at startup
  timezone: string
  completeHours: number
  cardFlushSeconds: number
  contextFile: string | null
  grokApiKey: string | null
}

// Mirrors packages/simplex-chat-nodejs/src/download-libs.js so runtime detection
// matches what was used at install time. Works whether the user installed via
// SIMPLEX_BACKEND env var, .npmrc (→ npm_config_simplex_backend), or the
// --simplex_backend=postgres CLI flag (also surfaced as npm_config_*).
export function detectBackend(): Backend {
  const raw = (process.env.SIMPLEX_BACKEND || process.env.npm_config_simplex_backend || "sqlite").toLowerCase()
  if (raw !== "sqlite" && raw !== "postgres") {
    throw new Error(`Invalid SIMPLEX_BACKEND: "${raw}". Must be "sqlite" or "postgres".`)
  }
  return raw
}

export function parseIdName(s: string): IdName {
  const i = s.indexOf(":")
  if (i < 1) throw new Error(`Invalid ID:name format: "${s}"`)
  const id = parseInt(s.slice(0, i), 10)
  if (isNaN(id)) throw new Error(`Invalid ID:name format (non-numeric ID): "${s}"`)
  return {id, name: s.slice(i + 1)}
}

function parseNonNegativeInt(raw: string, flag: string): number {
  const n = parseInt(raw, 10)
  if (!Number.isFinite(n) || n < 0) {
    throw new Error(`${flag} must be a non-negative integer, got "${raw}"`)
  }
  return n
}

export function parseConfig(args: string[]): Config {
  const {values} = parseArgs({
    args,
    strict: true,
    options: {
      "team-group":            {type: "string"},
      "state-file":            {type: "string", default: "./data/state.json"},
      "sqlite-file-prefix":    {type: "string", default: "./data/simplex"},
      "sqlite-key":            {type: "string"},
      "pg-conn":               {type: "string"},
      "pg-schema":             {type: "string"},
      "auto-add-team-members": {type: "string", short: "a"},
      "timezone":              {type: "string", default: "UTC"},
      "complete-hours":        {type: "string", default: "3"},
      "card-flush-seconds":    {type: "string", default: "300"},
      "context-file":          {type: "string"},
    },
  })

  // Treat empty string as absent so `GROK_API_KEY=` behaves like unset
  const grokApiKey = process.env.GROK_API_KEY || null

  const backend = detectBackend()
  let db: api.DbConfig
  if (backend === "sqlite") {
    // default guarantees non-undefined
    const filePrefix = values["sqlite-file-prefix"]!
    const encryptionKey = values["sqlite-key"]
    db = encryptionKey
      ? {type: "sqlite", filePrefix, encryptionKey}
      : {type: "sqlite", filePrefix}
  } else {
    const connectionString = values["pg-conn"]
    if (!connectionString) {
      throw new Error("--pg-conn is required when backend is postgres (PostgreSQL connection string)")
    }
    const schemaPrefix = values["pg-schema"]
    db = schemaPrefix
      ? {type: "postgres", connectionString, schemaPrefix}
      : {type: "postgres", connectionString}
  }

  const teamGroupName = values["team-group"]
  if (!teamGroupName) throw new Error("Missing required argument: --team-group")
  const teamGroup: IdName = {id: 0, name: teamGroupName}

  const teamMembersRaw = values["auto-add-team-members"] ?? ""
  const teamMembers = teamMembersRaw
    ? teamMembersRaw.split(",").map(parseIdName)
    : []

  const timezone = values["timezone"]!
  try {
    new Intl.DateTimeFormat("en-US", {timeZone: timezone, weekday: "short"})
  } catch (err) {
    throw new Error(`--timezone "${timezone}" is not a valid IANA time zone: ${(err as Error).message}`)
  }
  const completeHours = parseNonNegativeInt(values["complete-hours"]!, "--complete-hours")
  const cardFlushSeconds = parseNonNegativeInt(values["card-flush-seconds"]!, "--card-flush-seconds")
  const contextFile = values["context-file"] || null

  if (grokApiKey && !contextFile) {
    throw new Error("GROK_API_KEY is set but --context-file is not provided. Grok requires a context file.")
  }

  return {
    stateFile: values["state-file"]!,
    db,
    teamGroup,
    teamMembers,
    grokContactId: null,
    timezone,
    completeHours,
    cardFlushSeconds,
    contextFile,
    grokApiKey,
  }
}
