import {readFileSync} from "fs"
import {Command, Option} from "commander"
import {parse as parseToml} from "smol-toml"
import {api} from "simplex-chat"
import {Messages, defaultMessages} from "./messages.js"

export interface IdName {
  id: number
  name: string
}

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
  dryRun: boolean
  allowMigrations: boolean
  allowDownMigrations: boolean
  grokApiKey: string | null
  messages: Messages
}

export function parseIdName(s: string): IdName {
  const i = s.indexOf(":")
  if (i < 1) throw new Error(`Invalid ID:name format: "${s}"`)
  const id = parseInt(s.slice(0, i), 10)
  if (isNaN(id)) throw new Error(`Invalid ID:name format (non-numeric ID): "${s}"`)
  return {id, name: s.slice(i + 1)}
}

function parseNonNegativeInt(flag: string) {
  return (raw: string): number => {
    const n = parseInt(raw, 10)
    if (!Number.isFinite(n) || n < 0) {
      throw new Error(`${flag} must be a non-negative integer, got "${raw}"`)
    }
    return n
  }
}

function buildCommand(): Command {
  return new Command()
    .name("simplex-chat-support-bot")
    .description("business-address triage bot")
    .option("--config <path>", "TOML file with option values and message texts; command-line options take precedence")
    .requiredOption("--team-group <name>", "team group display name")
    .option("--state-file <path>", "state JSON path", "./data/state.json")
    .addOption(new Option("--db <backend>", "database backend").choices(["sqlite", "postgres"]).default("sqlite"))
    .option("--sqlite-file-prefix <path>", "SQLite DB file prefix", "./data/simplex")
    .option("--sqlite-key <key>", "SQLCipher encryption key (default: unencrypted)")
    .option("--pg-conn <conn>", "PostgreSQL connection string (required with --db postgres)")
    .option("--pg-schema <prefix>", "PostgreSQL schema prefix (default: simplex_v1)")
    .option("-a, --auto-add-team-members <list>", "comma-separated ID:name pairs (e.g. 1:Alice,2:Bob)")
    .option("--timezone <iana>", "IANA timezone for weekend detection", "UTC")
    .option("--complete-hours <n>", "auto-complete chats after N hours idle (0 disables)", parseNonNegativeInt("--complete-hours"), 3)
    .option("--card-flush-seconds <n>", "debounce card state writes", parseNonNegativeInt("--card-flush-seconds"), 300)
    .option("--context-file <path>", "text file with Grok system context (required if GROK_API_KEY set)")
    .option("--dry-run", "check config, database and state, then exit without starting chat")
    .option("--allow-migrations", "with --dry-run: apply pending migrations instead of reporting them")
    .option("--allow-down-migrations", "with --dry-run: roll back migrations that this build does not have")
    .addHelpText("after", "\nEnvironment:\n  GROK_API_KEY        xAI API key — enables Grok replies\n  SIMPLEX_LIBS_DIR    local libsimplex build for the --db backend\n  SIMPLEX_ADDON_PATH  locally built Node.js add-on\n")
}

interface RawOpts {
  teamGroup: string
  stateFile: string
  db: api.DbConfig["type"]
  sqliteFilePrefix: string
  sqliteKey?: string
  pgConn?: string
  pgSchema?: string
  autoAddTeamMembers?: string
  timezone: string
  completeHours: number
  cardFlushSeconds: number
  contextFile?: string
  dryRun?: boolean
  allowMigrations?: boolean
  allowDownMigrations?: boolean
}

interface ConfigFile {
  args: string[]
  messages: Messages
}

function configFilePath(args: string[]): string | undefined {
  const i = args.indexOf("--config")
  if (i >= 0) return args[i + 1]
  return args.find(arg => arg.startsWith("--config="))?.slice("--config=".length)
}

function readConfigFile(path: string): ConfigFile {
  const {messages = {}, ...options} = parseToml(readFileSync(path, "utf-8"))
  return {
    args: Object.entries(options).map(([key, value]) => configOptionArg(path, key, value)),
    messages: configMessages(path, messages),
  }
}

function configOptionArg(path: string, key: string, value: unknown): string {
  if (!/^[a-z][a-z_]*$/.test(key)) throw new Error(`${path}: key ${key} must be lowercase with underscores, as team_group`)
  if (key === "config") throw new Error(`${path}: config cannot be set in a config file`)
  const flag = `--${key.replaceAll("_", "-")}`
  if (typeof value === "string" || typeof value === "number") return `${flag}=${value}`
  if (Array.isArray(value) && value.every(v => typeof v === "string")) return `${flag}=${value.join(",")}`
  throw new Error(`${path}: ${key} must be a string, a number or a list of strings`)
}

const messageKeys = new Map(
  (Object.keys(defaultMessages) as (keyof Messages)[]).map(field => [field.replace(/[A-Z]/g, c => `_${c.toLowerCase()}`), field])
)

function configMessages(path: string, table: unknown): Messages {
  if (typeof table !== "object" || table === null || Array.isArray(table)) {
    throw new Error(`${path}: messages must be a table`)
  }
  const messages = {...defaultMessages}
  for (const [key, value] of Object.entries(table)) {
    const field = messageKeys.get(key)
    if (!field) throw new Error(`${path}: unknown message ${key}, known messages: ${[...messageKeys.keys()].join(", ")}`)
    if (typeof value !== "string") throw new Error(`${path}: message ${key} must be a string`)
    messages[field] = value
  }
  return messages
}

export function parseConfig(args: string[]): Config {
  const cmd = buildCommand().exitOverride()
  const configFile = configFilePath(args)
  const file: ConfigFile = configFile ? readConfigFile(configFile) : {args: [], messages: defaultMessages}
  try {
    cmd.parse([...file.args, ...args], {from: "user"})
  } catch (err) {
    const code = (err as {code?: string}).code
    if (code === "commander.helpDisplayed" || code === "commander.version") process.exit(0)
    throw err
  }
  const opts = cmd.opts<RawOpts>()
  const given = (option: keyof RawOpts) => cmd.getOptionValueSource(option) === "cli"

  const grokApiKey = process.env.GROK_API_KEY || null

  let db: api.DbConfig
  if (opts.db === "sqlite") {
    if (given("pgConn") || given("pgSchema")) throw new Error("--pg-conn and --pg-schema require --db postgres")
    db = opts.sqliteKey
      ? {type: "sqlite", filePrefix: opts.sqliteFilePrefix, encryptionKey: opts.sqliteKey}
      : {type: "sqlite", filePrefix: opts.sqliteFilePrefix}
  } else {
    if (given("sqliteFilePrefix") || given("sqliteKey")) throw new Error("--sqlite-file-prefix and --sqlite-key require --db sqlite")
    if (!opts.pgConn) throw new Error("--pg-conn is required with --db postgres")
    db = opts.pgSchema
      ? {type: "postgres", connectionString: opts.pgConn, schemaPrefix: opts.pgSchema}
      : {type: "postgres", connectionString: opts.pgConn}
  }

  if (opts.allowMigrations && !opts.dryRun) throw new Error("--allow-migrations requires --dry-run")
  if (opts.allowDownMigrations && !opts.dryRun) throw new Error("--allow-down-migrations requires --dry-run")

  const teamGroup: IdName = {id: 0, name: opts.teamGroup}

  const teamMembersRaw = opts.autoAddTeamMembers ?? ""
  const teamMembers = teamMembersRaw
    ? teamMembersRaw.split(",").map(parseIdName)
    : []

  try {
    new Intl.DateTimeFormat("en-US", {timeZone: opts.timezone, weekday: "short"})
  } catch (err) {
    throw new Error(`--timezone "${opts.timezone}" is not a valid IANA time zone: ${(err as Error).message}`)
  }

  const contextFile = opts.contextFile ?? null
  if (grokApiKey && !contextFile) {
    throw new Error("GROK_API_KEY is set but --context-file is not provided. Grok requires a context file.")
  }

  return {
    stateFile: opts.stateFile,
    db,
    teamGroup,
    teamMembers,
    grokContactId: null,
    timezone: opts.timezone,
    completeHours: opts.completeHours,
    cardFlushSeconds: opts.cardFlushSeconds,
    contextFile,
    dryRun: opts.dryRun ?? false,
    allowMigrations: opts.allowMigrations ?? false,
    allowDownMigrations: opts.allowDownMigrations ?? false,
    grokApiKey,
    messages: file.messages,
  }
}
