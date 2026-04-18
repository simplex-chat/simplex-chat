export interface IdName {
  id: number
  name: string
}

export interface Config {
  dbPrefix: string
  teamGroup: IdName          // name from CLI, id resolved at startup from state file
  teamMembers: IdName[]      // optional, empty if not provided
  grokContactId: number | null  // resolved at startup
  timezone: string
  completeHours: number
  cardFlushSeconds: number
  contextFile: string | null
  grokApiKey: string | null
}

export function parseIdName(s: string): IdName {
  const i = s.indexOf(":")
  if (i < 1) throw new Error(`Invalid ID:name format: "${s}"`)
  const id = parseInt(s.slice(0, i), 10)
  if (isNaN(id)) throw new Error(`Invalid ID:name format (non-numeric ID): "${s}"`)
  return {id, name: s.slice(i + 1)}
}

function requiredArg(args: string[], flag: string): string {
  const i = args.indexOf(flag)
  if (i < 0 || i + 1 >= args.length) throw new Error(`Missing required argument: ${flag}`)
  return args[i + 1]
}

function optionalArg(args: string[], flag: string, defaultValue: string): string {
  const i = args.indexOf(flag)
  if (i < 0 || i + 1 >= args.length) return defaultValue
  return args[i + 1]
}

export function parseConfig(args: string[]): Config {
  // Treat empty string as absent so `GROK_API_KEY=` behaves like unset
  const grokApiKey = process.env.GROK_API_KEY || null

  const dbPrefix = optionalArg(args, "--db-prefix", "./data/simplex")
  const teamGroupName = requiredArg(args, "--team-group")
  const teamGroup: IdName = {id: 0, name: teamGroupName}

  const teamMembersRaw = optionalArg(args, "--auto-add-team-members", "") || optionalArg(args, "-a", "")
  const teamMembers = teamMembersRaw
    ? teamMembersRaw.split(",").map(parseIdName)
    : []

  const timezone = optionalArg(args, "--timezone", "UTC")
  const completeHours = parseInt(optionalArg(args, "--complete-hours", "3"), 10)
  const cardFlushSeconds = parseInt(optionalArg(args, "--card-flush-seconds", "300"), 10)
  const contextFileRaw = optionalArg(args, "--context-file", "")
  const contextFile = contextFileRaw || null

  if (grokApiKey && !contextFile) {
    throw new Error("GROK_API_KEY is set but --context-file is not provided. Grok requires a context file.")
  }

  return {
    dbPrefix,
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
