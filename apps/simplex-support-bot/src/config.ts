export interface IdName {
  id: number
  name: string
}

export interface Config {
  dbPrefix: string
  grokDbPrefix: string
  teamGroup: IdName          // name from CLI, id resolved at startup from state file
  teamMembers: IdName[]      // optional, empty if not provided
  grokContactId: number | null  // resolved at startup from state file
  groupLinks: string
  timezone: string
  grokApiKey: string
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
  const grokApiKey = process.env.GROK_API_KEY
  if (!grokApiKey) throw new Error("Missing environment variable: GROK_API_KEY")

  const dbPrefix = optionalArg(args, "--db-prefix", "./data/bot")
  const grokDbPrefix = optionalArg(args, "--grok-db-prefix", "./data/grok")
  const teamGroupName = requiredArg(args, "--team-group")
  const teamGroup: IdName = {id: 0, name: teamGroupName} // id resolved at startup
  const teamMembersRaw = optionalArg(args, "--team-members", "")
  const teamMembers = teamMembersRaw ? teamMembersRaw.split(",").map(parseIdName) : []

  const groupLinks = optionalArg(args, "--group-links", "")
  const timezone = optionalArg(args, "--timezone", "UTC")

  return {
    dbPrefix,
    grokDbPrefix,
    teamGroup,
    teamMembers,
    grokContactId: null, // resolved at startup from state file
    groupLinks,
    timezone,
    grokApiKey,
  }
}
