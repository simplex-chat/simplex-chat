export interface IdName {
  id: number
  name: string
}

export interface Config {
  dbPrefix: string
  grokDbPrefix: string
  teamGroup: IdName
  teamMembers: IdName[]
  grokContact: IdName | null // null during first-run
  groupLinks: string
  timezone: string
  grokApiKey: string
  firstRun: boolean
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
  const firstRun = args.includes("--first-run")

  const grokApiKey = process.env.GROK_API_KEY
  if (!grokApiKey) throw new Error("Missing environment variable: GROK_API_KEY")

  const dbPrefix = optionalArg(args, "--db-prefix", "./data/bot")
  const grokDbPrefix = optionalArg(args, "--grok-db-prefix", "./data/grok")
  const teamGroup = parseIdName(requiredArg(args, "--team-group"))
  const teamMembers = requiredArg(args, "--team-members").split(",").map(parseIdName)
  if (teamMembers.length === 0) throw new Error("--team-members must have at least one member")

  let grokContact: IdName | null = null
  if (!firstRun) {
    grokContact = parseIdName(requiredArg(args, "--grok-contact"))
  } else {
    const i = args.indexOf("--grok-contact")
    if (i >= 0 && i + 1 < args.length) {
      grokContact = parseIdName(args[i + 1])
    }
  }

  const groupLinks = optionalArg(args, "--group-links", "")
  const timezone = optionalArg(args, "--timezone", "UTC")

  return {
    dbPrefix,
    grokDbPrefix,
    teamGroup,
    teamMembers,
    grokContact,
    groupLinks,
    timezone,
    grokApiKey,
    firstRun,
  }
}
