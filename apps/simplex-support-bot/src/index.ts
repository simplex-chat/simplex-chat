import {readFileSync} from "fs"
import {join} from "path"
import {bot, api} from "simplex-chat"
import {parseConfig} from "./config.js"
import {SupportBot} from "./bot.js"
import {GrokApiClient} from "./grok.js"
import {welcomeMessage} from "./messages.js"
import {log, logError} from "./util.js"

async function main(): Promise<void> {
  const config = parseConfig(process.argv.slice(2))
  log("Config parsed", {
    dbPrefix: config.dbPrefix,
    grokDbPrefix: config.grokDbPrefix,
    teamGroup: config.teamGroup,
    teamMembers: config.teamMembers,
    grokContact: config.grokContact,
    firstRun: config.firstRun,
    timezone: config.timezone,
  })

  // --- Init Grok agent (direct ChatApi) ---
  log("Initializing Grok agent...")
  const grokChat = await api.ChatApi.init(config.grokDbPrefix)
  let grokUser = await grokChat.apiGetActiveUser()
  if (!grokUser) {
    log("No Grok user, creating...")
    grokUser = await grokChat.apiCreateActiveUser({displayName: "Grok AI", fullName: ""})
  }
  log(`Grok user: ${grokUser.profile.displayName}`)
  await grokChat.startChat()

  // --- First-run mode: establish contact between bot and Grok agent ---
  if (config.firstRun) {
    log("First-run mode: establishing bot↔Grok contact...")
    // We need to init the main bot first to create the invitation link
    const mainChat = await api.ChatApi.init(config.dbPrefix)
    let mainUser = await mainChat.apiGetActiveUser()
    if (!mainUser) {
      log("No main bot user, creating...")
      mainUser = await mainChat.apiCreateActiveUser({displayName: "SimpleX Support", fullName: ""})
    }
    await mainChat.startChat()

    const invLink = await mainChat.apiCreateLink(mainUser.userId)
    log(`Invitation link created: ${invLink}`)

    await grokChat.apiConnectActiveUser(invLink)
    log("Grok agent connecting...")

    const evt = await mainChat.wait("contactConnected", 60000)
    if (!evt) {
      console.error("Timeout waiting for Grok agent to connect (60s). Exiting.")
      process.exit(1)
    }
    const contactId = evt.contact.contactId
    const displayName = evt.contact.profile.displayName
    log(`Grok contact established. ContactId=${contactId}`)
    console.log(`\nGrok contact established. Use: --grok-contact ${contactId}:${displayName}\n`)
    process.exit(0)
  }

  // --- Normal mode: validate config, init main bot ---
  if (!config.grokContact) {
    console.error("--grok-contact is required (unless --first-run)")
    process.exit(1)
  }

  // SupportBot forward-reference: assigned after bot.run returns.
  // Events use optional chaining so any events during init are safely skipped.
  let supportBot: SupportBot | undefined

  const events: api.EventSubscribers = {
    acceptingBusinessRequest: (evt) => supportBot?.onBusinessRequest(evt),
    newChatItems: (evt) => supportBot?.onNewChatItems(evt),
    leftMember: (evt) => supportBot?.onLeftMember(evt),
    deletedMemberUser: (evt) => supportBot?.onDeletedMemberUser(evt),
    groupDeleted: (evt) => supportBot?.onGroupDeleted(evt),
    connectedToGroupMember: (evt) => supportBot?.onMemberConnected(evt),
  }

  log("Initializing main bot...")
  const [mainChat, mainUser, _mainAddress] = await bot.run({
    profile: {displayName: "SimpleX Support", fullName: ""},
    dbOpts: {dbFilePrefix: config.dbPrefix},
    options: {
      addressSettings: {
        businessAddress: true,
        autoAccept: true,
        welcomeMessage: welcomeMessage(config.groupLinks),
      },
      commands: [
        {type: "command", keyword: "grok", label: "Ask Grok AI"},
        {type: "command", keyword: "team", label: "Switch to team"},
      ],
      useBotProfile: true,
    },
    events,
  })
  log(`Main bot user: ${mainUser.profile.displayName}`)

  // --- Startup validation ---
  log("Validating config against live data...")

  // Validate team group
  const groups = await mainChat.apiListGroups(mainUser.userId)
  const teamGroup = groups.find(g => g.groupId === config.teamGroup.id)
  if (!teamGroup) {
    console.error(`Team group not found: ID=${config.teamGroup.id}. Available groups: ${groups.map(g => `${g.groupId}:${g.groupProfile.displayName}`).join(", ") || "(none)"}`)
    process.exit(1)
  }
  if (teamGroup.groupProfile.displayName !== config.teamGroup.name) {
    console.error(`Team group name mismatch: expected "${config.teamGroup.name}", got "${teamGroup.groupProfile.displayName}" (ID=${config.teamGroup.id})`)
    process.exit(1)
  }
  log(`Team group validated: ${config.teamGroup.id}:${config.teamGroup.name}`)

  // Validate contacts (team members + Grok)
  const contacts = await mainChat.apiListContacts(mainUser.userId)
  for (const member of config.teamMembers) {
    const contact = contacts.find(c => c.contactId === member.id)
    if (!contact) {
      console.error(`Team member not found: ID=${member.id}. Available contacts: ${contacts.map(c => `${c.contactId}:${c.profile.displayName}`).join(", ") || "(none)"}`)
      process.exit(1)
    }
    if (contact.profile.displayName !== member.name) {
      console.error(`Team member name mismatch: expected "${member.name}", got "${contact.profile.displayName}" (ID=${member.id})`)
      process.exit(1)
    }
    log(`Team member validated: ${member.id}:${member.name}`)
  }

  const grokContact = contacts.find(c => c.contactId === config.grokContact!.id)
  if (!grokContact) {
    console.error(`Grok contact not found: ID=${config.grokContact.id}. Available contacts: ${contacts.map(c => `${c.contactId}:${c.profile.displayName}`).join(", ") || "(none)"}`)
    process.exit(1)
  }
  if (grokContact.profile.displayName !== config.grokContact.name) {
    console.error(`Grok contact name mismatch: expected "${config.grokContact.name}", got "${grokContact.profile.displayName}" (ID=${config.grokContact.id})`)
    process.exit(1)
  }
  log(`Grok contact validated: ${config.grokContact.id}:${config.grokContact.name}`)

  log("All config validated.")

  // Load Grok context docs
  let docsContext = ""
  try {
    docsContext = readFileSync(join(process.cwd(), "docs", "simplex-context.md"), "utf-8")
    log(`Loaded Grok context docs: ${docsContext.length} chars`)
  } catch {
    log("Warning: docs/simplex-context.md not found, Grok will operate without context docs")
  }
  const grokApi = new GrokApiClient(config.grokApiKey, docsContext)

  // Create SupportBot — event handlers now route through it
  supportBot = new SupportBot(mainChat, grokChat, grokApi, config)
  log("SupportBot initialized. Bot running.")

  // Subscribe Grok agent event handlers
  grokChat.on("receivedGroupInvitation", async (evt) => {
    await supportBot?.onGrokGroupInvitation(evt)
  })

  // Keep process alive
  process.on("SIGINT", () => {
    log("Received SIGINT, shutting down...")
    process.exit(0)
  })
  process.on("SIGTERM", () => {
    log("Received SIGTERM, shutting down...")
    process.exit(0)
  })
}

main().catch(err => {
  logError("Fatal error", err)
  process.exit(1)
})
