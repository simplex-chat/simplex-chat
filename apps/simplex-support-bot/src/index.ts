import {readFileSync, writeFileSync, existsSync} from "fs"
import {join} from "path"
import {bot, api} from "simplex-chat"
import {T} from "@simplex-chat/types"
import {parseConfig} from "./config.js"
import {SupportBot} from "./bot.js"
import {GrokApiClient} from "./grok.js"
import {welcomeMessage} from "./messages.js"
import {log, logError} from "./util.js"

interface BotState {
  teamGroupId?: number
  grokContactId?: number
}

function readState(path: string): BotState {
  if (!existsSync(path)) return {}
  try { return JSON.parse(readFileSync(path, "utf-8")) } catch { return {} }
}

function writeState(path: string, state: BotState): void {
  writeFileSync(path, JSON.stringify(state), "utf-8")
}

async function main(): Promise<void> {
  const config = parseConfig(process.argv.slice(2))
  log("Config parsed", {
    dbPrefix: config.dbPrefix,
    grokDbPrefix: config.grokDbPrefix,
    teamGroup: config.teamGroup,
    teamMembers: config.teamMembers,
    timezone: config.timezone,
  })

  const stateFilePath = `${config.dbPrefix}_state.json`
  const state = readState(stateFilePath)

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
    newMemberContactReceivedInv: (evt) => supportBot?.onMemberContactReceivedInv(evt),
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

  // --- Auto-accept direct messages from group members ---
  await mainChat.sendChatCmd(`/_set accept member contacts ${mainUser.userId} on`)
  log("Auto-accept member contacts enabled")

  // --- List contacts ---
  const contacts = await mainChat.apiListContacts(mainUser.userId)
  log(`Contacts (${contacts.length}):`, contacts.map(c => `${c.contactId}:${c.profile.displayName}`))

  // --- Resolve Grok contact: from state file or auto-establish ---
  log("Resolving Grok contact...")

  if (typeof state.grokContactId === "number") {
    const found = contacts.find(c => c.contactId === state.grokContactId)
    if (found) {
      config.grokContactId = found.contactId
      log(`Grok contact resolved from state file: ID=${config.grokContactId}`)
    } else {
      log(`Persisted Grok contact ID=${state.grokContactId} no longer exists, will re-establish`)
    }
  }

  if (config.grokContactId === null) {
    log("Establishing bot↔Grok contact...")
    const invLink = await mainChat.apiCreateLink(mainUser.userId)
    await grokChat.apiConnectActiveUser(invLink)
    log("Grok agent connecting...")

    const evt = await mainChat.wait("contactConnected", 60000)
    if (!evt) {
      console.error("Timeout waiting for Grok agent to connect (60s). Exiting.")
      process.exit(1)
    }
    config.grokContactId = evt.contact.contactId
    state.grokContactId = config.grokContactId
    writeState(stateFilePath, state)
    log(`Grok contact established: ID=${config.grokContactId} (persisted)`)
  }

  // --- Resolve team group: from state file or auto-create ---
  log("Resolving team group...")

  // Workaround: apiListGroups sends "/_groups {userId}" but the native parser
  // expects "/_groups{userId}" (no space). Send the command directly.
  const groupsResult = await mainChat.sendChatCmd(`/_groups${mainUser.userId}`)
  if (groupsResult.type !== "groupsList") {
    console.error("Failed to list groups:", groupsResult)
    process.exit(1)
  }
  const groups = groupsResult.groups

  if (typeof state.teamGroupId === "number") {
    const found = groups.find(g => g.groupId === state.teamGroupId)
    if (found) {
      config.teamGroup.id = found.groupId
      log(`Team group resolved from state file: ${config.teamGroup.id}:${found.groupProfile.displayName}`)
    } else {
      log(`Persisted team group ID=${state.teamGroupId} no longer exists, will create new`)
    }
  }

  const teamGroupPreferences: T.GroupPreferences = {
    directMessages: {enable: T.GroupFeatureEnabled.On},
  }

  if (config.teamGroup.id === 0) {
    log(`Creating team group "${config.teamGroup.name}"...`)
    const newGroup = await mainChat.apiNewGroup(mainUser.userId, {
      displayName: config.teamGroup.name,
      fullName: "",
      groupPreferences: teamGroupPreferences,
    })
    config.teamGroup.id = newGroup.groupId
    state.teamGroupId = config.teamGroup.id
    writeState(stateFilePath, state)
    log(`Team group created: ${config.teamGroup.id}:${config.teamGroup.name} (persisted)`)
  } else {
    // Ensure direct messages are enabled on existing team group
    await mainChat.apiUpdateGroupProfile(config.teamGroup.id, {
      displayName: config.teamGroup.name,
      fullName: "",
      groupPreferences: teamGroupPreferences,
    })
  }

  // --- Create invite link for team group (for team members to join) ---
  // Delete any stale link from a previous run (e.g., crash without graceful shutdown)
  try { await mainChat.apiDeleteGroupLink(config.teamGroup.id) } catch {}
  const teamGroupInviteLink = await mainChat.apiCreateGroupLink(config.teamGroup.id, T.GroupMemberRole.Member)
  log(`Team group invite link created`)
  console.log(`\nTeam group invite link (expires in 10 min):\n${teamGroupInviteLink}\n`)

  // Schedule invite link deletion after 10 minutes
  let inviteLinkDeleted = false
  async function deleteInviteLink(): Promise<void> {
    if (inviteLinkDeleted) return
    inviteLinkDeleted = true
    try {
      await mainChat.apiDeleteGroupLink(config.teamGroup.id)
      log("Team group invite link deleted")
    } catch (err) {
      logError("Failed to delete team group invite link", err)
    }
  }
  const inviteLinkTimer = setTimeout(async () => {
    log("10 minutes elapsed, deleting team group invite link...")
    await deleteInviteLink()
  }, 10 * 60 * 1000)
  inviteLinkTimer.unref() // don't keep process alive for the timer

  // --- Validate team member contacts (if provided) ---
  if (config.teamMembers.length > 0) {
    log("Validating team member contacts...")
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
  }

  log("Startup complete.")

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
  grokChat.on("connectedToGroupMember", (evt) => {
    supportBot?.onGrokMemberConnected(evt)
  })

  // Graceful shutdown: delete invite link before exit
  async function shutdown(signal: string): Promise<void> {
    log(`Received ${signal}, shutting down...`)
    clearTimeout(inviteLinkTimer)
    await deleteInviteLink()
    process.exit(0)
  }
  process.on("SIGINT", () => shutdown("SIGINT"))
  process.on("SIGTERM", () => shutdown("SIGTERM"))
}

main().catch(err => {
  logError("Fatal error", err)
  process.exit(1)
})
