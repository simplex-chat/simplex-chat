import {readFileSync, writeFileSync, existsSync} from "fs"
import {join} from "path"
import {bot, api} from "simplex-chat"
import {T} from "@simplex-chat/types"
import {parseConfig} from "./config.js"
import {SupportBot} from "./bot.js"
import {GrokApiClient} from "./grok.js"
import {welcomeMessage} from "./messages.js"
import {resolveDisplayNameConflict} from "./startup.js"
import {log, logError} from "./util.js"

interface BotState {
  teamGroupId?: number
  grokContactId?: number
  grokGroupMap?: {[mainGroupId: string]: number}
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

  // Profile image for the main support bot (SimpleX app icon, light variant)
  const supportImage = "data:image/jpg;base64,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"

  // --- Init Grok agent (direct ChatApi) ---
  log("Initializing Grok agent...")
  const grokChat = await api.ChatApi.init(config.grokDbPrefix)
  const grokImage = "data:image/jpg;base64,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"
  let grokUser = await grokChat.apiGetActiveUser()
  if (!grokUser) {
    log("No Grok user, creating...")
    grokUser = await grokChat.apiCreateActiveUser({displayName: "Grok AI", fullName: "", image: grokImage})
  }
  log(`Grok user: ${grokUser.profile.displayName}`)
  await grokChat.startChat()
  if (grokUser.profile.image !== grokImage) {
    try {
      log("Updating Grok profile image...")
      await grokChat.apiUpdateProfile(grokUser.userId, {
        displayName: grokUser.profile.displayName,
        fullName: grokUser.profile.fullName,
        image: grokImage,
      })
    } catch (err) {
      logError("Failed to update Grok profile image", err)
    }
  }

  // SupportBot forward-reference: assigned after bot.run returns.
  // Events use optional chaining so any events during init are safely skipped.
  let supportBot: SupportBot | undefined

  const events: api.EventSubscribers = {
    acceptingBusinessRequest: (evt) => supportBot?.onBusinessRequest(evt),
    newChatItems: (evt) => supportBot?.onNewChatItems(evt),
    chatItemUpdated: (evt) => supportBot?.onChatItemUpdated(evt),
    leftMember: (evt) => supportBot?.onLeftMember(evt),
    connectedToGroupMember: (evt) => supportBot?.onMemberConnected(evt),
    newMemberContactReceivedInv: (evt) => supportBot?.onMemberContactReceivedInv(evt),
  }

  log("Initializing main bot...")
  resolveDisplayNameConflict(config.dbPrefix, "Ask SimpleX Team")
  const [mainChat, mainUser, _mainAddress] = await bot.run({
    profile: {displayName: "Ask SimpleX Team", fullName: "", shortDescr: "Send questions about SimpleX Chat app and your suggestions", image: supportImage},
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
        {type: "command", keyword: "add", label: "Join group"},
      ],
      useBotProfile: true,
    },
    events,
  })
  log(`Main bot user: ${mainUser.profile.displayName}`)
  if (mainUser.profile.image !== supportImage) {
    try {
      log("Updating support bot profile image...")
      await mainChat.apiUpdateProfile(mainUser.userId, {
        displayName: mainUser.profile.displayName,
        fullName: mainUser.profile.fullName,
        image: supportImage,
      })
    } catch (err) {
      logError("Failed to update support bot profile image", err)
    }
  }

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

  // Restore Grok group map from persisted state
  if (state.grokGroupMap) {
    const entries: [number, number][] = Object.entries(state.grokGroupMap)
      .map(([k, v]) => [Number(k), v])
    supportBot.restoreGrokGroupMap(entries)
  }

  // Persist Grok group map on every change
  supportBot.onGrokMapChanged = (map) => {
    const obj: {[key: string]: number} = {}
    for (const [k, v] of map) obj[k] = v
    state.grokGroupMap = obj
    writeState(stateFilePath, state)
  }

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
