import {readFileSync, writeFileSync, existsSync} from "fs"
import {api, bot, util} from "simplex-chat"
import {T} from "@simplex-chat/types"
import {parseConfig} from "./config.js"
import {SupportBot} from "./bot.js"
import {GrokApiClient} from "./grok.js"
import {welcomeMessage} from "./messages.js"
import {profileMutex, log, logError} from "./util.js"

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
    teamGroup: config.teamGroup,
    teamMembers: config.teamMembers,
    timezone: config.timezone,
    completeHours: config.completeHours,
  })
  const grokEnabled = config.grokApiKey !== null
  if (!grokEnabled) log("No GROK_API_KEY provided, disabling Grok support")

  const stateFilePath = `${config.dbPrefix}_state.json`
  const state = readState(stateFilePath)

  // Forward-reference for event handlers during init
  let supportBot: SupportBot | undefined

  // On restart, the active user may be Grok (if the previous run was killed
  // mid-profile-switch). bot.run() uses apiGetActiveUser() and would then try
  // to rename Grok to "Ask SimpleX Team" → duplicateName error.
  // Fix: pre-init the DB, find the main user, set it active, then close.
  {
    const preChat = await api.ChatApi.init(config.dbPrefix)
    const activeUser = await preChat.apiGetActiveUser()
    if (activeUser && activeUser.profile.displayName !== "Ask SimpleX Team") {
      await preChat.startChat()
      const users = await preChat.apiListUsers()
      const mainUserInfo = users.find(u => u.user.profile.displayName === "Ask SimpleX Team")
      if (mainUserInfo) {
        await preChat.apiSetActiveUser(mainUserInfo.user.userId)
        log("Restored active user to Ask SimpleX Team")
      }
      await preChat.close()
    } else {
      await preChat.close()
    }
  }

  // Profile images (base64-encoded JPEG)
  const supportImage = "data:image/jpg;base64,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"
  const grokImage = "data:image/jpg;base64,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"

  // Step 1: Init main bot via bot.run()
  log("Initializing main bot...")
  const [chat, mainUser, mainAddress] = await bot.run({
    profile: {displayName: "Ask SimpleX Team", fullName: "", image: supportImage},
    dbOpts: {dbFilePrefix: config.dbPrefix},
    options: {
      addressSettings: {
        businessAddress: true,
        autoAccept: true,
        welcomeMessage,
      },
      commands: [
        ...(grokEnabled ? [{type: "command" as const, keyword: "grok", label: "Ask Grok AI"}] : []),
        {type: "command", keyword: "team", label: "Switch to team"},
      ],
      useBotProfile: true,
    },
    events: {
      acceptingBusinessRequest: (evt) => supportBot?.onBusinessRequest(evt),
      newChatItems: (evt) => supportBot?.onNewChatItems(evt),
      chatItemUpdated: (evt) => supportBot?.onChatItemUpdated(evt),
      chatItemReaction: (evt) => supportBot?.onChatItemReaction(evt),
      leftMember: (evt) => supportBot?.onLeftMember(evt),
      joinedGroupMember: (evt) => supportBot?.onJoinedGroupMember(evt),
      connectedToGroupMember: (evt) => supportBot?.onMemberConnected(evt),
      newMemberContactReceivedInv: (evt) => supportBot?.onMemberContactReceivedInv(evt),
      contactConnected: (evt) => supportBot?.onContactConnected(evt),
      contactSndReady: (evt) => supportBot?.onContactSndReady(evt),
    },
  })
  log(`Main bot user: ${mainUser.profile.displayName} (userId=${mainUser.userId})`)

  // Step 2: Resolve Grok profile from same ChatApi instance
  let grokUser: T.User | null = null
  if (grokEnabled) {
    log("Resolving Grok profile...")
    const users = await chat.apiListUsers()
    grokUser = users.find(u => u.user.profile.displayName === "Grok AI")?.user ?? null
    if (!grokUser) {
      log("Creating Grok profile...")
      grokUser = await chat.apiCreateActiveUser({displayName: "Grok AI", fullName: "", image: grokImage})
      // apiCreateActiveUser sets Grok as active — switch back to main
      await chat.apiSetActiveUser(mainUser.userId)
    } else {
      const grokProfile: T.Profile = {displayName: "Grok AI", fullName: "", image: grokImage}
      const currentProfile = util.fromLocalProfile(grokUser.profile)
      if (currentProfile.image !== grokProfile.image || currentProfile.displayName !== grokProfile.displayName || currentProfile.fullName !== grokProfile.fullName) {
        log("Grok profile changed, updating...")
        await chat.apiSetActiveUser(grokUser.userId)
        const summary = await chat.apiUpdateProfile(grokUser.userId, grokProfile)
        await chat.apiSetActiveUser(mainUser.userId)
        if (summary) {
          log(`Grok profile updated: ${summary.updateSuccesses} contact(s) updated, ${summary.updateFailures} failed`)
        } else {
          log("Unexpected: Grok profile did not change")
        }
      }
    }
    log(`Grok profile: ${grokUser.profile.displayName} (userId=${grokUser.userId})`)
  }

  // Step 3: Read state file
  // Step 4: Enable auto-accept DM contacts
  await chat.apiSetAutoAcceptMemberContacts(mainUser.userId, true)
  log("Auto-accept member contacts enabled")

  // Step 5: List contacts, resolve Grok contact
  const contacts = await chat.apiListContacts(mainUser.userId)
  log(`Contacts: ${contacts.map(c => `${c.contactId}:${c.profile.displayName}`).join(", ") || "(none)"}`)

  // Always restore grokContactId so the one-way gate can find and remove
  // Grok members even when Grok API is disabled.
  if (typeof state.grokContactId === "number") {
    const found = contacts.find(c => c.contactId === state.grokContactId)
    if (found) {
      config.grokContactId = found.contactId
      log(`Grok contact from state: ID=${config.grokContactId}`)
    } else {
      log(`Persisted Grok contact ID=${state.grokContactId} not found`)
    }
  }

  if (grokEnabled) {
    if (config.grokContactId === null) {
      log("Establishing bot↔Grok contact...")
      const invLink = await chat.apiCreateLink(mainUser.userId)
      // Switch to Grok profile to connect
      await profileMutex.runExclusive(async () => {
        await chat.apiSetActiveUser(grokUser!.userId)
        await chat.apiConnectActiveUser(invLink)
        await chat.apiSetActiveUser(mainUser.userId)
      })
      log("Grok connecting...")

      const evt = await chat.wait("contactConnected", 60000)
      if (!evt) {
        console.error("Timeout waiting for Grok contact (60s). Exiting.")
        process.exit(1)
      }
      config.grokContactId = evt.contact.contactId
      state.grokContactId = config.grokContactId
      writeState(stateFilePath, state)
      log(`Grok contact established: ID=${config.grokContactId}`)
    }
  }

  // Step 6: Resolve team group
  log("Resolving team group...")
  const groups = await chat.apiListGroups(mainUser.userId)

  let existingGroup: T.GroupInfo | undefined

  if (typeof state.teamGroupId === "number") {
    existingGroup = groups.find(g => g.groupId === state.teamGroupId)
    if (existingGroup) {
      config.teamGroup.id = existingGroup.groupId
      log(`Team group from state: ${config.teamGroup.id}:${existingGroup.groupProfile.displayName}`)
    } else {
      log(`Persisted team group ID=${state.teamGroupId} not found, will create`)
    }
  }

  const teamGroupPreferences: T.GroupPreferences = {
    directMessages: {enable: T.GroupFeatureEnabled.On},
    fullDelete: {enable: T.GroupFeatureEnabled.On},
    commands: [
      {type: "command", keyword: "join", label: "Join customer chat", params: "groupId:name"},
    ],
  }

  if (config.teamGroup.id === 0) {
    log(`Creating team group "${config.teamGroup.name}"...`)
    const newGroup = await chat.apiNewGroup(mainUser.userId, {
      displayName: config.teamGroup.name,
      fullName: "",
      groupPreferences: teamGroupPreferences,
    })
    config.teamGroup.id = newGroup.groupId
    state.teamGroupId = config.teamGroup.id
    writeState(stateFilePath, state)
    log(`Team group created: ${config.teamGroup.id}:${config.teamGroup.name}`)
  } else if (existingGroup) {
    // Only update profile if preferences or name changed
    const prefs = existingGroup.fullGroupPreferences
    const needsUpdate =
      existingGroup.groupProfile.displayName !== config.teamGroup.name ||
      prefs.directMessages?.enable !== T.GroupFeatureEnabled.On ||
      prefs.fullDelete?.enable !== T.GroupFeatureEnabled.On ||
      JSON.stringify(prefs.commands) !== JSON.stringify(teamGroupPreferences.commands)
    if (needsUpdate) {
      await chat.apiUpdateGroupProfile(config.teamGroup.id, {
        displayName: config.teamGroup.name,
        fullName: "",
        groupPreferences: teamGroupPreferences,
      })
      log("Team group profile updated")
    }
  }

  // Step 7: Ensure direct messages enabled (done via groupPreferences above)

  // Step 8: Create team group invite link (best-effort — bot works without it)
  let inviteLinkCreated = false
  try {
    try { await chat.apiDeleteGroupLink(config.teamGroup.id) } catch {}
    const teamGroupInviteLink = await chat.apiCreateGroupLink(
      config.teamGroup.id, T.GroupMemberRole.Member
    )
    inviteLinkCreated = true
    log("Team group invite link created")
    console.log(`\nTeam group invite link (expires in 10 min):\n${teamGroupInviteLink}\n`)
  } catch (err) {
    logError("Failed to create team group invite link (SMP relay may be unreachable). Bot will continue without it.", err)
  }

  let inviteLinkDeleted = false
  async function deleteInviteLink(): Promise<void> {
    if (inviteLinkDeleted) return
    inviteLinkDeleted = true
    try {
      await profileMutex.runExclusive(async () => {
        await chat.apiSetActiveUser(mainUser.userId)
        await chat.apiDeleteGroupLink(config.teamGroup.id)
      })
      log("Team group invite link deleted")
    } catch (err) {
      logError("Failed to delete invite link", err)
    }
  }
  let inviteLinkTimer: ReturnType<typeof setTimeout> | undefined
  if (inviteLinkCreated) {
    inviteLinkTimer = setTimeout(async () => {
      log("10 minutes elapsed, deleting invite link...")
      await deleteInviteLink()
    }, 10 * 60 * 1000)
    inviteLinkTimer.unref()
  }

  // Step 9: Validate team members
  if (config.teamMembers.length > 0) {
    log("Validating team members...")
    for (const member of config.teamMembers) {
      const contact = contacts.find(c => c.contactId === member.id)
      if (!contact) {
        console.error(`Team member not found: ID=${member.id}. Available: ${contacts.map(c => `${c.contactId}:${c.profile.displayName}`).join(", ") || "(none)"}`)
        process.exit(1)
      }
      if (contact.profile.displayName !== member.name) {
        console.error(`Team member name mismatch: expected "${member.name}", got "${contact.profile.displayName}" (ID=${member.id})`)
        process.exit(1)
      }
      log(`Team member validated: ${member.id}:${member.name}`)
    }
  }

  // Load Grok context and build API client only if enabled
  let grokApi: GrokApiClient | null = null
  if (grokEnabled) {
    let contextFile = ""
    if (config.contextFile) {
      try {
        contextFile = readFileSync(config.contextFile, "utf-8")
        log(`Loaded Grok context: ${contextFile.length} chars from ${config.contextFile}`)
      } catch {
        log(`Warning: context file not found: ${config.contextFile}`)
      }
    }
    grokApi = new GrokApiClient(config.grokApiKey!, contextFile)
  }

  // Create SupportBot
  supportBot = new SupportBot(chat, grokApi, config, mainUser.userId, grokUser?.userId ?? null)

  if (mainAddress) {
    supportBot.businessAddress = util.contactAddressStr(mainAddress.connLinkContact)
    log(`Business address: ${supportBot.businessAddress}`)
  }

  // Step 10: Register Grok event handlers (filtered by profile in handler)
  if (grokEnabled) {
    chat.on("receivedGroupInvitation", (evt) => supportBot?.onGrokGroupInvitation(evt))
    chat.on("connectedToGroupMember", (evt) => supportBot?.onGrokMemberConnected(evt))
    chat.on("newChatItems", (evt) => supportBot?.onGrokNewChatItems(evt))
  }

  // Step 10b: Refresh stale cards from before restart
  await supportBot.cards.refreshAllCards()

  log("SupportBot initialized. Bot running.")

  // Step 11: Graceful shutdown
  async function shutdown(signal: string): Promise<void> {
    log(`Received ${signal}, shutting down...`)
    clearTimeout(inviteLinkTimer)
    supportBot?.cards.destroy()
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
