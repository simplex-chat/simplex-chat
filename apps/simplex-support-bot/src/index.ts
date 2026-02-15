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
  const supportImage = "data:image/jpg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAYEBQYFBAYGBQYHBwYIChAKCgkJChQODwwQFxQYGBcUFhYaHSUfGhsjHBYWICwgIyYnKSopGR8tMC0oMCUoKSj/2wBDAQcHBwoIChMKChMoGhYaKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCj/wAARCACAAIADASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD6pooooAKKKKACiignAyelABRQCGAIIIPIIooAKKKKACikjdZEDxsGU8gqcg0tAk01dBRRRQMKKKKACiiigAooooAK898ZeKftBew058Qj5ZZR/H7D29+9ehVxHjTwt5++/wBMT9996WFR9/8A2h7+3f69e/LnRVZe1+Xa587xNTxtTBNYP/t627Xl+vVr8c/wf4oNkyWWoPm1PCSH/ln7H/Z/lXo6kMAVIIPIIrwTdiuw8GeKjYsljqDk2h4SQ/8ALP2P+z/KvSzDLua9WkteqPmOGeJHQtg8Y/d+zLt5Py7Pp6bel1wXjHxRv32GmyfJ92WZT97/AGV9vU1H4z8ViTfYaZJ+7+7LMp+9/sqfT1NcOGqMvy61qtVeiNeJuJea+Dwb02lJfkv1Z1PhTxI+lSiC5JeyY8jqYz6j29RXp6MHRWU5VhkGuG8F+F8eXqGpx8/eihYdP9ph/IV3VcWZTpSq/u9+p7fCdDG0cHbFP3X8Ke6X+XZdAooorzj6kKKKKACiikYhVJYgAckmgBTxRXzJ8dPi6dUNx4d8LXGNPGY7u8jP+v8AVEP9z1P8XQcddL4E/F7/AI9/Dfiu49I7K+kbr2Ech/QN+B7Gu95dWVH2tvl1scqxdN1OQ+iaKKK4DqOG8b+FPPEmoaYn7770sKj7/wDtD39u/wBevnAas346/F77X9o8N+FLj/R+Y7y+jb/WdjHGf7vYt36DjJPnvgPxibXy9M1aT/R+FhnY/wCr9FY/3fQ9vp0+ty32qpJVvl3sfnPEmS051HiMItftJfmv1PVN1eheCPCvEeo6mmScNDC36M39BXm+6u18EeLTYMljqTk2h4jkP/LL2P8As/yrTMIVnRfsfn3t5Hh8PPB08ZF4xadOyfn/AF6nqNFIrBlDKQQeQR3pa+OP2IKRHV1DIwZT0IORXn/jjxdt8zTtLk+b7s0ynp6qp/maxPB3il9HmFvdFnsHPI6mM+o9vUV6cMqrTo+169F5HzNfinCUcYsM9Y7OXRP/AC7voeuUU2KRZY0kjIZGAZSO4NOrzD6VO+qCkZQylWAKkYIPelooGfMHxz+EZ0Zp/EPheAnTDl7q0jH/AB7eroP7nqP4fp08Lr9EmUMpVgCDwQa+Yfjn8Im0dp/EPhe3LaaSXurOMZNue7oP7nqP4fp09/L8w5rUqr16M8vF4S3vwNb4FfF7/j38N+K7jniOyvpG69hHIT+QY/Q9jVb47fF03RufDfhS4xbjMd7exn/WdjHGf7vYt36DjJPz/RXZ/Z9H23tbfLpfuc/1up7PkE6D0FfRnwK+EOw2/iTxXb/PxJZ2Mi/d7iSQevcL26nnAB8C/hD5Zt/Efiy3xJxJZ2Mq/d7iSQHv3C9up5wB9D1wZhmG9Kk/VnVhMJ9uZwPjvwj9o8zUtKj/AH33poVH3/8AaX39R3+vXzLdX0XXn3j3wd9o8zUtJj/f/emgUff/ANpR6+o7/XrpleZ2tRrPTo/0Z8xxFw5z3xeEWvVd/NfqjL8DeLzp7JYam5NmTiOQ/wDLL2P+z/KtDx14xAD6dpEuT0mnQ9P9lT/M15nu5pd1etLLKMq3tmvl0v3Pm4Z9jIYP6mpad+qXYn3V6D4E8ImXy9S1WP8Ad/ehgYfe9GYenoKj8A+EPOEWp6tH+74aCBh970Zh6eg716ZXl5nmVr0aL9X+iPe4d4cvbF4tecY/q/0QUUUV86ffhRRRQAV82/HX4vfa/tHhvwpcf6NzHeX0bf6zsY4z/d7Fu/QcZJPjr8XvtRuPDfhS4/0fmO8vo2/1nYxxkfw9i3foOMk/P/8AKvdy/L7Wq1V6I8zF4v7EBOn0pa+i/gX8INot/Efiy2+fiSzsZV+76SSA9/RT06nnAGP8dPhGdHa48Q+F4CdMJL3Vogybc93Qf3PUfw/Tp3rH0XV9lf59L9jleFqKn7Q1vgV8Xjm38N+LLnJ4js76VuvYRyE/kGP0PY19E1+dlfRXwJ+L3Nv4b8V3HPEdlfSN17COQn8g34Hsa8/MMv3q0l6o68Ji/sTPomvNfiB412mTS9Hl+blZ7hT09VU+vqaj+InjfYZdK0eX5uVnuFPT1VT6+p/CvMN1dOVZTe1euvRfqz5riDP98LhX6v8ARfqybdS7q9E+HngszeVqmsRfu+Ggt2H3vRmHp6DvVz4heC/tAk1PR4v3/wB6aBR9/wD2lHr6jv8AXr6TzTDqv7C/z6X7Hgx4dxcsJ9aS/wC3etu//AMrwD4zOnMmn6pITZE4jlY5MXsf9n+X0r1pWDKGUgqRkEd6+Zd2K7z4f+NDprR6dqrk2JOI5T/yx9j/ALP8vpXFmuU8961Ba9V3815/mevw/n7o2wuKfu9H28n5fl6bev0UisGUMpBUjII70tfKn3wVHdQRXVtLb3CCSGVCjoejKRgg/hUlFAHx98Z/hbceCrttQ0tXm8PTNhWPLWrHojn09G/A89e7+BXwh8v7P4k8V2/z8SWdjIv3e4kkB79wvbqecAfQc0Mc8TRzRpJG3VXUEH8DT69GeZVZ0vZ9e5yRwcI1Of8AAKRlDKVYAg8EGlorzjrPmD45/CM6O0/iHwvATphJe6tIx/x7+roP7nqP4fp04Hwh4aB2X+pR8feihYdf9ph/IV9EfErx2B52kaLKCeUuLhT09UU/zP4V5Tur7jKaFaVFTxHy728z4LPcxgpujhX6v9F+pPur074c+CDN5Wq6zF+64aC3cfe9GYenoO9eV7q9d+G/joXXlaVrUv8ApHCwXDH/AFnorH+96Hv9eumb/WI4duh8+9vI87IaeFeKX1n5dr+f6HptFFFfBn6ceb/ETwT9pEuqaNH/AKR96eBR/rPVlH971Hf69c34d+CTdmPU9ZiIth80MDj/AFn+0w/u+g7/AE6+tUV6kc2rxw/sE/n1t2PEnkGEnivrTXy6X7/8AAAAABgCiiivLPbCiiigAooooAK8n+Jnj7YZdI0OX5uUuLlD09UU+vqfwFerSossbxuMowKkeoNeBfETwTL4cuDd2QaTSpG4PUwk/wALe3ofwPPX2sjpYepiLVnr0XRv+uh4Wf1cTTw37hadX1S/rdnG7q9U+GngPzxFq2uRfueGt7Zx9/0dh6eg79TTPhj4B87ytY1yL91w9vbOPv8Ao7D09B36mvYK9POc4tfD4d+r/RHlZJkV7YnEr0X6v/I8U+JPgZtKaTVNIjLaeTuliXkwH1H+z/L6V52GxX1c6q6lWAKkYIIyDXiXxL8CNpLSapo8ZbTyd0sK9YPcf7P8vpV5PnHtLYfEPXo+/k/P8/XfLO8i9nfE4ZadV2815fl6bb/w18eC68rSdbl/0j7sFw5/1norH+96Hv8AXr6fXjXwy8Bm9MWr61ERajDQW7D/AFvozD+76Dv9OvsteLnMcPHENYf59r+R72RyxMsMnifl3t5/oFFFFeSeyFFFFABRRRQAUUUUAFMmijmjaOZFkjYYZXGQR7in0UJ2Bq+4UUUUAFIyh1KsAVIwQRwaWigAAAAAGAKKKKACiiigAooooA//2Q=="

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
