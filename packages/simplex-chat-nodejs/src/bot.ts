import {T} from "@simplex-chat/types"
import * as api from "./api"
import * as core from "./core"
import * as util from "./util"
import equal = require("fast-deep-equal")

export interface BotDbOpts {
  dbFilePrefix: string // two schema files will be named <prefix>_chat.db and <prefix>_agent.db
  dbKey?: string
  confirmMigrations?: core.MigrationConfirmation
}

export interface BotOptions {
  createAddress?: boolean
  updateAddress?: boolean
  updateProfile?: boolean
  addressSettings?: api.BotAddressSettings
  allowFiles?: boolean
  commands?: T.ChatBotCommand[] // commands to show in client UI
  useBotProfile?: boolean // create profile not marked as a bot, with default preferences  
  logContacts?: boolean
  logNetwork?: boolean
}

const defaultOpts: Required<BotOptions> = {
  createAddress: true,
  updateAddress: true,
  updateProfile: true,
  addressSettings: api.defaultBotAddressSettings,
  allowFiles: false,
  commands: [],
  useBotProfile: true,
  logContacts: true,
  logNetwork: false
}

export interface BotConfig {
  profile: T.Profile,
  dbOpts: BotDbOpts,
  options: BotOptions,
  onMessage?: (chatItem: T.AChatItem, content: T.MsgContent) => void | Promise<void>,
  // command handlers can be different from commands to be shown in client UI
  onCommands?: {[K in string]?: ((chatItem: T.AChatItem, command: util.BotCommand) => void | Promise<void>)},
  // If you use `onMessage` and to subscribe "newChatItems" event, exclude content messages from processing
  // If you use `onCommands` and to subscribe "newChatItems" event, exclude commands from processing
  events?: api.EventSubscribers
}

export async function run({profile, dbOpts, options = defaultOpts, onMessage, onCommands = {}, events = {}}: BotConfig): Promise<[api.ChatApi, T.User]> {
  const bot = await api.ChatApi.init(dbOpts.dbFilePrefix, dbOpts.dbKey || "", dbOpts.confirmMigrations || core.MigrationConfirmation.YesUp)
  const opts = fullOptions(options)
  if (onMessage) subscribeMessages(bot, onMessage)
  if (Object.keys(onCommands).length > 0) subscribeCommands(bot, onCommands)
  if (Object.keys(events).length > 0) bot.on(events)
  subscribeLogEvents(bot, opts)
  const botProfile = mkBotProfile(profile, opts)
  const user = await createBotUser(bot, botProfile)
  await bot.startChat()
  const address = await createOrUpdateAddress(bot, user, opts)  
  const addressLink = util.contactAddressStr(address.connLinkContact)
  console.log(`Bot address: ${addressLink}`)
  if (opts.useBotProfile) botProfile.contactLink = addressLink
  await updateBotUserProfile(bot, user, botProfile, opts)  
  return [bot, user]
}

function fullOptions(options: BotOptions): Required<BotOptions> {
  const opts =  {
    createAddress: options.createAddress ?? defaultOpts.createAddress,
    updateAddress: options.updateAddress ?? defaultOpts.updateAddress,
    updateProfile: options.updateProfile ?? defaultOpts.updateProfile,
    addressSettings: options.addressSettings ?? defaultOpts.addressSettings,
    allowFiles: options.allowFiles ?? defaultOpts.allowFiles,
    commands: options.commands ?? defaultOpts.commands,
    useBotProfile: options.useBotProfile ?? defaultOpts.useBotProfile,
    logContacts: options.logContacts ?? defaultOpts.logContacts,
    logNetwork: options.logNetwork ?? defaultOpts.logNetwork
  }
  const welcomeMessage = opts.addressSettings.welcomeMessage ?? defaultOpts.addressSettings.welcomeMessage
  opts.addressSettings = {
    autoAccept: opts.addressSettings.autoAccept ?? defaultOpts.addressSettings.autoAccept,
    welcomeMessage: typeof welcomeMessage === "string" ? {type: "text", text: welcomeMessage} : welcomeMessage,
    businessAddress: opts.addressSettings.businessAddress ?? defaultOpts.addressSettings.businessAddress
  }
  return opts
}

function mkBotProfile(profile: T.Profile, opts: Required<BotOptions>): T.Profile {
  if (opts.useBotProfile) {
    const prefs = profile.preferences || {}
    if (prefs.files || prefs.calls || prefs.voice || prefs.commands) {
      console.log("Option useBotProfile is enabled and profile preferences used for files, calls, voice or commands, exiting")
      process.exit()
    }
    prefs.files = {allow: opts.allowFiles ? T.FeatureAllowed.Yes : T.FeatureAllowed.No}
    prefs.calls = {allow: T.FeatureAllowed.No}
    prefs.voice = {allow: T.FeatureAllowed.No}
    prefs.commands = opts.commands
    profile.preferences = prefs
    profile.peerType = T.ChatPeerType.Bot
  } else if (opts.commands.length > 0) {
      console.log("Option useBotProfile is disabled and commands are passed, exiting")
      process.exit()
  }
  return profile 
}

function subscribeMessages(bot: api.ChatApi, onMessage: (chatItem: T.AChatItem, content: T.MsgContent) => void | Promise<void>) {
  bot.on("newChatItems", async ({chatItems}) => {
    for (const ci of chatItems) {
      if (ci.chatItem.content.type === "rcvMsgContent") {
        try {
          const p = onMessage(ci, ci.chatItem.content.msgContent)
          if (p instanceof Promise) await p
        } catch (e) {
          console.log(`message processing error`, e)
        }
      }
    }
  })
}

function subscribeCommands(bot: api.ChatApi, commands: {[K in string]?: ((chatItem: T.AChatItem, command: util.BotCommand) => void | Promise<void>)}) {
  bot.on("newChatItems", async (evt) => {
    for (const ci of evt.chatItems) {
      const cmd = util.ciBotCommand(ci.chatItem)
      if (cmd) {
        const cmdFunc = commands[cmd.keyword] || commands[""]
        if (cmdFunc) {
          try {
            const p = cmdFunc(ci, cmd)
            if (p instanceof Promise) await p
          } catch(e) {
            console.log(`${cmd} command processing error`, e)
          }
        }
      }
    }
  })    
}

function subscribeLogEvents(bot: api.ChatApi, opts: Required<BotOptions>) {
  if (opts.logContacts) {
    bot.on({
      "contactConnected": ({contact}) => console.log(`${contact.profile.displayName} connected`),
      "contactDeletedByContact": ({contact}) => console.log(`${contact.profile.displayName} deleted connection with bot`)
    })      
  }
  if (opts.logNetwork) {
    bot.on({
      "hostConnected": ({transportHost}) => console.log(`connected server ${transportHost}`),
      "hostDisconnected": ({transportHost}) => console.log(`diconnected server ${transportHost}`),
      "subscriptionStatus": ({subscriptionStatus, connections}) => console.log(`${connections.length} subscription(s) ${subscriptionStatus.type}`)
    })          
  }
}

async function createBotUser(bot: api.ChatApi, profile: T.Profile): Promise<T.User> {
  let user = await bot.apiGetActiveUser()
  if (!user) {
    console.log("No active user in dabase, creating...")
    user = await bot.apiCreateActiveUser(profile)
  }
  console.log("Bot user: ", user.profile.displayName)
  return user    
}
    
async function createOrUpdateAddress(bot: api.ChatApi, user: T.User, opts: Required<BotOptions>): Promise<T.UserContactLink> {
  const {userId} = user
  let address = await bot.apiGetUserAddress(userId)
  if (!address) {
    if (opts.createAddress) {
      console.log("Bot has no address, creating...")
      await bot.apiCreateUserAddress(userId)
      address = await bot.apiGetUserAddress(userId)
      if (!address) {
        console.log("Failed reading created user address, exiting")
        process.exit()
      }
    } else {
      console.log("Bot has no address, exiting")
      process.exit()
    }
  }
  
  const addressSettings = opts.addressSettings || defaultOpts.addressSettings
  if (!equal(util.botAddressSettings(address), addressSettings)) {
    console.log(util.botAddressSettings(address), addressSettings)
    if (opts.updateAddress) {
      console.log("Bot address settings changed, updating...")
      await bot.apiSetAddressSettings(userId, addressSettings)
    } else {
      console.log("Bot address settings changed")
    }
  }
  
  return address    
}

async function updateBotUserProfile(bot: api.ChatApi, user: T.User, profile: T.Profile, opts: Required<BotOptions>): Promise<void> {
  const {userId} = user
  if (!equal(util.fromLocalProfile(user.profile), profile)) {
    if (opts.updateProfile) {
      console.log("Bot profile changed, updating...")
      const summary = await bot.apiUpdateProfile(userId, profile)
      console.log(
        summary
        ? `Bot profile updated: ${summary.updateSuccesses} updated contact(s), ${summary.updateFailures} failed contact update(s).`
        : "Unexpected: profile did not change!"
      )
    } else {
      console.log("Bot profile changed")
    }
  }  
}
