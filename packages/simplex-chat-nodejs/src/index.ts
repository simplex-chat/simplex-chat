/**
 * A simple declarative API to run a chat-bot with a single function call.
 * It automates creating and updating of the bot profile, address and bot commands shown in the app UI.
 */
export * as bot from "./bot"

/**
 * An API to send chat commands and receive chat events to/from chat core.
 * You need to use it in bot event handlers, and for any other use cases.
 */
export * as api from "./api"

/**
 * A low level API to the core library - the same that is used in desktop clients.
 * You are unlikely to ever need to use this module directly.
 */
export * as core from "./core"

/**
 * Useful functions for chat events and types.
 */
export * as util from "./util"
