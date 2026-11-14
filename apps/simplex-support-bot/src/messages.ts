import {isWeekend} from "./util.js"

export interface Messages {
  welcome: string
  queue: string
  queueGrok: string
  grokActivated: string
  teamAdded: string
  teamAddedGrok: string
  teamAlreadyInvited: string
  teamLocked: string
  noTeamMembers: string
  noTeamMembersGrok: string
  grokInviting: string
  grokUnavailable: string
  grokError: string
  grokNoHistory: string
}

export const defaultMessages: Messages = {
  welcome: `Hello! This is a **SimpleX team** support bot - not an AI.

Discover public groups: [simplex.chat/directory](https://simplex.chat/directory)

Join the livestream about SimpleX roadmap and equity crowdfunding: [simplex.chat/livestream](https://simplex.chat/livestream) (September 15, at 17:00 UTC)`,
  queue: "The team will reply to your message within {hours} hours.",
  queueGrok: `If your question is about SimpleX, click /grok for an **instant Grok answer**.

Send /team to switch back.`,
  grokActivated: "**You are now chatting with Grok** - use any language.",
  teamAdded: "We will reply within {hours} hours.",
  teamAddedGrok: "Grok will be answering your questions until then.",
  teamAlreadyInvited: "A team member was invited to this conversation and will reply when available.",
  teamLocked: "Only the team will now receive your messages.",
  noTeamMembers: "No team members are available yet. Please try again later.",
  noTeamMembersGrok: "No team members are available yet. Please try again later or click /grok.",
  grokInviting: "Inviting Grok, please wait...",
  grokUnavailable: "Grok is temporarily unavailable. Please try again later or send /team for a human team member.",
  grokError: "Sorry, I couldn't process that. Please try again or send /team for a human team member.",
  grokNoHistory: "I just joined but couldn't see your earlier messages. Could you repeat your question?",
}

function withReplyHours(text: string, timezone: string): string {
  return text.replaceAll("{hours}", isWeekend(timezone) ? "48" : "24")
}

export function queueMessage(messages: Messages, timezone: string, grokEnabled: boolean): string {
  const base = withReplyHours(messages.queue, timezone)
  return grokEnabled ? `${base}\n\n${messages.queueGrok}` : base
}

export function teamAddedMessage(messages: Messages, timezone: string, grokPresent: boolean): string {
  const base = withReplyHours(messages.teamAdded, timezone)
  return grokPresent ? `${base}\n${messages.teamAddedGrok}` : base
}

export function noTeamMembersMessage(messages: Messages, grokEnabled: boolean): string {
  return grokEnabled ? messages.noTeamMembersGrok : messages.noTeamMembers
}
