import {isWeekend} from "./util.js"

export const welcomeMessage = `Hello! This is a *SimpleX team* support bot - not an AI.
*Join public groups* at https://simplex.chat/directory or [via directory bot](https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok)
Please ask any questions about SimpleX Chat.`

export function queueMessage(timezone: string, grokEnabled: boolean): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  const base = `The team will reply to your message within ${hours} hours.`
  if (!grokEnabled) return base
  return `${base}

If your question is about SimpleX, click /grok for an *instant Grok answer*.

Send /team to switch back.`
}

export const grokActivatedMessage = `*You are now chatting with Grok* - use any language.`

export function teamAddedMessage(timezone: string, grokPresent: boolean): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  const base = `We will reply within ${hours} hours.`
  if (!grokPresent) return base
  return `${base}
Grok will be answering your questions until then.`
}

export const teamAlreadyInvitedMessage = "A team member was invited to this conversation and will reply when available."

export const teamLockedMessage = "Only the team will now receive your messages."

export function noTeamMembersMessage(grokEnabled: boolean): string {
  return grokEnabled
    ? "No team members are available yet. Please try again later or click /grok."
    : "No team members are available yet. Please try again later."
}

export const grokInvitingMessage = "Inviting Grok, please wait..."

export const grokUnavailableMessage = "Grok is temporarily unavailable. Please try again later or send /team for a human team member."

export const grokErrorMessage = "Sorry, I couldn't process that. Please try again or send /team for a human team member."

export const grokNoHistoryMessage = "I just joined but couldn't see your earlier messages. Could you repeat your question?"
