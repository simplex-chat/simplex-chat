import {isWeekend} from "./util.js"

export const welcomeMessage = `Hello! Feel free to ask any question about SimpleX Chat.
*Only SimpleX Chat team has access to your messages.* This is a SimpleX Chat team bot - it is not any LLM or AI.
Please send questions in English, you can use translator.`

export function queueMessage(timezone: string, grokEnabled: boolean): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  const base = `The team can see your message. A reply may take up to ${hours} hours.`
  if (!grokEnabled) return base
  return `${base}

If your question is about SimpleX Chat, click /grok for an instant AI answer (non-sensitive questions only). Click /team to switch back any time.`
}

export const grokActivatedMessage = `*You are now chatting with Grok. You can send questions in any language.* Grok can see your earlier messages.
Send /team at any time to switch to a human team member.`

export function teamAddedMessage(timezone: string): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  return `A team member has been added and will reply within ${hours} hours. You can keep describing your issue - they will see the full conversation.`
}

export const teamAlreadyInvitedMessage = "A team member has already been invited to this conversation and will reply when available."

export const teamLockedMessage = "You are now in team mode. A team member will reply to your message."

export function noTeamMembersMessage(grokEnabled: boolean): string {
  return grokEnabled
    ? "No team members are available yet. Please try again later or click /grok."
    : "No team members are available yet. Please try again later."
}

export const grokInvitingMessage = "Inviting Grok, please wait..."

export const grokUnavailableMessage = "Grok is temporarily unavailable. Please try again later or send /team for a human team member."

export const grokErrorMessage = "Sorry, I couldn't process that. Please try again or send /team for a human team member."

export const grokNoHistoryMessage = "I just joined but couldn't see your earlier messages. Could you repeat your question?"
