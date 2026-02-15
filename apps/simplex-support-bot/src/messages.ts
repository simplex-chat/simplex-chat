import {isWeekend} from "./util.js"

export function welcomeMessage(groupLinks: string): string {
  return `Hello! Feel free to ask any question about SimpleX Chat.\n*Only SimpleX Chat team has access to your messages.* This is a SimpleX Chat team bot — it is not any LLM or AI.${groupLinks ? `\n*Join public groups*: ${groupLinks}` : ""}\nPlease send questions in English, you can use translator.`
}

export function teamQueueMessage(timezone: string): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  return `Your message is forwarded to the team. A reply may take up to ${hours} hours.\n\nIf your question is about SimpleX Chat, click /grok for an instant AI answer (non-sensitive questions only). Click /team to switch back any time.`
}

export const grokActivatedMessage = `*You are now chatting with Grok. You can send questions in any language.* Your message(s) have been forwarded.\nSend /team at any time to switch to a human team member.`

export function teamAddedMessage(timezone: string): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  return `The team was notified. Somebody will reply within ${hours} hours. Please send your question or problem description - it will be visible to the team.`
}

export const teamLockedMessage = "You are now in team mode. A team member will reply to your message."
