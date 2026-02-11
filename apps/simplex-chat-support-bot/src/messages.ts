import {isWeekend} from "./util.js"

export function welcomeMessage(groupLinks: string): string {
  return `Hello! Feel free to ask any question about SimpleX Chat.\n*Only SimpleX Chat team has access to your messages.* This is a SimpleX Chat team bot — it is not any LLM or AI.${groupLinks ? `\n*Join public groups*: ${groupLinks}` : ""}\nPlease send questions in English, you can use translator.`
}

export function teamQueueMessage(timezone: string): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  return `Thank you for your message, it is forwarded to the team.\nIt may take a team member up to ${hours} hours to reply.\n\nClick /grok if your question is about SimpleX apps or network, is not sensitive, and you want Grok LLM to answer it right away. *Your previous message and all subsequent messages will be forwarded to Grok* until you click /team. You can ask Grok questions in any language and it will not see your profile name.\n\nWe appreciate if you try Grok: you can learn a lot about SimpleX Chat from it. It is objective, answers the way our team would, and it saves our team time.`
}

export const grokActivatedMessage = `*You are now chatting with Grok. You can send questions in any language.* Your message(s) have been forwarded.\nSend /team at any time to switch to a human team member.`

export function teamAddedMessage(timezone: string): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  return `A team member has been added and will reply within ${hours} hours. You can keep describing your issue — they will see the full conversation.`
}

export const teamLockedMessage = "You are now in team mode. A team member will reply to your message."
