export interface GrokMessage {
  role: "user" | "assistant"
  content: string
}

export type ConversationState =
  | {type: "welcome"}
  | {type: "teamQueue"; userMessages: string[]}
  | {type: "grokMode"; grokMemberGId: number; history: GrokMessage[]}
  | {type: "teamPending"; teamMemberGId: number}
  | {type: "teamLocked"; teamMemberGId: number}
