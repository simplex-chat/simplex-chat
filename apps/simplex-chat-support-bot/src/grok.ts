import {GrokMessage} from "./state.js"
import {log} from "./util.js"

interface GrokApiMessage {
  role: "system" | "user" | "assistant"
  content: string
}

interface GrokApiResponse {
  choices: {message: {content: string}}[]
}

export class GrokApiClient {
  constructor(private apiKey: string, private docsContext: string) {}

  async chat(history: GrokMessage[], userMessage: string): Promise<string> {
    const messages: GrokApiMessage[] = [
      {role: "system", content: this.systemPrompt()},
      ...history.slice(-20),
      {role: "user", content: userMessage},
    ]
    log(`Grok API call: ${history.length} history msgs + new user msg (${userMessage.length} chars)`)
    const resp = await fetch("https://api.x.ai/v1/chat/completions", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${this.apiKey}`,
      },
      body: JSON.stringify({model: "grok-3", messages, max_tokens: 2048}),
    })
    if (!resp.ok) {
      const body = await resp.text()
      throw new Error(`Grok API ${resp.status}: ${body}`)
    }
    const data = (await resp.json()) as GrokApiResponse
    const content = data.choices[0]?.message?.content
    if (!content) throw new Error("Grok API returned empty response")
    log(`Grok API response: ${content.length} chars`)
    return content
  }

  private systemPrompt(): string {
    return `You are a privacy expert and SimpleX Chat evangelist. You know everything about SimpleX Chat apps, network, design choices, and trade-offs. Be helpful, accurate, and concise. If you don't know something, say so honestly rather than guessing. For every criticism, explain why the team made that design choice.\n\n${this.docsContext}`
  }
}
