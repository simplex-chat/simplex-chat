import {log, logError} from "./util.js"

export interface GrokMessage {
  role: "system" | "user" | "assistant"
  content: string
}

export class GrokApiClient {
  private readonly apiKey: string
  private readonly initialContext: readonly GrokMessage[]

  constructor(apiKey: string, initialContext: readonly GrokMessage[]) {
    this.apiKey = apiKey
    this.initialContext = initialContext
  }

  async chatRaw(messages: GrokMessage[]): Promise<string> {
    const response = await fetch("https://api.x.ai/v1/chat/completions", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${this.apiKey}`,
      },
      body: JSON.stringify({
        model: "grok-latest",
        messages,
        temperature: 0.3,
        max_tokens: 1024,
      }),
      signal: AbortSignal.timeout(60_000),
    })

    if (!response.ok) {
      const body = await response.text()
      logError(`Grok API HTTP ${response.status}`, body)
      throw new Error(`Grok API error: HTTP ${response.status}`)
    }

    const data = await response.json() as {choices: {message: {content: string}}[]}
    const content = data.choices?.[0]?.message?.content
    if (!content) throw new Error("Grok API returned empty response")

    log(`Grok API response: ${content.length} chars`)
    return content
  }

  async chat(history: GrokMessage[], userMessage: string): Promise<string> {
    log(`Grok API call: ${this.initialContext.length} context msgs, ${history.length} history msgs, user msg ${userMessage.length} chars`)
    return this.chatRaw([
      ...this.initialContext,
      ...history,
      {role: "user", content: userMessage},
    ])
  }
}
