import {log, logError} from "./util.js"

export interface GrokMessage {
  role: "system" | "user" | "assistant"
  content: string
}

export class GrokApiClient {
  private readonly apiKey: string
  private readonly systemPrompt: string

  constructor(apiKey: string, systemPrompt: string) {
    this.apiKey = apiKey
    this.systemPrompt = systemPrompt
  }

  async chatRaw(messages: GrokMessage[]): Promise<string> {
    const response = await fetch("https://api.x.ai/v1/chat/completions", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${this.apiKey}`,
      },
      body: JSON.stringify({
        model: "grok-3-mini",
        messages,
        temperature: 0.3,
        max_tokens: 1024,
      }),
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
    log(`Grok API call: ${history.length} history msgs, user msg ${userMessage.length} chars`)
    return this.chatRaw([
      {role: "system", content: this.systemPrompt},
      ...history,
      {role: "user", content: userMessage},
    ])
  }
}
