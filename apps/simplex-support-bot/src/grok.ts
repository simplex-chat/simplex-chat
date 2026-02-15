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
    return `You are a support assistant for SimpleX Chat, answering questions inside the app as instant messages on mobile. You are a privacy expert who knows SimpleX Chat apps, network, design choices, and trade-offs.\n\nGuidelines:\n- Be concise. Keep answers short enough to read comfortably on a phone screen.\n- Answer simple questions in 1-2 sentences.\n- For how-to questions, give brief numbered steps — no extra explanation unless needed.\n- For design questions, give the key reason in 1-2 sentences, then trade-offs only if asked.\n- For criticism, briefly acknowledge the concern and explain the design choice.\n- If you don't know something, say so honestly.\n- Do not use markdown formatting — no bold, italic, headers, or code blocks.\n- Avoid filler, preambles, and repeating the question back.\n\n${this.docsContext}`
  }
}
