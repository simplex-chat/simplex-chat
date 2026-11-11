import {oneLine} from "./log"

export interface Endpoint {
  url: string
  key: string | undefined
}

export type InferenceState = {type: "loading"; model?: string} | {type: "prompt"; model?: string; tokens: number} | {type: "output"; model?: string; rate?: number} | {type: "idle"}

interface Slot {
  proxy: string
  model?: string
  id?: number
  task?: number
  processing: boolean
  decoded: number
  prompt: number
  at: number
}

type Sample = Extract<InferenceState, {type: "loading"}> | {type: "slots"; slots: Slot[]}

const deadline = 1_000
const maxBody = 1_048_576
const maxProxies = 4
const maxSlots = 64
const modelPattern = /^[\w.:/+@-]{1,64}$/
const loopback = ["localhost", "127.0.0.1", "[::1]"]
const largeBody = "body over 1 MiB"

export function inferencePoller(endpoint: Endpoint, reuse = 3_000): () => Promise<InferenceState | undefined> {
  let latest: {at: number; promise: Promise<InferenceState | undefined>} | undefined
  let previous: Slot[] = []
  return () => {
    const now = Date.now()
    if (latest !== undefined && now - latest.at < reuse) return latest.promise
    const promise = sample(endpoint, AbortSignal.timeout(deadline)).then(polled => {
      if (polled?.type !== "slots") {
        previous = []
        return polled
      }
      const state = stateOf(polled.slots, previous)
      previous = polled.slots
      return state
    })
    latest = {at: now, promise}
    return promise
  }
}

async function sample(endpoint: Endpoint, signal: AbortSignal): Promise<Sample | undefined> {
  const {url, key} = endpoint
  const running = await get(`${url}/running`, key, signal, 404)
  if (running.status === 404) return proxySlots(endpoint, signal)
  const models = listOf(fieldOf(running.json, "running")).map(entry => ({model: textOf(entry, "model", modelPattern), state: fieldOf(entry, "state"), proxy: fieldOf(entry, "proxy")}))
  const starting = models.find(({state}) => state === "starting")
  if (starting !== undefined) return {type: "loading", model: starting.model}
  const ready = models.filter(({state}) => state === "ready")
  if (ready.length === 0) return models.some(({state}) => state === "stopping") ? {type: "loading"} : undefined
  const proxies = new Map<string, string | undefined>()
  for (const {model, proxy} of ready) {
    const allowed = allowedProxy(url, proxy)
    if (allowed !== undefined && !proxies.has(allowed) && proxies.size < maxProxies) proxies.set(allowed, model)
  }
  if (proxies.size === 0) return undefined
  const reads = await Promise.allSettled([...proxies].map(([proxy, model]) => proxySlots({url: proxy, key: undefined}, signal, model)))
  const samples = reads.flatMap(read => (read.status === "fulfilled" ? [read.value] : []))
  if (samples.length === 0) throw (reads[0] as PromiseRejectedResult).reason
  const slots: Slot[] = []
  for (const sampled of samples) {
    if (sampled.type === "loading") return sampled
    slots.push(...sampled.slots)
  }
  return {type: "slots", slots}
}

async function proxySlots({url, key}: Endpoint, signal: AbortSignal, model?: string): Promise<Sample> {
  const props = await get(`${url}/props`, key, signal, 503)
  if (props.status === 503) return {type: "loading", model}
  if (fieldOf(props.json, "is_sleeping") === true) return {type: "slots", slots: []}
  const slots = await get(`${url}/slots`, key, signal, 503)
  if (slots.status === 503) return {type: "loading", model}
  const at = Date.now()
  return {type: "slots", slots: listOf(slots.json).slice(0, maxSlots).map(slot => slotOf(slot, url, model, at))}
}

function slotOf(slot: unknown, proxy: string, model: string | undefined, at: number): Slot {
  const decoded = countOf(fieldOf(listOf(fieldOf(slot, "next_token"))[0], "n_decoded")) ?? 0
  return {proxy, model, id: countOf(fieldOf(slot, "id")), task: countOf(fieldOf(slot, "id_task")), processing: fieldOf(slot, "is_processing") === true, decoded, prompt: countOf(fieldOf(slot, "n_prompt_tokens_processed")) ?? 0, at}
}

function stateOf(slots: Slot[], previous: Slot[]): InferenceState {
  const processing = slots.filter(({processing}) => processing)
  if (processing.length === 0) return {type: "idle"}
  const model = processing.every(slot => slot.model === processing[0].model) ? processing[0].model : undefined
  const decoding = processing.filter(({decoded}) => decoded > 0)
  if (decoding.length === 0) return {type: "prompt", model, tokens: processing.reduce((sum, {prompt}) => sum + prompt, 0)}
  return {type: "output", model, rate: rateOf(decoding, previous)}
}

function rateOf(decoding: Slot[], previous: Slot[]): number | undefined {
  const rate = decoding.reduce((sum, slot) => {
    const before = previous.find(old => old.proxy === slot.proxy && old.id === slot.id && old.task === slot.task && old.processing && old.decoded > 0 && old.decoded <= slot.decoded && old.at < slot.at)
    return before === undefined ? sum : sum + ((slot.decoded - before.decoded) * 1000) / (slot.at - before.at)
  }, 0)
  return rate > 0 ? rate : undefined
}

async function get(url: string, key: string | undefined, signal: AbortSignal, handled: number): Promise<{status: number; json?: unknown}> {
  try {
    const response = await fetch(url, {headers: key === undefined ? {} : {Authorization: `Bearer ${key}`}, redirect: "error", signal})
    if (response.status === handled) return {status: handled}
    if (!response.ok) throw new Error(`HTTP ${response.status}`)
    return {status: response.status, json: await bodyJson(response)}
  } catch (error) {
    throw new Error(oneLine(`GET ${url}: ${signal.aborted ? `no answer within ${deadline / 1000} s` : failureText(error)}`))
  }
}

async function bodyJson(response: Response): Promise<unknown> {
  if (Number(response.headers.get("content-length")) > maxBody) throw new Error(largeBody)
  const chunks: Uint8Array[] = []
  let bytes = 0
  for await (const chunk of response.body ?? []) {
    bytes += chunk.byteLength
    if (bytes > maxBody) throw new Error(largeBody)
    chunks.push(chunk)
  }
  try {
    return JSON.parse(Buffer.concat(chunks).toString())
  } catch {
    throw new Error("invalid JSON")
  }
}

function failureText(error: unknown): string {
  const cause = fieldOf(error, "cause") ?? error
  const first = listOf(fieldOf(cause, "errors"))[0] ?? cause
  const message = fieldOf(first, "message")
  return typeof message === "string" && message !== "" ? message : String(first)
}

function allowedProxy(url: string, proxy: unknown): string | undefined {
  const target = typeof proxy === "string" && URL.canParse(proxy) ? new URL(proxy) : undefined
  if (target === undefined || !["http:", "https:"].includes(target.protocol) || target.username !== "" || target.password !== "") return undefined
  const {hostname} = new URL(url)
  return target.hostname === hostname || (loopback.includes(target.hostname) && loopback.includes(hostname)) ? target.origin : undefined
}

export function textOf(value: unknown, key: string, pattern?: RegExp): string | undefined {
  const field = fieldOf(value, key)
  return typeof field === "string" && (pattern === undefined || pattern.test(field)) ? field : undefined
}

export function fieldOf(value: unknown, key: string): unknown {
  return typeof value === "object" && value !== null ? (value as {[key: string]: unknown})[key] : undefined
}

function listOf(value: unknown): unknown[] {
  return Array.isArray(value) ? value : []
}

function countOf(value: unknown): number | undefined {
  return typeof value === "number" && Number.isSafeInteger(value) && value >= 0 ? value : undefined
}
