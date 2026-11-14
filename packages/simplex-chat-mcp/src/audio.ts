import {readFileSync} from "fs"
import {readFile, writeFile} from "fs/promises"
import {IncomingMessage, request as httpRequest} from "http"
import {request as httpsRequest} from "https"
import {basename} from "path"
import {buffer} from "stream/consumers"
import {errorText, logError} from "./log"

// spec: spec/architecture.md#audio-audiooptions
export interface AudioOptions {
  url: string
  stt: string
  tts: string
  voice?: string
}

// spec: spec/architecture.md#audio-transcript-type
export interface Transcript {
  transcript?: string
  transcript_error?: string
}

// spec: spec/architecture.md#audio-maxvoicebytes
export const maxVoiceBytes = 1_500_000
const requestTimeout = 10 * 60_000
let transcriptions: Promise<unknown> = Promise.resolve()

// spec: spec/architecture.md#audio-transcript
export function transcript(audio: AudioOptions, path: string): Promise<string> {
  const turn = transcriptions.then(() => transcribe(audio, path))
  transcriptions = turn.catch(() => undefined)
  return turn
}

// spec: spec/architecture.md#audio-speech
export function speech(audio: AudioOptions, input: string): Promise<Buffer> {
  const body = JSON.stringify({model: audio.tts, input, response_format: "wav", voice: audio.voice})
  return post("Text-to-speech", `${audio.url}/v1/audio/speech`, {headers: {"content-type": "application/json"}, body}, wav => wav)
}

// spec: spec/architecture.md#audio-writetranscript
export async function writeTranscript(audio: AudioOptions, fileId: number, path: string): Promise<void> {
  const result: Transcript = await transcript(audio, path).then(
    text => ({transcript: text}),
    error => {
      logError(`transcription of file ${fileId}`, error)
      return {transcript_error: errorText(error)}
    },
  )
  await writeFile(`${path}.transcript.json`, JSON.stringify(result), {mode: 0o600}).catch(error => logError(`transcription of file ${fileId}`, error))
}

// spec: spec/architecture.md#audio-transcriptof
export function transcriptOf(path: string): Transcript {
  try {
    const {transcript, transcript_error} = JSON.parse(readFileSync(`${path}.transcript.json`, "utf8"))
    return typeof transcript === "string" ? {transcript} : typeof transcript_error === "string" ? {transcript_error} : {}
  } catch {
    return {}
  }
}

// spec: spec/architecture.md#audio-transcribe
async function transcribe(audio: AudioOptions, path: string): Promise<string> {
  const bytes = await readFile(path)
  if (bytes.length > maxVoiceBytes) throw new Error(`Speech-to-text failed: the file has more than ${maxVoiceBytes} bytes.`)
  const form = new FormData()
  form.append("model", audio.stt)
  form.append("file", new Blob([bytes]), basename(path))
  return post("Speech-to-text", `${audio.url}/v1/audio/transcriptions`, {body: form}, json => {
    const {text} = JSON.parse(json.toString()) as {text?: unknown}
    if (typeof text !== "string") throw new Error("no text")
    return text.trim()
  })
}

// spec: spec/architecture.md#audio-post
async function post<R>(service: string, url: string, init: RequestInit, read: (body: Buffer) => R): Promise<R> {
  const signal = AbortSignal.timeout(requestTimeout)
  try {
    const response = await send(new Request(url, {...init, method: "POST"}), signal)
    const body = await buffer(response)
    if (response.statusCode! < 200 || response.statusCode! > 299) throw new Error(`HTTP ${response.statusCode}`)
    return read(body)
  } catch (error) {
    const cause = (signal.reason ?? error) as NodeJS.ErrnoException
    throw new Error(`${service} failed: ${cause.message || cause.code}.`)
  }
}

// spec: spec/architecture.md#audio-send
async function send(request: Request, signal: AbortSignal): Promise<IncomingMessage> {
  const body = Buffer.from(await request.arrayBuffer())
  const open = request.url.startsWith("https:") ? httpsRequest : httpRequest
  return new Promise((resolve, reject) => open(request.url, {agent: false, method: request.method, headers: Object.fromEntries(request.headers), signal}, resolve).on("error", reject).end(body))
}
