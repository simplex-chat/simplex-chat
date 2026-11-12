import {readFileSync} from "fs"
import {readFile, writeFile} from "fs/promises"
import {basename} from "path"
import {errorText, logError} from "./log"

export interface AudioOptions {
  url: string
  stt: string
  tts: string
  voice?: string
}

export interface Transcript {
  transcript?: string
  transcript_error?: string
}

export const maxVoiceBytes = 1_500_000
const requestTimeout = 10 * 60_000
let transcriptions: Promise<unknown> = Promise.resolve()

export function transcript(audio: AudioOptions, path: string): Promise<string> {
  const turn = transcriptions.then(() => transcribe(audio, path))
  transcriptions = turn.catch(() => undefined)
  return turn
}

export function speech(audio: AudioOptions, input: string): Promise<Buffer> {
  const body = JSON.stringify({model: audio.tts, input, response_format: "wav", voice: audio.voice})
  return post("Text-to-speech", `${audio.url}/v1/audio/speech`, {headers: {"content-type": "application/json"}, body}, async response => Buffer.from(await response.arrayBuffer()))
}

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

export function transcriptOf(path: string): Transcript {
  try {
    const {transcript, transcript_error} = JSON.parse(readFileSync(`${path}.transcript.json`, "utf8"))
    return typeof transcript === "string" ? {transcript} : typeof transcript_error === "string" ? {transcript_error} : {}
  } catch {
    return {}
  }
}

async function transcribe(audio: AudioOptions, path: string): Promise<string> {
  const bytes = await readFile(path)
  if (bytes.length > maxVoiceBytes) throw new Error(`Speech-to-text failed: the file has more than ${maxVoiceBytes} bytes.`)
  const form = new FormData()
  form.append("model", audio.stt)
  form.append("file", new Blob([bytes]), basename(path))
  return post("Speech-to-text", `${audio.url}/v1/audio/transcriptions`, {body: form}, async response => {
    const {text} = (await response.json()) as {text?: unknown}
    if (typeof text !== "string") throw new Error("no text")
    return text.trim()
  })
}

async function post<R>(service: string, url: string, init: RequestInit, read: (response: Response) => Promise<R>): Promise<R> {
  try {
    const response = await fetch(url, {...init, method: "POST", redirect: "error", signal: AbortSignal.timeout(requestTimeout)})
    if (!response.ok) throw new Error(`HTTP ${response.status}`)
    return await read(response)
  } catch (error) {
    throw new Error(`${service} failed: ${(error as Error).message}.`)
  }
}
