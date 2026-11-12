import {mkdtempSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {AudioOptions, speech, transcript} from "../../src/audio"
import {AudioServer, audioServer, formFields, until} from "./fixtures"

let server: AudioServer
let audio: AudioOptions
let folder: string

beforeEach(async () => {
  server = await audioServer()
  audio = {url: server.url, stt: "stt", tts: "tts"}
  folder = mkdtempSync(join(tmpdir(), "simplex-mcp-audio-"))
})

afterEach(async () => {
  jest.restoreAllMocks()
  await server.close()
  rmSync(folder, {recursive: true, force: true})
})

function voiceFile(name: string): string {
  const path = join(folder, name)
  writeFileSync(path, `audio of ${name}`)
  return path
}

describe("transcript", () => {
  it("posts the model and the file with its name only, and returns the trimmed text", async () => {
    server.replies.set("/v1/audio/transcriptions", {body: JSON.stringify({text: " Grüezi. \n"})})
    expect(await transcript(audio, voiceFile("7.m4a"))).toBe("Grüezi.")
    expect(server.requests.map(({path, type}) => [path, type.split(";")[0]])).toEqual([["/v1/audio/transcriptions", "multipart/form-data"]])
    expect(await formFields(server.requests[0])).toEqual({model: "stt", file: {name: "7.m4a", content: "audio of 7.m4a"}})
  })

  it("fails with the HTTP status, a response without text, a redirect, a refused connection and a timeout", async () => {
    const failure = (path: string): Promise<unknown> => transcript(audio, path).then(() => undefined, (error: Error) => error.message)
    server.replies.set("/v1/audio/transcriptions", {status: 500, body: "{}"})
    expect(await failure(voiceFile("7.m4a"))).toBe("Speech-to-text failed: HTTP 500.")
    server.replies.set("/v1/audio/transcriptions", {body: JSON.stringify({segments: []})})
    expect(await failure(voiceFile("7.m4a"))).toBe("Speech-to-text failed: no text.")
    server.replies.set("/v1/audio/transcriptions", {status: 307, headers: {location: "/elsewhere"}})
    expect(await failure(voiceFile("7.m4a"))).toBe("Speech-to-text failed: fetch failed.")
    expect(server.requests.map(({path}) => path)).not.toContain("/elsewhere")
    const timeout = AbortSignal.timeout
    jest.spyOn(AbortSignal, "timeout").mockImplementation(() => timeout(50))
    server.replies.set("/v1/audio/transcriptions", {hold: new Promise(() => undefined)})
    expect(await failure(voiceFile("7.m4a"))).toBe("Speech-to-text failed: The operation was aborted due to timeout.")
    const closed = await audioServer()
    await closed.close()
    audio = {...audio, url: closed.url}
    expect(await failure(voiceFile("7.m4a"))).toBe("Speech-to-text failed: fetch failed.")
  })

  it("posts a file of 1 500 000 bytes, and refuses a larger one without a request", async () => {
    server.replies.set("/v1/audio/transcriptions", {body: JSON.stringify({text: "Hoi."})})
    const path = join(folder, "7.m4a")
    writeFileSync(path, Buffer.alloc(1_500_001))
    await expect(transcript(audio, path)).rejects.toThrow(new Error("Speech-to-text failed: the file has more than 1500000 bytes."))
    writeFileSync(path, Buffer.alloc(1_500_000))
    expect(await transcript(audio, path)).toBe("Hoi.")
    expect(server.requests).toHaveLength(1)
  })

  it("sends one request at a time, in the order of the calls", async () => {
    let release = (): void => undefined
    server.replies.set("/v1/audio/transcriptions", {body: JSON.stringify({text: "Hoi."}), hold: new Promise<void>(resolve => (release = resolve))})
    const first = transcript(audio, voiceFile("7.m4a"))
    const second = transcript(audio, voiceFile("8.m4a"))
    await until(() => server.requests.length === 1)
    await new Promise(resolve => setTimeout(resolve, 100))
    expect(server.requests).toHaveLength(1)
    release()
    expect(await Promise.all([first, second])).toEqual(["Hoi.", "Hoi."])
    expect((await Promise.all(server.requests.map(formFields))).map(({file}) => file)).toEqual([
      {name: "7.m4a", content: "audio of 7.m4a"},
      {name: "8.m4a", content: "audio of 8.m4a"},
    ])
  })
})

describe("speech", () => {
  it("posts the model, the text and the WAV format, with the voice only when it is set, returns the audio, and fails with the HTTP status or at a redirect", async () => {
    server.replies.set("/v1/audio/speech", {body: Buffer.from("RIFF wav")})
    expect((await speech(audio, "Grüezi.")).toString()).toBe("RIFF wav")
    expect((await speech({...audio, voice: "ana"}, "Hoi.")).toString()).toBe("RIFF wav")
    expect(server.requests.map(({path, type, body}) => [path, type, JSON.parse(body.toString())])).toEqual([
      ["/v1/audio/speech", "application/json", {model: "tts", input: "Grüezi.", response_format: "wav"}],
      ["/v1/audio/speech", "application/json", {model: "tts", input: "Hoi.", response_format: "wav", voice: "ana"}],
    ])
    server.replies.set("/v1/audio/speech", {status: 503})
    await expect(speech(audio, "Grüezi.")).rejects.toThrow(new Error("Text-to-speech failed: HTTP 503."))
    server.replies.set("/v1/audio/speech", {status: 308, headers: {location: "/elsewhere"}})
    await expect(speech(audio, "Grüezi.")).rejects.toThrow(new Error("Text-to-speech failed: fetch failed."))
    expect(server.requests.map(({path}) => path)).not.toContain("/elsewhere")
  })
})
