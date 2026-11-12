import {spawnSync} from "child_process"
import {chmodSync, existsSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {ModelEntry, acquireLock, createDataFolder, modelAtStart, readServerFile, releaseLock, serverToken, writeHeadersFile, writeModelFile, writeServerFile} from "../../src/files"
import {ExitError} from "../../src/log"

let dir: string

beforeEach(() => {
  dir = join(mkdtempSync(join(tmpdir(), "simplex-mcp-files-")), "agent")
  createDataFolder(dir)
})

afterEach(() => rmSync(join(dir, ".."), {recursive: true, force: true}))

describe("lock", () => {
  it("is created exclusively with the PID and removed", () => {
    acquireLock(dir)
    expect(readFileSync(join(dir, "lock"), "utf8")).toBe(`${process.pid}\n`)
    expect(statSync(join(dir, "lock")).mode & 0o777).toBe(0o600)
    releaseLock(dir)
    expect(existsSync(join(dir, "lock"))).toBe(false)
  })

  it("refuses a lock of a running process", () => {
    const runningPid = process.ppid
    writeFileSync(join(dir, "lock"), `${runningPid}\n`)
    expect(() => acquireLock(dir)).toThrow(new ExitError(`Another server uses ${dir} (PID ${runningPid}). If no server runs, remove ${join(dir, "lock")}.`, 1))
  })

  it("replaces a lock of a stopped process or of this process", () => {
    const exited = spawnSync(process.execPath, ["-e", "process.stdout.write(String(process.pid))"], {encoding: "utf8"})
    writeFileSync(join(dir, "lock"), `${exited.stdout}\n`)
    acquireLock(dir)
    expect(readFileSync(join(dir, "lock"), "utf8")).toBe(`${process.pid}\n`)
    acquireLock(dir)
    expect(readFileSync(join(dir, "lock"), "utf8")).toBe(`${process.pid}\n`)
  })
})

describe("data folder", () => {
  it("is private", () => {
    expect(statSync(dir).mode & 0o777).toBe(0o700)
  })

  it("is made private again at every start", () => {
    chmodSync(dir, 0o755)
    createDataFolder(dir)
    expect(statSync(dir).mode & 0o777).toBe(0o700)
  })

  it("writes the headers for the MCP host, readable only by the user, and rewrites them", () => {
    writeHeadersFile(dir, "first")
    expect(readFileSync(join(dir, "headers.json"), "utf8")).toBe('{"Authorization":"Bearer first"}\n')
    expect(statSync(join(dir, "headers.json")).mode & 0o777).toBe(0o600)
    writeHeadersFile(dir, "second")
    expect(JSON.parse(readFileSync(join(dir, "headers.json"), "utf8"))).toEqual({Authorization: "Bearer second"})
  })

  it("keeps the token until renewed", () => {
    const token = serverToken(dir, false)
    expect(token).toMatch(/^[A-Za-z0-9_-]{43}$/)
    expect(statSync(join(dir, "token")).mode & 0o777).toBe(0o600)
    expect(serverToken(dir, false)).toBe(token)
    expect(serverToken(dir, true)).not.toBe(token)
  })

  it("makes the token, the headers and the server record private again when they exist", () => {
    const files = ["token", "headers.json", "server.json"].map(name => join(dir, name))
    for (const file of files) {
      writeFileSync(file, "{}\n")
      chmodSync(file, 0o644)
    }
    serverToken(dir, true)
    writeHeadersFile(dir, "token")
    writeServerFile(dir, {name: "agent", port: 5226, expireHours: 23})
    expect(files.map(file => statSync(file).mode & 0o777)).toEqual([0o600, 0o600, 0o600])
  })

  it("records the server", () => {
    expect(readServerFile(dir)).toBeUndefined()
    writeServerFile(dir, {name: "agent", port: 5226, expireHours: 23})
    expect(readServerFile(dir)).toEqual({name: "agent", port: 5226, expireHours: 23})
  })
})

describe("model file", () => {
  const models: ModelEntry[] = [
    {name: "gemma", model: "llama-swap/gemma-4-26b"},
    {name: "coder", model: "llama-swap/qwen3-coder-next"},
  ]
  let file: string

  beforeEach(() => {
    file = join(dir, "..", "model")
  })

  it("is written as one line readable by all, through FILE.part, and replaces a private file and a private FILE.part", () => {
    writeModelFile(file, "llama-swap/gemma-4-26b")
    expect(readFileSync(file, "utf8")).toBe("llama-swap/gemma-4-26b\n")
    expect(statSync(file).mode & 0o777).toBe(0o644)
    chmodSync(file, 0o600)
    writeFileSync(`${file}.part`, "x", {mode: 0o600})
    writeModelFile(file, "llama-swap/qwen3-coder-next")
    expect(readFileSync(file, "utf8")).toBe("llama-swap/qwen3-coder-next\n")
    expect(statSync(file).mode & 0o777).toBe(0o644)
    expect(existsSync(`${file}.part`)).toBe(false)
  })

  it("keeps a listed model at the start and leaves the file unchanged", () => {
    writeFileSync(file, "llama-swap/qwen3-coder-next\nchanged by hand\n")
    const selection = modelAtStart({models, file})
    expect(selection).toEqual({models, file, selected: models[1]})
    expect(selection.selected).toBe(models[1])
    expect(readFileSync(file, "utf8")).toBe("llama-swap/qwen3-coder-next\nchanged by hand\n")
  })

  it("writes the first model at the start when the file is missing or its model is not listed", () => {
    expect(modelAtStart({models, file}).selected).toBe(models[0])
    expect(readFileSync(file, "utf8")).toBe("llama-swap/gemma-4-26b\n")
    for (const content of ["openai/gpt-5\n", "coder\n", " llama-swap/qwen3-coder-next\n", ""]) {
      writeFileSync(file, content)
      expect(modelAtStart({models, file}).selected).toBe(models[0])
      expect(readFileSync(file, "utf8")).toBe("llama-swap/gemma-4-26b\n")
    }
  })

  it("fails at the start when the folder of the file is missing", () => {
    expect(() => modelAtStart({models, file: join(dir, "..", "missing", "model")})).toThrow(/ENOENT/)
  })
})
