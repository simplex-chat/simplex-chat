import {spawnSync} from "child_process"
import * as fs from "fs"
import {chmodSync, closeSync, existsSync, fstatSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {LockedError, ModelEntry, acquireLock, createDataFolder, dataRoot, hubFolder, logFile, modelAtStart, openLog, readServerFile, readSessionFile, releaseLock, serverToken, sessionFolder, socketPath, storedSessions, writeHeadersFile, writeModelFile, writeServerFile, writeSessionFile} from "../../src/files"
import {ExitError} from "../../src/log"

const id = "34B7AEBF-E707-4D6F-ADD0-ECDFB4FFE281"

let dir: string
let roots: string[]

function tempRoot(prefix: string): string {
  const root = mkdtempSync(join(tmpdir(), prefix))
  roots.push(root)
  return root
}

beforeEach(() => {
  roots = []
  dir = join(tempRoot("simplex-mcp-files-"), "agent")
  createDataFolder(dir)
})

afterEach(() => {
  for (const root of roots) rmSync(root, {recursive: true, force: true})
})

describe("lock", () => {
  it("is created exclusively with the PID and removed", () => {
    acquireLock(dir)
    expect(readFileSync(join(dir, "lock"), "utf8")).toBe(`${process.pid}\n`)
    expect(statSync(join(dir, "lock")).mode & 0o777).toBe(0o600)
    releaseLock(dir)
    expect(existsSync(join(dir, "lock"))).toBe(false)
  })

  it("tolerates a lock that is already removed, and reports other release errors", () => {
    releaseLock(dir)
    mkdirSync(join(dir, "lock"))
    expect(() => releaseLock(dir)).toThrow(expect.objectContaining({code: expect.stringMatching(/^(EISDIR|EPERM)$/)}))
  })

  it("refuses a lock of a running process", () => {
    const runningPid = process.ppid
    writeFileSync(join(dir, "lock"), `${runningPid}\n`)
    expect(() => acquireLock(dir)).toThrow(new ExitError(`Another server uses ${dir} (PID ${runningPid}). If no server runs, remove ${join(dir, "lock")}.`, 1))
    const locked = new LockedError(`Another service uses ${dir} (PID ${runningPid}). If no service runs, remove ${join(dir, "lock")}.`, 1)
    expect(() => acquireLock(dir, "service")).toThrow(locked)
    expect(() => acquireLock(dir, "service")).toThrow(LockedError)
    expect(locked).toBeInstanceOf(ExitError)
  })

  it("replaces a lock of a stopped process or of this process", () => {
    const exited = spawnSync(process.execPath, ["-e", "process.stdout.write(String(process.pid))"], {encoding: "utf8"})
    writeFileSync(join(dir, "lock"), `${exited.stdout}\n`)
    acquireLock(dir)
    expect(readFileSync(join(dir, "lock"), "utf8")).toBe(`${process.pid}\n`)
    acquireLock(dir)
    expect(readFileSync(join(dir, "lock"), "utf8")).toBe(`${process.pid}\n`)
  })

  it("is not taken by a second starter that reads it before the first has written its PID", () => {
    const file = join(dir, "lock")
    const realWrite = fs.writeFileSync
    const realLink = fs.linkSync
    let racing = false
    let second: unknown
    const secondStart = (): void => {
      racing = true
      try {
        acquireLock(dir, "service")
        second = "acquired"
      } catch (error) {
        second = error
      }
    }
    const write = jest.spyOn(fs, "writeFileSync").mockImplementation(((path: fs.PathOrFileDescriptor, text: string, options?: fs.WriteFileOptions) => {
      if (path !== file || racing || (options as {flag?: string} | undefined)?.flag !== "wx") return realWrite(path, text, options)
      const fd = fs.openSync(file, "wx", 0o600)
      secondStart()
      fs.writeSync(fd, `${process.ppid}\n`)
      fs.closeSync(fd)
    }) as typeof fs.writeFileSync)
    const link = jest.spyOn(fs, "linkSync").mockImplementation((existing: fs.PathLike, path: fs.PathLike) => {
      if (path !== file || racing) return realLink(existing, path)
      realWrite(existing, `${process.ppid}\n`)
      realLink(existing, path)
      secondStart()
    })
    try {
      acquireLock(dir, "service")
    } finally {
      write.mockRestore()
      link.mockRestore()
    }
    expect(second).toBeInstanceOf(LockedError)
  })

  function expectOneOfTwoTakeOvers(): void {
    const exited = spawnSync(process.execPath, ["-e", "process.stdout.write(String(process.pid))"], {encoding: "utf8"})
    const file = join(dir, "lock")
    writeFileSync(file, `${exited.stdout}\n`)
    const realRead = fs.readFileSync
    let second: unknown
    const read = jest.spyOn(fs, "readFileSync").mockImplementation(((path: fs.PathOrFileDescriptor, options?: unknown) => {
      const text = realRead(path, options as {encoding: BufferEncoding})
      if (path === file && second === undefined) {
        second = "racing"
        try {
          acquireLock(dir)
          second = "acquired"
        } catch (error) {
          second = error
        }
      }
      return text
    }) as typeof fs.readFileSync)
    let first: unknown = "acquired"
    try {
      acquireLock(dir)
    } catch (error) {
      first = error
    } finally {
      read.mockRestore()
    }
    expect([first, second].filter(result => result === "acquired")).toHaveLength(1)
    expect([first, second].find(result => result !== "acquired")).toBeInstanceOf(LockedError)
    expect(readdirSync(dir).filter(name => name.startsWith("lock"))).toEqual(["lock"])
  }

  it("lets only one of two starters take over the lock of a stopped process", () => {
    expectOneOfTwoTakeOvers()
  })

  it("lets only one of two starters take over the lock of a stopped process where the file system has no hard links", () => {
    const link = jest.spyOn(fs, "linkSync").mockImplementation(() => {
      throw Object.assign(new Error("operation not permitted"), {code: "EPERM"})
    })
    try {
      expectOneOfTwoTakeOvers()
    } finally {
      link.mockRestore()
    }
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

  it("makes the token private again when it exists", () => {
    const file = join(dir, "token")
    writeFileSync(file, "existing\n")
    chmodSync(file, 0o644)
    expect(serverToken(dir)).toBe("existing")
    expect(statSync(file).mode & 0o777).toBe(0o600)
  })

  it("reads an existing token of another user without changing its mode, as 0.2 did", () => {
    const file = join(dir, "token")
    writeFileSync(file, "existing\n")
    chmodSync(file, 0o644)
    const uid = jest.spyOn(process, "getuid").mockReturnValue(statSync(file).uid + 1)
    try {
      expect(serverToken(dir)).toBe("existing")
    } finally {
      uid.mockRestore()
    }
    expect(statSync(file).mode & 0o777).toBe(0o644)
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

describe("data root", () => {
  it("is ~/.simplex-mcp unless SIMPLEX_MCP_HOME is set", () => {
    expect(dataRoot({}, "/home/u")).toBe("/home/u/.simplex-mcp")
    expect(dataRoot({SIMPLEX_MCP_HOME: "/data/mcp"}, "/home/u")).toBe("/data/mcp")
    expect(dataRoot({SIMPLEX_MCP_HOME: ""}, "/home/u")).toBe("/home/u/.simplex-mcp")
  })

  it("keeps the hub and the sessions in folders 0.1 agent names can't take", () => {
    expect(hubFolder("/r")).toBe("/r/_hub")
    expect(sessionFolder("/r", id)).toBe("/r/_sessions/34b7aebf-e707-4d6f-add0-ecdfb4ffe281")
  })

  it("refuses session IDs that are not UUIDs", () => {
    for (const bad of ["", "../x", "34b7aebf", `${id}/..`]) {
      expect(() => sessionFolder("/r", bad)).toThrow(new ExitError(`Not a Claude Code session ID: ${bad}`, 2))
    }
  })
})

describe("session files", () => {
  it("round-trip privately and list the stored sessions", () => {
    const root = tempRoot("simplex-mcp-files-")
    const folder = sessionFolder(root, id)
    mkdirSync(folder, {recursive: true})
    expect(readSessionFile(folder)).toBeUndefined()
    const file = {number: 3, name: "simplex-chat-6", project: "/p/simplex-chat-6", created: "2026-10-24T10:00:00.000Z", group: 12, mode: "host" as const}
    writeSessionFile(folder, file)
    expect(readSessionFile(folder)).toEqual(file)
    expect(statSync(join(folder, "session.json")).mode & 0o777).toBe(0o600)
    mkdirSync(join(root, "_sessions", "00000000-0000-0000-0000-000000000000"))
    expect(storedSessions(root)).toEqual([{id: id.toLowerCase(), session: file}])
    expect(storedSessions(join(root, "missing"))).toEqual([])
  })

  it("skip an unparsable file with a log line, and are replaced whole", () => {
    const errors: string[] = []
    const stderr = jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text)), true))
    const rename = jest.spyOn(fs, "renameSync")
    try {
      const root = tempRoot("simplex-mcp-files-")
      const folder = sessionFolder(root, id)
      const file = join(folder, "session.json")
      mkdirSync(folder, {recursive: true})
      writeFileSync(file, "")
      expect(storedSessions(root)).toEqual([])
      expect(errors).toEqual([expect.stringMatching(new RegExp(`^${file.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}: `))])
      writeSessionFile(folder, {number: 3, name: "app", project: "/p/app", created: "2026-10-24T10:00:00.000Z"})
      expect(rename).toHaveBeenCalledWith(`${file}.tmp`, file)
      expect(readdirSync(folder)).toEqual(["session.json"])
      expect(readSessionFile(folder)).toMatchObject({number: 3})
    } finally {
      stderr.mockRestore()
      rename.mockRestore()
    }
  })
})

describe("socket path", () => {
  it("is in the root, in tmpdir when too long, and a named pipe on Windows", () => {
    expect(socketPath("/home/u/.simplex-mcp", "linux")).toBe("/home/u/.simplex-mcp/service.sock")
    const long = `/home/${"u".repeat(120)}/.simplex-mcp`
    expect(socketPath(long, "darwin")).toMatch(new RegExp(`^${tmpdir().replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}/simplex-mcp-[0-9a-f]{16}\\.sock$`))
    expect(socketPath("C:\\Users\\u\\.simplex-mcp", "win32")).toMatch(/^\\\\\.\\pipe\\simplex-mcp-[0-9a-f]{16}$/)
    expect(socketPath("/a", "win32")).not.toBe(socketPath("/b", "win32"))
  })
})

describe("token creation", () => {
  it("is exclusive", () => {
    const root = tempRoot("simplex-mcp-token-")
    const token = serverToken(root)
    expect(serverToken(root)).toBe(token)
    expect(readFileSync(join(root, "token"), "utf8")).toBe(`${token}\n`)
  })

  it("is complete when it appears and loses no race", () => {
    const root = tempRoot("simplex-mcp-token-")
    const linkSync = fs.linkSync
    const seen: string[] = []
    const spy = jest.spyOn(fs, "linkSync").mockImplementation((existing, created) => {
      seen.push(readFileSync(existing, "utf8"))
      if (seen.length === 1) {
        writeFileSync(join(root, "winner"), "winner\n")
        linkSync(join(root, "winner"), created)
      }
      linkSync(existing, created)
    })
    try {
      expect(serverToken(root)).toBe("winner")
      expect(seen).toHaveLength(1)
      expect(seen[0]).toMatch(/^[A-Za-z0-9_-]{43}\n$/)
      fs.rmSync(join(root, "winner"))
      expect(readdirSync(root)).toEqual(["token"])
      expect(readFileSync(join(root, "token"), "utf8")).toBe("winner\n")
    } finally {
      spy.mockRestore()
    }
  })

  it("is written exclusively where the file system has no hard links", () => {
    const root = tempRoot("simplex-mcp-token-")
    const spy = jest.spyOn(fs, "linkSync").mockImplementation(() => {
      throw Object.assign(new Error("operation not permitted"), {code: "EPERM"})
    })
    try {
      const token = serverToken(root)
      expect(readFileSync(join(root, "token"), "utf8")).toBe(`${token}\n`)
      expect(serverToken(root)).toBe(token)
      expect(readdirSync(root)).toEqual(["token"])
    } finally {
      spy.mockRestore()
    }
  })

  it("leaves only the token after creating it", () => {
    const root = tempRoot("simplex-mcp-token-")
    const token = serverToken(root)
    expect(readdirSync(root)).toEqual(["token"])
    expect(readFileSync(join(root, "token"), "utf8")).toBe(`${token}\n`)
  })
})

describe("service log", () => {
  it("is opened for append and truncated when over 10 MB", () => {
    const root = tempRoot("simplex-mcp-log-")
    writeFileSync(logFile(root), "x".repeat(10 * 1_048_576 + 1))
    const fd = openLog(root)
    expect(fstatSync(fd).size).toBe(0)
    closeSync(fd)
    writeFileSync(logFile(root), "kept\n")
    closeSync(openLog(root))
    expect(readFileSync(logFile(root), "utf8")).toBe("kept\n")
  })

  it("is created private and made private again when it exists", () => {
    const root = tempRoot("simplex-mcp-log-")
    closeSync(openLog(root))
    expect(statSync(logFile(root)).mode & 0o777).toBe(0o600)
    chmodSync(logFile(root), 0o644)
    closeSync(openLog(root))
    expect(statSync(logFile(root)).mode & 0o777).toBe(0o600)
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
