import {spawnSync} from "child_process"
import {chmodSync, closeSync, existsSync, fstatSync, mkdirSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {LockedError, acquireLock, createDataFolder, dataRoot, hubFolder, logFile, openLog, readSessionFile, releaseLock, serverToken, sessionFolder, sessionNumbers, socketPath, writeSessionFile} from "../../src/files"
import {ExitError} from "../../src/log"

const id = "34B7AEBF-E707-4D6F-ADD0-ECDFB4FFE281"

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
    const locked = new LockedError(`Another daemon uses ${dir} (PID ${runningPid}). If none runs, remove ${join(dir, "lock")}.`, 1)
    expect(() => acquireLock(dir)).toThrow(locked)
    expect(() => acquireLock(dir)).toThrow(LockedError)
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

  it("keeps the token", () => {
    const token = serverToken(dir)
    expect(token).toMatch(/^[A-Za-z0-9_-]{43}$/)
    expect(statSync(join(dir, "token")).mode & 0o777).toBe(0o600)
    expect(serverToken(dir)).toBe(token)
  })

  it("makes the token private again when it exists", () => {
    const file = join(dir, "token")
    writeFileSync(file, "existing\n")
    chmodSync(file, 0o644)
    expect(serverToken(dir)).toBe("existing")
    expect(statSync(file).mode & 0o777).toBe(0o600)
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
  it("round-trip privately and list their numbers", () => {
    const root = mkdtempSync(join(tmpdir(), "simplex-mcp-files-"))
    const folder = sessionFolder(root, id)
    mkdirSync(folder, {recursive: true})
    expect(readSessionFile(folder)).toBeUndefined()
    const file = {number: 3, name: "simplex-chat-6", project: "/p/simplex-chat-6", created: "2026-10-24T10:00:00.000Z"}
    writeSessionFile(folder, file)
    expect(readSessionFile(folder)).toEqual(file)
    expect(statSync(join(folder, "session.json")).mode & 0o777).toBe(0o600)
    mkdirSync(join(root, "_sessions", "00000000-0000-0000-0000-000000000000"))
    expect(sessionNumbers(root)).toEqual([3])
    expect(sessionNumbers(join(root, "missing"))).toEqual([])
    rmSync(root, {recursive: true, force: true})
  })
})

describe("socket path", () => {
  it("is in the root, in tmpdir when too long, and a named pipe on Windows", () => {
    expect(socketPath("/home/u/.simplex-mcp", "linux")).toBe("/home/u/.simplex-mcp/daemon.sock")
    const long = `/home/${"u".repeat(120)}/.simplex-mcp`
    expect(socketPath(long, "darwin")).toMatch(new RegExp(`^${tmpdir().replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}/simplex-mcp-[0-9a-f]{16}\\.sock$`))
    expect(socketPath("C:\\Users\\u\\.simplex-mcp", "win32")).toMatch(/^\\\\\.\\pipe\\simplex-mcp-[0-9a-f]{16}$/)
    expect(socketPath("/a", "win32")).not.toBe(socketPath("/b", "win32"))
  })
})

describe("token creation", () => {
  it("is exclusive", () => {
    const root = mkdtempSync(join(tmpdir(), "simplex-mcp-token-"))
    const token = serverToken(root)
    expect(serverToken(root)).toBe(token)
    expect(readFileSync(join(root, "token"), "utf8")).toBe(`${token}\n`)
    rmSync(root, {recursive: true, force: true})
  })
})

describe("daemon log", () => {
  it("is opened for append and truncated when over 10 MB", () => {
    const root = mkdtempSync(join(tmpdir(), "simplex-mcp-log-"))
    writeFileSync(logFile(root), "x".repeat(10 * 1_048_576 + 1))
    const fd = openLog(root)
    expect(fstatSync(fd).size).toBe(0)
    closeSync(fd)
    writeFileSync(logFile(root), "kept\n")
    closeSync(openLog(root))
    expect(readFileSync(logFile(root), "utf8")).toBe("kept\n")
    rmSync(root, {recursive: true, force: true})
  })

  it("is created private", () => {
    const root = mkdtempSync(join(tmpdir(), "simplex-mcp-log-"))
    closeSync(openLog(root))
    expect(statSync(logFile(root)).mode & 0o777).toBe(0o600)
    rmSync(root, {recursive: true, force: true})
  })
})
