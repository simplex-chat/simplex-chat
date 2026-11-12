import {spawnSync} from "child_process"
import {chmodSync, existsSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {abortCounter, acquireLock, createDataFolder, readServerFile, releaseLock, serverToken, writeHeadersFile, writeServerFile} from "../../src/files"
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

  it("writes the headers for the MCP host and for the status hooks, readable only by the user, and rewrites them", () => {
    writeHeadersFile(dir, "first")
    expect(readFileSync(join(dir, "headers.json"), "utf8")).toBe('{"Authorization":"Bearer first"}\n')
    expect(readFileSync(join(dir, "hook-headers"), "utf8")).toBe("Authorization: Bearer first\n")
    expect(statSync(join(dir, "headers.json")).mode & 0o777).toBe(0o600)
    expect(statSync(join(dir, "hook-headers")).mode & 0o777).toBe(0o600)
    writeHeadersFile(dir, "second")
    expect(JSON.parse(readFileSync(join(dir, "headers.json"), "utf8"))).toEqual({Authorization: "Bearer second"})
    expect(readFileSync(join(dir, "hook-headers"), "utf8")).toBe("Authorization: Bearer second\n")
  })

  it("keeps the token until renewed", () => {
    const token = serverToken(dir, false)
    expect(token).toMatch(/^[A-Za-z0-9_-]{43}$/)
    expect(statSync(join(dir, "token")).mode & 0o777).toBe(0o600)
    expect(serverToken(dir, false)).toBe(token)
    expect(serverToken(dir, true)).not.toBe(token)
  })

  it("makes the token, the headers and the server record private again when they exist", () => {
    const files = ["token", "headers.json", "hook-headers", "server.json"].map(name => join(dir, name))
    for (const file of files) {
      writeFileSync(file, "{}\n")
      chmodSync(file, 0o644)
    }
    serverToken(dir, true)
    writeHeadersFile(dir, "token")
    writeServerFile(dir, {name: "agent", port: 5226, expireHours: 23})
    expect(files.map(file => statSync(file).mode & 0o777)).toEqual([0o600, 0o600, 0o600, 0o600])
  })

  it("records the server", () => {
    expect(readServerFile(dir)).toBeUndefined()
    writeServerFile(dir, {name: "agent", port: 5226, expireHours: 23})
    expect(readServerFile(dir)).toEqual({name: "agent", port: 5226, expireHours: 23})
  })
})

describe("abort counter", () => {
  let file: string

  beforeEach(() => {
    file = join(dir, "..", "abort")
  })

  it("writes 0 when the file is missing, then each count, as one line readable by all, through FILE.part, also over a private file and a private FILE.part", () => {
    const count = abortCounter(file)
    expect(readFileSync(file, "utf8")).toBe("0\n")
    expect(statSync(file).mode & 0o777).toBe(0o644)
    expect(existsSync(`${file}.part`)).toBe(false)
    count()
    expect(readFileSync(file, "utf8")).toBe("1\n")
    chmodSync(file, 0o600)
    writeFileSync(`${file}.part`, "x", {mode: 0o600})
    count()
    expect(readFileSync(file, "utf8")).toBe("2\n")
    expect(statSync(file).mode & 0o777).toBe(0o644)
    expect(existsSync(`${file}.part`)).toBe(false)
  })

  it("keeps a count at the start and continues it", () => {
    writeFileSync(file, "7\nchanged by hand\n")
    const count = abortCounter(file)
    expect(readFileSync(file, "utf8")).toBe("7\nchanged by hand\n")
    count()
    expect(readFileSync(file, "utf8")).toBe("8\n")
    writeFileSync(file, `${"9".repeat(15)}\n`)
    abortCounter(file)
    expect(readFileSync(file, "utf8")).toBe(`${"9".repeat(15)}\n`)
  })

  it("writes 0 at the start over a first line that is not a count of at most 15 digits", () => {
    for (const content of ["", "x\n", "-1\n", " 7\n", "7 \n", "1e3\n", `${"9".repeat(16)}\n`]) {
      writeFileSync(file, content)
      abortCounter(file)
      expect(readFileSync(file, "utf8")).toBe("0\n")
    }
  })

  it("fails at the start when the folder of the file is missing", () => {
    expect(() => abortCounter(join(dir, "..", "missing", "abort"))).toThrow(/ENOENT/)
  })
})
