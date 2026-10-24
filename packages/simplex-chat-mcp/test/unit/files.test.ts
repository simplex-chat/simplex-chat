import {spawnSync} from "child_process"
import {chmodSync, existsSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {acquireLock, createDataFolder, releaseLock, serverToken} from "../../src/files"
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

  it("keeps the token until renewed", () => {
    const token = serverToken(dir, false)
    expect(token).toMatch(/^[A-Za-z0-9_-]{43}$/)
    expect(statSync(join(dir, "token")).mode & 0o777).toBe(0o600)
    expect(serverToken(dir, false)).toBe(token)
    expect(serverToken(dir, true)).not.toBe(token)
  })

  it("makes the token private again when it exists", () => {
    const file = join(dir, "token")
    writeFileSync(file, "{}\n")
    chmodSync(file, 0o644)
    serverToken(dir, true)
    expect(statSync(file).mode & 0o777).toBe(0o600)
  })
})
