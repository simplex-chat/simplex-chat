import {spawnSync} from "child_process"
import {chmodSync, existsSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {SecretRecord, Secrets, acquireLock, createDataFolder, readServerFile, releaseLock, serverToken, writeHeadersFile, writeServerFile} from "../../src/files"
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

describe("secrets", () => {
  const alice: SecretRecord = {setBy: "Alice", setById: 3, setAt: "2026-11-12T10:00:00.000Z"}
  const bob: SecretRecord = {setBy: "Bob", setById: 7, setAt: "2026-11-12T11:30:00.000Z"}
  let folder: string
  let secrets: Secrets

  beforeEach(() => {
    folder = join(dir, "..", "secrets")
    secrets = new Secrets(folder, join(dir, "secrets.json"))
  })

  it("creates a private folder and keeps the mode of an existing one", () => {
    expect(statSync(folder).mode & 0o777).toBe(0o700)
    chmodSync(folder, 0o750)
    new Secrets(folder, join(dir, "secrets.json"))
    expect(statSync(folder).mode & 0o777).toBe(0o750)
  })

  it("writes each value to a private file named by its secret, and replaces it", () => {
    secrets.set("API_TOKEN", "first value", alice)
    expect(readFileSync(join(folder, "API_TOKEN"), "utf8")).toBe("first value")
    expect(statSync(join(folder, "API_TOKEN")).mode & 0o777).toBe(0o600)
    expect(existsSync(join(folder, "API_TOKEN.part"))).toBe(false)
    secrets.set("API_TOKEN", "second value", bob)
    expect(readFileSync(join(folder, "API_TOKEN"), "utf8")).toBe("second value")
  })

  it("records the setter and the time in a private index in the data folder", () => {
    expect(secrets.records()).toEqual({})
    secrets.set("API_TOKEN", "first value", alice)
    secrets.set("GIT_TOKEN", "second value", bob)
    expect(statSync(join(dir, "secrets.json")).mode & 0o777).toBe(0o600)
    expect(JSON.parse(readFileSync(join(dir, "secrets.json"), "utf8"))).toEqual({API_TOKEN: alice, GIT_TOKEN: bob})
    expect(secrets.records()).toEqual({API_TOKEN: alice, GIT_TOKEN: bob})
  })

  it("lists the names of the secret files only, sorted", () => {
    for (const name of ["GIT_TOKEN", "API_TOKEN", "API_TOKEN.part", "api_token", ".API_TOKEN", "1_TOKEN", `A${"B".repeat(64)}`]) writeFileSync(join(folder, name), "value")
    expect(secrets.names()).toEqual(["API_TOKEN", "GIT_TOKEN"])
  })

  it("removes a secret with its record, and nothing for another name", () => {
    secrets.set("API_TOKEN", "first value", alice)
    secrets.set("GIT_TOKEN", "second value", bob)
    writeFileSync(join(folder, "..", "x"), "outside")
    for (const name of ["OTHER", "../x", "api_token"]) expect(secrets.remove(name)).toBe(false)
    expect(readFileSync(join(folder, "..", "x"), "utf8")).toBe("outside")
    expect(secrets.records()).toEqual({API_TOKEN: alice, GIT_TOKEN: bob})
    expect(secrets.remove("API_TOKEN")).toBe(true)
    expect(existsSync(join(folder, "API_TOKEN"))).toBe(false)
    expect(secrets.names()).toEqual(["GIT_TOKEN"])
    expect(secrets.records()).toEqual({GIT_TOKEN: bob})
  })
})
