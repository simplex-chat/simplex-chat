import * as childProcess from "child_process"
import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {createDataFolder, serverToken, writeServerFile} from "../../src/files"
import {ExitError} from "../../src/log"
import {RunResult, cmdArgument, projectRoot, register, registryEntry} from "../../src/register"

let dir: string

beforeEach(() => {
  dir = join(mkdtempSync(join(tmpdir(), "simplex-mcp-register-")), "agent")
  createDataFolder(dir)
})

afterEach(() => {
  jest.restoreAllMocks()
  rmSync(join(dir, ".."), {recursive: true, force: true})
})

const ok: RunResult = {missing: false, status: 0, stderr: ""}

function recorder(results: RunResult[]): {calls: string[][]; run: (command: string, args: string[]) => RunResult} {
  const calls: string[][] = []
  return {calls, run: (command, args) => (calls.push([command, ...args]), results[calls.length - 1] ?? ok)}
}

describe("register", () => {
  const platform = Object.getOwnPropertyDescriptor(process, "platform")!
  const onPlatform = (value: NodeJS.Platform): void => {
    Object.defineProperty(process, "platform", {...platform, value})
  }

  afterEach(() => Object.defineProperty(process, "platform", platform))

  it("builds the entry with the timeout from server.json and a helper that prints the headers file", () => {
    onPlatform("linux")
    expect(registryEntry(5226, "/home/alice/.simplex-mcp/agent", 23)).toEqual({
      type: "http",
      url: "http://127.0.0.1:5226/mcp",
      headersHelper: "cat /home/alice/.simplex-mcp/agent/headers.json",
      timeout: 169_800_000,
    })
    expect(registryEntry(5227, "/tmp/my agent's", 1)).toMatchObject({headersHelper: "cat '/tmp/my agent'\\''s/headers.json'", timeout: 25 * 3_600_000 + 600_000})
    onPlatform("win32")
    const data = "C:\\Users\\Ana Lu\\agent"
    expect(registryEntry(5226, data, 23).headersHelper).toBe(`type "${join(data, "headers.json")}"`)
  })

  it("removes the entry before adding it, with no token in the command", () => {
    writeServerFile(dir, {name: "agent", port: 5227, expireHours: 2})
    const token = serverToken(dir, false)
    const {calls, run} = recorder([{missing: false, status: 1, stderr: "not found"}, ok])
    expect(register({name: "agent", data: dir, scope: "user"}, run)).toBe("Added to Claude Code: scope user, server simplex-agent.")
    const entry = JSON.stringify({type: "http", url: "http://127.0.0.1:5227/mcp", headersHelper: `cat ${join(dir, "headers.json")}`, timeout: 26 * 3_600_000 + 600_000})
    expect(calls).toEqual([
      ["claude", "mcp", "remove", "--scope", "user", "simplex-agent"],
      ["claude", "mcp", "add-json", "--scope", "user", "simplex-agent", entry],
    ])
    expect(calls.flat().join(" ")).not.toContain(token)
  })

  it("reports an agent that was never started", () => {
    const {run} = recorder([])
    expect(() => register({name: "agent", data: dir, scope: "user"}, run)).toThrow(
      new ExitError('Agent "agent" was never started. Start it with: npx -y @simplex-chat/mcp serve agent', 1),
    )
  })

  it("prints the entry when claude is missing", () => {
    writeServerFile(dir, {name: "agent", port: 5226, expireHours: 23})
    const entry = JSON.stringify(registryEntry(5226, dir, 23))
    const missing = {missing: true, status: null, stderr: ""}
    expect(() => register({name: "agent", data: dir, scope: "user"}, recorder([missing]).run)).toThrow(
      new ExitError(`Add this entry under "mcpServers" in ~/.claude.json: {"simplex-agent": ${entry}}`, 1),
    )
    expect(() => register({name: "agent", data: dir, scope: "local"}, recorder([missing]).run, () => "/work/project")).toThrow(
      new ExitError(`Add this entry under "projects" > "/work/project" > "mcpServers" in ~/.claude.json: {"simplex-agent": ${entry}}`, 1),
    )
  })

  it("reports a failed add", () => {
    writeServerFile(dir, {name: "agent", port: 5226, expireHours: 23})
    expect(() => register({name: "agent", data: dir, scope: "user"}, recorder([ok, {missing: false, status: 1, stderr: "bad scope\n"}]).run)).toThrow(
      new ExitError("claude mcp add-json failed: bad scope", 1),
    )
  })

  describe("when claude can't be spawned", () => {
    const spawnClaude = (shellStatus: number): jest.SpyInstance =>
      jest.spyOn(childProcess, "spawnSync").mockImplementation(((_command: string, args: unknown) =>
        Array.isArray(args) ? {error: Object.assign(new Error("spawnSync claude ENOENT"), {code: "ENOENT"}), status: null, stderr: ""} : {status: shellStatus, stderr: ""}) as never)
    const manualEntry = (): ExitError => new ExitError(`Add this entry under "mcpServers" in ~/.claude.json: {"simplex-agent": ${JSON.stringify(registryEntry(5226, dir, 23))}}`, 1)

    beforeEach(() => writeServerFile(dir, {name: "agent", port: 5226, expireHours: 23}))

    it("runs it through cmd.exe on Windows, with the arguments quoted for cmd.exe", () => {
      onPlatform("win32")
      const spawn = spawnClaude(0)
      expect(register({name: "agent", data: dir, scope: "user"})).toBe("Added to Claude Code: scope user, server simplex-agent.")
      const words = (args: string[]): string => ["claude", ...args.map(cmdArgument)].join(" ")
      expect(spawn.mock.calls).toEqual([
        ["claude", ["mcp", "remove", "--scope", "user", "simplex-agent"], {encoding: "utf8"}],
        [words(["mcp", "remove", "--scope", "user", "simplex-agent"]), {encoding: "utf8", shell: true}],
        ["claude", ["mcp", "add-json", "--scope", "user", "simplex-agent", JSON.stringify(registryEntry(5226, dir, 23))], {encoding: "utf8"}],
        [words(["mcp", "add-json", "--scope", "user", "simplex-agent", JSON.stringify(registryEntry(5226, dir, 23))]), {encoding: "utf8", shell: true}],
      ])
    })

    it("prints the entry on Windows when cmd.exe does not find it either", () => {
      onPlatform("win32")
      const spawn = spawnClaude(9009)
      expect(() => register({name: "agent", data: dir, scope: "user"})).toThrow(manualEntry())
      expect(spawn).toHaveBeenCalledTimes(2)
    })

    it("prints the entry on other platforms without a shell", () => {
      onPlatform("linux")
      const spawn = spawnClaude(0)
      expect(() => register({name: "agent", data: dir, scope: "user"})).toThrow(manualEntry())
      expect(spawn).toHaveBeenCalledTimes(1)
    })
  })

  it("quotes an argument for cmd.exe and for the npm command shim it runs", () => {
    expect(cmdArgument("mcp")).toBe('^^^"mcp^^^"')
    expect(cmdArgument('{"a":"b c"}')).toBe('^^^"{\\^^^"a\\^^^":\\^^^"b^^^ c\\^^^"}^^^"')
    expect(cmdArgument('a\\"b')).toBe('^^^"a\\\\\\^^^"b^^^"')
    expect(cmdArgument("C:\\dir\\")).toBe('^^^"C:\\dir\\\\^^^"')
    expect(cmdArgument("100% & more")).toBe('^^^"100^^^%^^^ ^^^&^^^ more^^^"')
  })
})

describe("project root", () => {
  function folder(...parts: string[]): string {
    const path = join(dir, "..", ...parts)
    mkdirSync(path, {recursive: true})
    return path
  }

  it("keys a repository folder, a worktree, a submodule and a plain folder as Claude Code does", () => {
    const repo = folder("repo")
    const worktreeGit = folder("repo", ".git", "worktrees", "feature")
    writeFileSync(join(worktreeGit, "commondir"), "../..\n")
    writeFileSync(join(folder("feature"), ".git"), `gitdir: ${worktreeGit}\n`)
    folder("repo", ".git", "modules", "sub")
    writeFileSync(join(folder("repo", "sub"), ".git"), "gitdir: ../.git/modules/sub\n")
    expect(projectRoot(folder("repo", "src", "lib"))).toBe(repo)
    expect(projectRoot(folder("feature", "src"))).toBe(repo)
    expect(projectRoot(folder("repo", "sub", "src"))).toBe(join(repo, "sub"))
    expect(projectRoot(folder("plain", "a"))).toBe(folder("plain", "a"))
  })
})
