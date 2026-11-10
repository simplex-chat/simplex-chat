import {execFile, spawn} from "child_process"
import {existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {promisify} from "util"
import {LineClient} from "../unit/fixtures"

const run = promisify(execFile)
const packageDir = join(__dirname, "../..")
const {version} = JSON.parse(readFileSync(join(packageDir, "package.json"), "utf8"))
const id = "77777777-7777-4777-8777-777777777777"
const installLimit = 60_000
let dir: string
let root: string

beforeAll(async () => {
  dir = mkdtempSync(join(tmpdir(), "simplex-mcp-launch-"))
  await run("npm", ["pack", "--pack-destination", dir], {cwd: packageDir})
  root = join(dir, "cache", "simplex-chat", "simplex", version)
  mkdirSync(root, {recursive: true})
  await run("tar", ["-xzf", join(dir, `simplex-chat-mcp-${version}.tgz`), "--strip-components=1", "-C", root])
  await run("npm", ["ci", "--ignore-scripts", "--workspaces=false", "--no-audit", "--no-fund"], {cwd: root, timeout: installLimit})
}, 300_000)

afterAll(() => {
  rmSync(dir, {recursive: true, force: true})
})

function command(template: string): string {
  return template.replaceAll("${CLAUDE_PLUGIN_ROOT}", root)
}

test("the packed package declares production dependencies only, its shrinkwrap lists only them and agrees with package.json, and the checkout is restored", () => {
  const manifest = JSON.parse(readFileSync(join(root, "package.json"), "utf8"))
  const lock = JSON.parse(readFileSync(join(root, "npm-shrinkwrap.json"), "utf8"))
  expect(manifest.devDependencies).toBeUndefined()
  expect(Object.keys(lock.packages).filter(path => lock.packages[path].dev)).toEqual([])
  expect(lock.packages[""]).toMatchObject({name: manifest.name, version: manifest.version})
  expect(lock.packages[""].dependencies).toEqual(manifest.dependencies)
  expect(lock.packages[""].devDependencies).toBeUndefined()
  expect(JSON.parse(readFileSync(join(packageDir, "package.json"), "utf8")).devDependencies).toBeDefined()
  expect(readdirSync(packageDir).filter(name => name.endsWith(".orig"))).toEqual([])
})

test("the plugin, installed with the command of Claude Code within its 60 s limit, runs its hooks and its session server from its own folder, with dependencies from its shrinkwrap", async () => {
  expect(readdirSync(root)).toEqual(expect.arrayContaining(["npm-shrinkwrap.json", "dist", "node_modules", ".mcp.json", "hooks"]))
  const env = {...process.env, SIMPLEX_MCP_HOME: join(dir, "home"), CLAUDE_PLUGIN_ROOT: root}
  const {hooks} = JSON.parse(readFileSync(join(root, "hooks", "hooks.json"), "utf8"))
  const turn = spawn("sh", ["-c", command(hooks.Stop[0].hooks[0].command)], {cwd: dir, env, stdio: ["pipe", "pipe", "ignore"]})
  turn.stdin.end("not json")
  let output = ""
  turn.stdout.on("data", chunk => (output += chunk))
  expect(await new Promise(resolve => turn.on("close", resolve))).toBe(0)
  expect(output.trim()).toBe("{}")
  const {simplex} = JSON.parse(readFileSync(join(root, ".mcp.json"), "utf8")).mcpServers
  const server = spawn(simplex.command, simplex.args.map(command), {cwd: dir, env: {...env, CLAUDE_CODE_SESSION_ID: id, CLAUDE_PLUGIN_OPTION_APPROVE: "x"}, stdio: ["pipe", "pipe", "inherit"]})
  server.stdout.setEncoding("utf8")
  const client = new LineClient(server.stdin, server.stdout)
  expect(await client.initialize()).toMatchObject({serverInfo: {name: "simplex-mcp", version}})
  const {result} = await client.request("tools/list")
  expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "download", "list", "leave", "reply"])
  server.stdin.end()
  await new Promise(resolve => server.on("close", resolve))
  expect(existsSync(join(dir, "home", "daemon.lock"))).toBe(false)
}, 300_000)

test("the package with its shrinkwrap installs and runs as a dependency, as npx runs it", async () => {
  const project = join(dir, "project")
  mkdirSync(project)
  await run("npm", ["install", "--ignore-scripts", "--no-audit", "--no-fund", "--no-save", join(dir, `simplex-chat-mcp-${version}.tgz`)], {cwd: project})
  const {stdout} = await run("npm", ["exec", "--", "simplex-mcp", "--help"], {cwd: project})
  expect(stdout).toContain("simplex-mcp serve NAME [options]")
}, 300_000)
