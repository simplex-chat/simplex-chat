import {execFile, spawn} from "child_process"
import {cpSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync} from "fs"
import {tmpdir} from "os"
import {basename, join} from "path"
import {promisify} from "util"
import {LineClient} from "../unit/fixtures"

const run = promisify(execFile)
const packageDir = join(__dirname, "../..")
const {version} = JSON.parse(readFileSync(join(packageDir, "package.json"), "utf8"))
const id = "77777777-7777-4777-8777-777777777777"
const lockfiles = ["bun.lock", "bun.lockb", "npm-shrinkwrap.json", "package-lock.json", "pnpm-lock.yaml", "yarn.lock"]
const packageRoot = /^((?:node_modules\/(?:@[^/]+\/)?[^/]+\/)*node_modules\/(?:@[^/]+\/)?[^/]+)\/package\.json$/
let dir: string
let tarball: string
let packed: string[]
let root: string

beforeAll(async () => {
  dir = mkdtempSync(join(tmpdir(), "simplex-mcp-launch-"))
  const source = join(dir, "source")
  cpSync(packageDir, source, {recursive: true, filter: path => !["node_modules", "dist", "coverage"].includes(basename(path))})
  await run("npm", ["ci", "--no-audit", "--no-fund"], {cwd: source})
  await run("npm", ["pack", "--pack-destination", dir], {cwd: source})
  tarball = join(dir, `simplex-chat-mcp-${version}.tgz`)
  const {stdout} = await run("tar", ["-tzf", tarball], {maxBuffer: 16 * 1024 * 1024})
  packed = stdout.split("\n").filter(path => path !== "").map(path => path.replace(/^package\//, ""))
  root = join(dir, "cache", "simplex-chat", "simplex", version)
  mkdirSync(root, {recursive: true})
  await run("tar", ["-xzf", tarball, "--strip-components=1", "-C", root])
}, 300_000)

afterAll(() => {
  rmSync(dir, {recursive: true, force: true})
})

function command(template: string): string {
  return template.replaceAll("${CLAUDE_PLUGIN_ROOT}", root)
}

test("the packed package has no lockfile, so Claude Code installs nothing, no link, which Claude Code refuses, and bundles exactly the production packages of package-lock.json, without development files", async () => {
  expect(packed.filter(path => lockfiles.includes(basename(path)))).toEqual([])
  const {stdout: listing} = await run("tar", ["-tvzf", tarball], {maxBuffer: 16 * 1024 * 1024})
  expect(listing.split("\n").filter(line => line !== "" && !["-", "d"].includes(line[0]))).toEqual([])
  expect([...new Set(packed.map(path => path.split("/")[0]))].sort()).toEqual([".claude-plugin", ".mcp.json", "LICENSE", "README.md", "dist", "hooks", "node_modules", "package.json", "plugin"])
  const lock = JSON.parse(readFileSync(join(packageDir, "package-lock.json"), "utf8"))
  const production = Object.keys(lock.packages).filter(path => path !== "" && !lock.packages[path].dev)
  expect([...new Set(packed.flatMap(path => packageRoot.exec(path)?.[1] ?? []))].sort()).toEqual(production.sort())
})

test("the unpacked plugin, with nothing installed, runs its hooks and its session server from its bundled modules", async () => {
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
  expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "download", "list", "leave", "reply", "pre_tool_use"])
  server.stdin.end()
  await new Promise(resolve => server.on("close", resolve))
  expect(existsSync(join(dir, "home", "service.lock"))).toBe(false)
}, 300_000)

test("the package installs from its one tarball without the registry and runs as a dependency, as npx runs it", async () => {
  const project = join(dir, "project")
  mkdirSync(project)
  await run("npm", ["install", "--offline", "--ignore-scripts", "--no-audit", "--no-fund", "--no-save", tarball], {cwd: project})
  expect(existsSync(join(project, "node_modules", "@simplex-chat", "mcp", "node_modules", "simplex-chat", "package.json"))).toBe(true)
  const {stdout} = await run("npm", ["exec", "--offline", "--", "simplex-mcp", "--help"], {cwd: project})
  expect(stdout).toContain("simplex-mcp serve NAME [options]")
}, 300_000)
