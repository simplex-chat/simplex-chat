import {execFileSync, spawn} from "child_process"
import {mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {configVariables, defaultConfig} from "../../src/config"

const read = (file: string): any => JSON.parse(readFileSync(join(__dirname, "../..", file), "utf8"))
const packageJson = read("package.json")
const cli = (args: string): string => `node "\${CLAUDE_PLUGIN_ROOT}/dist/cli.js" ${args}`

describe("plugin", () => {
  it("has the package version and the config defaults", () => {
    const plugin = read(".claude-plugin/plugin.json")
    expect(plugin.name).toBe("simplex")
    expect(plugin.version).toBe(packageJson.version)
    expect(plugin.userConfig.main_group.default).toBe(defaultConfig.mainGroup)
    expect(plugin.userConfig.approve.default).toBe(defaultConfig.approve)
    expect(plugin.userConfig.expire_hours.default).toBe(defaultConfig.expireHours)
    expect(plugin.userConfig.approve_files).toMatchObject({type: "boolean", default: defaultConfig.approveFiles})
  })

  it("starts the session server of the plugin's own build with the options as variables", () => {
    const {simplex} = read(".mcp.json").mcpServers
    expect(simplex.command).toBe("node")
    expect(simplex.args).toEqual(["${CLAUDE_PLUGIN_ROOT}/dist/cli.js", "session"])
    expect(simplex.env).toEqual({
      [configVariables.mainGroup]: "${user_config.main_group}",
      [configVariables.approve]: "${user_config.approve}",
      [configVariables.expireHours]: "${user_config.expire_hours}",
      [configVariables.approveFiles]: "${user_config.approve_files}",
    })
  })

  it("lets a tool call wait for the longest expiry, the responses and a margin", () => {
    const responseWaitHours = 24
    const marginMs = 10 * 60_000
    const maxExpireHours = read(".claude-plugin/plugin.json").userConfig.expire_hours.max
    expect(maxExpireHours).toBe(23)
    expect(read(".mcp.json").mcpServers.simplex.timeout).toBe((maxExpireHours + responseWaitHours) * 3_600_000 + marginMs)
  })

  it("runs the hooks from the plugin's own build and allows the plugin's tools", () => {
    const {hooks} = read("hooks/hooks.json")
    expect(hooks.SessionStart[0].hooks[0]).toEqual({type: "command", command: cli("hook session-start"), timeout: 600})
    expect(hooks.PreToolUse[0].matcher).toBe("mcp__plugin_simplex_simplex__.*")
    expect(hooks.PreToolUse[0].hooks[0]).toEqual({type: "command", command: 'node "${CLAUDE_PLUGIN_ROOT}/plugin/allow.js"'})
    for (const event of ["UserPromptSubmit", "Stop", "PostModelSwitch"]) expect(hooks[event]).toEqual([{hooks: [{type: "command", command: cli("hook turn"), async: true}]}])
  })

  it("ships the build, the dependency lock and the plugin files in the package", () => {
    expect(packageJson.files).toEqual(expect.arrayContaining(["dist", "npm-shrinkwrap.json", ".claude-plugin", ".mcp.json", "hooks", "plugin", "skills"]))
  })

  it.each(["link", "reset-owners"])("offers the %s skill to the user only, and pre-approves its command", command => {
    const skill = readFileSync(join(__dirname, "../../skills", command, "SKILL.md"), "utf8")
    const [, frontmatter] = /^---\n([\s\S]*?)\n---\n/.exec(skill)!
    expect(frontmatter.split("\n")).toEqual([
      `name: ${command}`,
      expect.stringMatching(/^description: \S/),
      "disable-model-invocation: true",
      `allowed-tools: Bash(${cli(command)})`,
    ])
    expect(skill).toContain(`!\`${cli(command)}\``)
  })

  it("packs production dependencies only, restores the files after the pack, and heals a pack that failed after the strip", () => {
    expect(packageJson.scripts.prepack).toMatch(/ && node scripts\/strip-dev\.js$/)
    expect(packageJson.scripts.postpack).toBe("node scripts/strip-dev.js --restore")
    const dir = mkdtempSync(join(tmpdir(), "simplex-mcp-strip-"))
    try {
      const manifest = {name: "p", version: "1.0.0", dependencies: {a: "^1"}, devDependencies: {b: "^1"}}
      const lock = {name: "p", lockfileVersion: 3, packages: {"": {name: "p", dependencies: {a: "^1"}, devDependencies: {b: "^1"}}, "node_modules/a": {version: "1.0.0"}, "node_modules/b": {version: "1.0.0", dev: true}}}
      writeFileSync(join(dir, "package.json"), JSON.stringify(manifest))
      writeFileSync(join(dir, "npm-shrinkwrap.json"), JSON.stringify(lock))
      const strip = (...args: string[]) => execFileSync(process.execPath, [join(__dirname, "../../scripts/strip-dev.js"), ...args], {cwd: dir})
      const files = () => ({manifest: JSON.parse(readFileSync(join(dir, "package.json"), "utf8")), lock: JSON.parse(readFileSync(join(dir, "npm-shrinkwrap.json"), "utf8"))})
      const stripped = {manifest: {name: "p", version: "1.0.0", dependencies: {a: "^1"}}, lock: {name: "p", lockfileVersion: 3, packages: {"": {name: "p", dependencies: {a: "^1"}}, "node_modules/a": {version: "1.0.0"}}}}
      strip()
      expect(files()).toEqual(stripped)
      strip()
      expect(files()).toEqual(stripped)
      strip("--restore")
      expect(files()).toEqual({manifest, lock})
      expect(readdirSync(dir).sort()).toEqual(["npm-shrinkwrap.json", "package.json"])
    } finally {
      rmSync(dir, {recursive: true, force: true})
    }
  })

  it("allows a tool call", async () => {
    const stdout = await new Promise<string>((resolve, reject) => {
      const child = spawn(process.execPath, [join(__dirname, "../../plugin/allow.js")], {stdio: ["ignore", "pipe", "inherit"]})
      let text = ""
      child.stdout.on("data", chunk => (text += chunk))
      child.on("error", reject)
      child.on("close", () => resolve(text))
    })
    expect(JSON.parse(stdout)).toEqual({hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "allow"}})
  })

  it("lists the plugin in the marketplace with an npm source", () => {
    const marketplace = JSON.parse(readFileSync(join(__dirname, "../../../../.claude-plugin/marketplace.json"), "utf8"))
    expect(marketplace.name).toBe("simplex-chat")
    expect(marketplace.plugins).toEqual([expect.objectContaining({name: "simplex", source: {source: "npm", package: "@simplex-chat/mcp"}})])
  })
})
