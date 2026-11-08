import {spawn} from "child_process"
import {readFileSync} from "fs"
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
  })

  it("starts the session server of the plugin's own build with the options as variables", () => {
    const {simplex} = read(".mcp.json").mcpServers
    expect(simplex.command).toBe("node")
    expect(simplex.args).toEqual(["${CLAUDE_PLUGIN_ROOT}/dist/cli.js", "session"])
    expect(simplex.env).toEqual({
      [configVariables.mainGroup]: "${user_config.main_group}",
      [configVariables.approve]: "${user_config.approve}",
      [configVariables.expireHours]: "${user_config.expire_hours}",
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
