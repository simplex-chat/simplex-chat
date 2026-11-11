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
    expect(plugin.userConfig.approve.description).toBe("requests: you approve connections and messages in the SimpleX app. host: Claude Code asks you instead, after an admin or owner approves this in the session's group.")
    expect(plugin.userConfig.expire_hours.default).toBe(defaultConfig.expireHours)
    expect(Object.keys(plugin.userConfig)).toEqual(["main_group", "approve", "expire_hours"])
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

  it("ships the build, its bundled dependencies and the plugin files in the package, without a lockfile", () => {
    expect(packageJson.files).toEqual(["dist", "README.md", ".claude-plugin", ".mcp.json", "hooks", "plugin"])
    expect(packageJson.bundleDependencies).toBe(true)
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
