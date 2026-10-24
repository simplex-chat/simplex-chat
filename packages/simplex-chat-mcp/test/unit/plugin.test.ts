import {spawn} from "child_process"
import {chmodSync, mkdtempSync, readFileSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {delimiter, join} from "path"
import {configVariables, defaultConfig} from "../../src/config"

const read = (file: string): any => JSON.parse(readFileSync(join(__dirname, "../..", file), "utf8"))
const packageJson = read("package.json")

describe("plugin", () => {
  it("has the package version and the config defaults", () => {
    const plugin = read(".claude-plugin/plugin.json")
    expect(plugin.name).toBe("simplex")
    expect(plugin.version).toBe(packageJson.version)
    expect(plugin.userConfig.main_group.default).toBe(defaultConfig.mainGroup)
    expect(plugin.userConfig.approve.default).toBe(defaultConfig.approve)
    expect(plugin.userConfig.expire_hours.default).toBe(defaultConfig.expireHours)
  })

  it("starts the session server with the options as variables", () => {
    const {simplex} = read(".mcp.json").mcpServers
    expect(simplex.command).toBe("node")
    expect(simplex.args).toEqual(["${CLAUDE_PLUGIN_ROOT}/plugin/launch.js", "session"])
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

  it("runs the hook and allows the plugin's tools", () => {
    const {hooks} = read("hooks/hooks.json")
    expect(hooks.SessionStart[0].hooks[0]).toEqual({type: "command", command: 'node "${CLAUDE_PLUGIN_ROOT}/plugin/launch.js" hook session-start', timeout: 600})
    expect(hooks.PreToolUse[0].matcher).toBe("mcp__plugin_simplex_simplex__.*")
    expect(hooks.PreToolUse[0].hooks[0]).toEqual({type: "command", command: 'node "${CLAUDE_PLUGIN_ROOT}/plugin/allow.js"'})
  })

  it("ships the plugin files in the package", () => {
    expect(packageJson.files).toEqual(expect.arrayContaining([".claude-plugin", ".mcp.json", "hooks", "plugin", "skills"]))
  })

  it.each([
    ["link", "link"],
    ["reset-owners", "reset-owners"],
  ])("offers the %s skill to the user only", (folder, command) => {
    const skill = readFileSync(join(__dirname, "../../skills", folder, "SKILL.md"), "utf8")
    expect(skill).toContain(`name: ${folder}\n`)
    expect(skill).toContain("disable-model-invocation: true\n")
    expect(skill).toContain('!`node "${CLAUDE_PLUGIN_ROOT}/plugin/launch.js" ' + command + "`")
  })

  it("allows a tool call", async () => {
    const output = await run(process.execPath, [join(__dirname, "../../plugin/allow.js")], process.env)
    expect(JSON.parse(output.stdout)).toEqual({hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "allow"}})
  })

  it("launches the pinned package through npx with the arguments and passes the exit code", async () => {
    const bin = fakeNpx(`echo "$@"\nexit 7`)
    const output = await run(process.execPath, [join(__dirname, "../../plugin/launch.js"), "hook", "session-start"], {...process.env, PATH: `${bin}${delimiter}${process.env.PATH}`})
    expect(output.stdout.trim()).toBe(`-y @simplex-chat/mcp@${packageJson.version} hook session-start`)
    expect(output.code).toBe(7)
  })

  it.each(["SIGTERM", "SIGINT"] as const)("forwards %s to the launched process", async signal => {
    const bin = fakeNpx(`trap 'echo got ${signal}; exit 3' ${signal.slice(3)}\necho ready\nwhile true; do sleep 0.1; done`)
    const child = spawn(process.execPath, [join(__dirname, "../../plugin/launch.js"), "session"], {env: {...process.env, PATH: `${bin}${delimiter}${process.env.PATH}`}, stdio: ["ignore", "pipe", "inherit"]})
    let stdout = ""
    const ready = new Promise<void>(resolve => child.stdout.on("data", chunk => {
      stdout += chunk
      if (stdout.includes("ready")) resolve()
    }))
    const closed = new Promise<number | null>(resolve => child.on("close", resolve))
    await ready
    child.kill(signal)
    expect(await closed).toBe(3)
    expect(stdout).toContain(`got ${signal}`)
  })

  it("lists the plugin in the marketplace with an npm source", () => {
    const marketplace = JSON.parse(readFileSync(join(__dirname, "../../../../.claude-plugin/marketplace.json"), "utf8"))
    expect(marketplace.name).toBe("simplex-chat")
    expect(marketplace.plugins).toEqual([expect.objectContaining({name: "simplex", source: {source: "npm", package: "@simplex-chat/mcp"}})])
  })
})

function fakeNpx(body: string): string {
  const bin = mkdtempSync(join(tmpdir(), "fake-npx-"))
  const npx = join(bin, "npx")
  writeFileSync(npx, `#!/bin/sh\n${body}\n`)
  chmodSync(npx, 0o755)
  return bin
}

function run(command: string, args: string[], env: NodeJS.ProcessEnv): Promise<{stdout: string; code: number | null}> {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {env, stdio: ["ignore", "pipe", "inherit"]})
    let stdout = ""
    child.stdout.on("data", chunk => (stdout += chunk))
    child.on("error", reject)
    child.on("close", code => resolve({stdout, code}))
  })
}
