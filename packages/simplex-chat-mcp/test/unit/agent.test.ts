import {profileName, sessionName} from "../../src/agent"

describe("session profile names", () => {
  it("use the title, else the project folder, else Claude Code", () => {
    expect(sessionName("  auth refactor ", "/p/app")).toBe("auth refactor")
    expect(sessionName(undefined, "/p/simplex-chat-6")).toBe("simplex-chat-6")
    expect(sessionName("", "/")).toBe("Claude Code")
  })

  it("start with SimpleX MCP and stay valid SimpleX names of at most 50 characters", () => {
    expect(profileName("simplex-chat-6")).toBe("SimpleX MCP - simplex-chat-6")
    expect(profileName("a".repeat(80))).toBe(`SimpleX MCP - ${"a".repeat(36)}`)
    expect(profileName("fix #12, then @bob")).toBe("SimpleX MCP - fix 12 then bob")
  })
})
