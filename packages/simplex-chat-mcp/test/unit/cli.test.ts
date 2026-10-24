import {helpText, parseCommand} from "../../src/cli"
import {ExitError} from "../../src/log"

describe("commands", () => {
  it("parses each command", () => {
    expect(parseCommand(["session"])).toBe("session")
    expect(parseCommand(["daemon"])).toBe("daemon")
    expect(parseCommand(["hook", "session-start"])).toBe("hook session-start")
    expect(parseCommand(["link"])).toBe("link")
    expect(parseCommand(["reset-owners"])).toBe("reset-owners")
    expect(parseCommand(["--help"])).toBe("help")
  })

  it("refuses other commands and extra arguments", () => {
    const unknown = new ExitError("Unknown command. Run simplex-mcp --help.", 2)
    for (const args of [[], ["serve", "agent"], ["register", "agent"], ["hook"], ["link", "x"], ["session", "--port", "1"]]) expect(() => parseCommand(args)).toThrow(unknown)
  })

  it("documents the commands, the install and the data folder", () => {
    for (const text of ["simplex-mcp session", "simplex-mcp daemon", "simplex-mcp hook session-start", "simplex-mcp link", "simplex-mcp reset-owners", "/plugin install simplex@simplex-chat", "SIMPLEX_MCP_HOME"]) {
      expect(helpText).toContain(text)
    }
  })
})
