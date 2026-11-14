import {helpText, parseCommand} from "../../src/cli"
import {ExitError} from "../../src/log"

describe("plugin commands", () => {
  it("parses each command", () => {
    expect(parseCommand(["session"])).toEqual({command: "session"})
    expect(parseCommand(["service"])).toEqual({command: "service"})
    expect(parseCommand(["hook", "session-start"])).toEqual({command: "hook session-start"})
    expect(parseCommand(["link"])).toEqual({command: "link"})
    expect(parseCommand(["reset-owners"])).toEqual({command: "reset-owners"})
    expect(parseCommand(["--help"])).toEqual({command: "help"})
  })

  it("refuses other commands and extra arguments", () => {
    const usage = new ExitError("Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.", 2)
    for (const args of [[], ["hook"], ["link", "x"], ["session", "--port", "1"], ["constructor"], ["__proto__"], ["toString"]]) expect(() => parseCommand(args)).toThrow(usage)
  })

  it("documents the commands, the install and the data folder", () => {
    for (const text of ["simplex-mcp session", "simplex-mcp service", "simplex-mcp hook session-start", "simplex-mcp link", "simplex-mcp reset-owners", "/plugin install simplex@simplex-chat", "SIMPLEX_MCP_HOME"]) {
      expect(helpText).toContain(text)
    }
  })
})
