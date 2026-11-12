import {join} from "path"
import {helpText, parseCommand, validAgentName} from "../../src/cli"
import {entryText} from "../../src/control"
import {ExitError} from "../../src/log"
import {validName} from "../../src/messages"
import {ServeOptions, registerHint} from "../../src/serve"

const home = "/home/alice"

function usage(args: string[]): string {
  try {
    parseCommand(args, home)
  } catch (error) {
    expect(error).toBeInstanceOf(ExitError)
    expect((error as ExitError).code).toBe(2)
    return (error as ExitError).message
  }
  throw new Error("arguments accepted")
}

describe("serve options", () => {
  it("has defaults", () => {
    expect(parseCommand(["serve", "agent"], home)).toEqual({
      command: "serve",
      options: {
        name: "agent",
        data: "/home/alice/.simplex-mcp/agent",
        dataGiven: false,
        approve: "requests",
        expireHours: 23,
        resetOwners: false,
        newToken: false,
        port: 5226,
        qrCode: "small",
        stopWait: 10,
      },
    })
  })

  it("reads every option", () => {
    const {options} = parseCommand(
      ["serve", "agent-2", "--data", "/tmp/a", "--display-name", "Agent Two", "--approve", "host", "--expire", "1", "--stop-wait", "0", "--port", "5227", "--qr-code", "large", "--new-token", "--reset-owners", "--owners", "--models", "gemma=llama-swap/gemma-4-26b", "--model-file", "/tmp/model/model"],
      home,
    ) as {options: object}
    expect(options).toEqual({
      name: "agent-2",
      data: "/tmp/a",
      dataGiven: true,
      displayName: "Agent Two",
      approve: "host",
      expireHours: 1,
      stopWait: 0,
      port: 5227,
      qrCode: "large",
      newToken: true,
      resetOwners: true,
      owners: [],
      model: {models: [{name: "gemma", model: "llama-swap/gemma-4-26b"}], file: "/tmp/model/model"},
    })
  })

  it("takes the models with their file, resolved to an absolute path", () => {
    const models = "gemma=llama-swap/gemma-4-26b,coder=llama-swap/qwen3-coder-next,large=hosted/large-latest"
    expect(parseCommand(["serve", "agent", "--model-file", "agent-model/model", "--models", models], home)).toMatchObject({
      options: {
        model: {
          models: [
            {name: "gemma", model: "llama-swap/gemma-4-26b"},
            {name: "coder", model: "llama-swap/qwen3-coder-next"},
            {name: "large", model: "hosted/large-latest"},
          ],
          file: join(process.cwd(), "agent-model", "model"),
        },
      },
    })
    const limits = `${"g".repeat(16)}=${"p".repeat(32)}/${"m".repeat(64)},a=p_1.x-y/m.1:2/z+q@r-s_t`
    expect(parseCommand(["serve", "agent", "--models", limits, "--model-file", "/tmp/model"], home)).toMatchObject({
      options: {model: {models: [{name: "g".repeat(16), model: `${"p".repeat(32)}/${"m".repeat(64)}`}, {name: "a", model: "p_1.x-y/m.1:2/z+q@r-s_t"}]}},
    })
  })

  it("refuses models that are not NAME=PROVIDER/MODEL items with unique names and models", () => {
    const refused = [
      "llama-swap/gemma-4-26b",
      "gemma=llama-swap",
      "gemma=-llama-swap/gemma-4-26b",
      "-gemma=llama-swap/gemma-4-26b",
      "Gemma=llama-swap/gemma-4-26b",
      `${"g".repeat(17)}=llama-swap/gemma-4-26b`,
      `gemma=${"p".repeat(33)}/gemma-4-26b`,
      `gemma=llama-swap/${"m".repeat(65)}`,
      "gemma=llama-swap/gemma 4",
      "gemma=llama-swap/`gemma`",
      "gemma=llama-swap/gemma-4-26b,",
      "gemma=llama-swap/gemma-4-26b,,coder=llama-swap/qwen3-coder-next",
      "gemma=llama-swap/gemma-4-26b,gemma=llama-swap/qwen3-coder-next",
      "gemma=llama-swap/gemma-4-26b,coder=llama-swap/gemma-4-26b",
      "",
    ]
    for (const models of refused) {
      expect(usage(["serve", "agent", "--models", models, "--model-file", "/tmp/model"])).toBe(`--models: NAME=PROVIDER/MODEL,... expected, got ${models}`)
    }
    expect(usage(["serve", "agent", "--models"])).toBe("--models: a value expected.")
  })

  it("takes --models and --model-file only together", () => {
    expect(usage(["serve", "agent", "--models", "gemma=llama-swap/gemma-4-26b"])).toBe("--models and --model-file: both expected.")
    expect(usage(["serve", "agent", "--model-file", "/tmp/model"])).toBe("--models and --model-file: both expected.")
  })

  it("refuses a model file in a folder not disjoint from the data folder", () => {
    const models = ["--models", "gemma=llama-swap/gemma-4-26b"]
    for (const file of ["/home/alice/.simplex-mcp/agent/model", "/home/alice/.simplex-mcp/agent", "/home/alice/.simplex-mcp/model"]) {
      expect(usage(["serve", "agent", ...models, "--model-file", file])).toBe(`--model-file: a file in a folder disjoint from the data folder expected, got ${file}`)
    }
    expect(usage(["serve", "agent", "--model-file", "/tmp/a/model/model", ...models, "--data", "/tmp/a"])).toBe("--model-file: a file in a folder disjoint from the data folder expected, got /tmp/a/model/model")
    expect(parseCommand(["serve", "agent", "--data", "/tmp/a", ...models, "--model-file", "/tmp/ab/model"], home)).toMatchObject({options: {model: {file: "/tmp/ab/model"}}})
  })

  it("takes the QR code size", () => {
    expect(parseCommand(["serve", "agent", "--qr-code", "small"], home)).toMatchObject({options: {qrCode: "small"}})
    expect(parseCommand(["serve", "agent", "--qr-code", "large"], home)).toMatchObject({options: {qrCode: "large"}})
    expect(usage(["serve", "agent", "--qr-code", "huge"])).toBe("--qr-code: small or large expected, got huge")
    expect(usage(["serve", "agent", "--qr-code"])).toBe("--qr-code: a value expected.")
  })

  it("takes --owners with and without a list", () => {
    expect(parseCommand(["serve", "agent", "--owners", "--port", "5300"], home)).toMatchObject({options: {owners: [], port: 5300}})
    expect(parseCommand(["serve", "agent", "--owners", "3:Alice Smith,7:Bob"], home)).toMatchObject({
      options: {owners: [{memberId: 3, name: "Alice Smith"}, {memberId: 7, name: "Bob"}]},
    })
  })

  it("splits --owners only at commas before an entry ID, so a name may have commas", () => {
    expect(parseCommand(["serve", "agent", "--owners", "3:Smith, Alice,7:Bob,,Jr,12:Lu,8"], home)).toMatchObject({
      options: {owners: [{memberId: 3, name: "Smith, Alice"}, {memberId: 7, name: "Bob,,Jr"}, {memberId: 12, name: "Lu,8"}]},
    })
  })

  it("checks ranges", () => {
    expect(usage(["serve", "agent", "--expire", "24"])).toBe("--expire: hours from 1 to 23 expected, got 24")
    expect(usage(["serve", "agent", "--expire", "0"])).toBe("--expire: hours from 1 to 23 expected, got 0")
    expect(parseCommand(["serve", "agent", "--stop-wait", "60"], home)).toMatchObject({options: {stopWait: 60}})
    expect(usage(["serve", "agent", "--stop-wait", "61"])).toBe("--stop-wait: seconds from 0 to 60 expected, got 61")
    expect(usage(["serve", "agent", "--stop-wait", "-1"])).toBe("--stop-wait: seconds from 0 to 60 expected, got -1")
    expect(usage(["serve", "agent", "--stop-wait", "2.5"])).toBe("--stop-wait: seconds from 0 to 60 expected, got 2.5")
    expect(usage(["serve", "agent", "--stop-wait"])).toBe("--stop-wait: a value expected.")
    expect(usage(["serve", "agent", "--port", "65536"])).toBe("--port: a port from 1 to 65535 expected, got 65536")
    expect(usage(["serve", "agent", "--port", "x"])).toBe("--port: a port from 1 to 65535 expected, got x")
    expect(usage(["serve", "agent", "--approve", "all"])).toBe("--approve: requests or host expected, got all")
    expect(usage(["serve", "agent", "--port"])).toBe("--port: a value expected.")
  })

  it("rejects arguments", () => {
    expect(usage(["serve", "agent", "--reset-owners", "--owners", "3:Alice"])).toBe("--reset-owners takes --owners without a list: member IDs change in a new agent control chat.")
    expect(usage(["serve", "agent", "--owners", "Alice"])).toBe("--owners: ID:NAME expected, got Alice")
    expect(usage(["serve", "Agent"])).toBe("NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.")
    expect(usage(["serve"])).toBe("NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.")
    expect(usage(["serve", "agent", "--colour"])).toBe("Unknown option --colour. Run simplex-mcp --help.")
    expect(usage(["start", "agent"])).toMatch(/^Usage: /)
  })

  it("checks the display name", () => {
    expect(usage(["serve", "agent", "--display-name", "agent.bot"])).toBe("--display-name: not a valid SimpleX name; try agentbot")
    expect(usage(["serve", "agent", "--display-name", " agent"])).toBe("--display-name: not a valid SimpleX name; try agent")
    expect(parseCommand(["serve", "agent", "--display-name", "Ridgeline Scout"], home)).toMatchObject({options: {displayName: "Ridgeline Scout"}})
  })
})

describe("NAME", () => {
  it("accepts lowercase letters, digits and single hyphens", () => {
    for (const name of ["agent", "a", "agent-2", "2agent", "a-b-c", "a-", "x".repeat(32)]) expect(validAgentName(name)).toBe(true)
    for (const name of ["", "-a", "a--b", "A", "a_b", "a.b", "x".repeat(33)]) expect(validAgentName(name)).toBe(false)
  })
})

describe("display names", () => {
  it("follows mkValidName", () => {
    expect(validName("Alice")).toBe("Alice")
    expect(validName("Alice Smith")).toBe("Alice Smith")
    expect(validName("Alice  Smith ")).toBe("Alice Smith")
    expect(validName("@alice")).toBe("alice")
    expect(validName("alice_12")).toBe("alice-12")
    expect(validName("a-b")).toBe("a-b")
    expect(validName("x".repeat(60))).toBe("x".repeat(50))
    expect(validName("$")).toBe("")
  })
})

describe("register options", () => {
  it("has defaults and reads options", () => {
    expect(parseCommand(["register", "agent"], home)).toEqual({command: "register", options: {name: "agent", data: "/home/alice/.simplex-mcp/agent", scope: "user"}})
    expect(parseCommand(["register", "agent", "--scope", "local", "--data", "/tmp/a"], home)).toEqual({command: "register", options: {name: "agent", data: "/tmp/a", scope: "local"}})
    expect(usage(["register", "agent", "--scope", "project"])).toBe("--scope: user or local expected, got project")
  })
})

describe("printed commands", () => {
  const platform = Object.getOwnPropertyDescriptor(process, "platform")!
  const onPlatform = (value: NodeJS.Platform): void => {
    Object.defineProperty(process, "platform", {...platform, value})
  }
  const hint = (data?: string): string => registerHint({...(parseCommand(["serve", "agent"], home) as {options: ServeOptions}).options, ...(data === undefined ? {} : {data, dataGiven: true})})

  afterEach(() => Object.defineProperty(process, "platform", platform))

  it("quote owner entries and the data folder for a POSIX shell", () => {
    onPlatform("linux")
    expect(entryText({memberId: 3, name: "Alice"})).toBe("3:Alice")
    expect(entryText({memberId: 3, name: "Ana-Lu_2"})).toBe("3:Ana-Lu_2")
    expect(entryText({memberId: 3, name: "Alice Smith"})).toBe("'3:Alice Smith'")
    expect(entryText({memberId: 3, name: "O'Neil"})).toBe("'3:O'\\''Neil'")
    expect(hint()).toBe("Add the agent to Claude Code:\n  npx -y @simplex-chat/mcp register agent")
    expect(hint("/tmp/a-1/b_2.c")).toBe("Add the agent to Claude Code:\n  npx -y @simplex-chat/mcp register agent --data /tmp/a-1/b_2.c")
    expect(hint("/tmp/my agent's")).toBe("Add the agent to Claude Code:\n  npx -y @simplex-chat/mcp register agent --data '/tmp/my agent'\\''s'")
  })

  it("quote with double quotes on Windows", () => {
    onPlatform("win32")
    expect(entryText({memberId: 3, name: "Ana-Lu_2"})).toBe("3:Ana-Lu_2")
    expect(entryText({memberId: 3, name: "Alice Smith"})).toBe('"3:Alice Smith"')
    expect(hint("C:\\Users\\alice\\agent")).toBe("Add the agent to Claude Code:\n  npx -y @simplex-chat/mcp register agent --data C:\\Users\\alice\\agent")
    expect(hint("C:\\Users\\Ana Lu\\agent")).toBe('Add the agent to Claude Code:\n  npx -y @simplex-chat/mcp register agent --data "C:\\Users\\Ana Lu\\agent"')
  })
})

describe("help", () => {
  it("matches the manual", () => {
    expect(parseCommand(["--help"], home)).toEqual({command: "help"})
    expect(helpText).toContain("  --owners [ID:NAME,...] only these owners approve; without a list, the link\n")
    expect(helpText).toContain("  --stop-wait SECONDS    at stop, wait SECONDS for cancelled cards to be sent,\n                         0 to 60 (default: 10)\n")
    expect(helpText).toContain("  --models MODELS        opencode models NAME=PROVIDER/MODEL, comma-separated;\n                         /model NAME selects one in the agent control chat;\n                         the first is the default; requires --model-file\n")
    expect(helpText).toContain("  --model-file FILE      file with the selected PROVIDER/MODEL for the script\n                         that runs opencode; in a folder disjoint from the\n                         data folder\n")
    expect(helpText.endsWith("--reset-owners takes --owners without a list.\n")).toBe(true)
  })
})
