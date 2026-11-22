import {Agents, agentsLine, applyJournal, cardText, clock, tokensText} from "../../src/agents"

const start = {type: "Explore", description: "find the suspend callers", toolUse: "toolu_1", transcript: "/t/agent-a1.jsonl"}
const journal = (lines: object[]): string[] => lines.map(line => JSON.stringify(line))

describe("agents", () => {
  it("shows a working agent's steps, tokens, time in 5 s steps and its open step", () => {
    const agents = new Agents()
    const agent = agents.start("a1", start, 0)
    agents.step("a1", {id: "s1", text: "📖 Read SuspendChat.swift"}, 1_000)
    agent.tokens = 38_200
    expect(cardText(agent, 67_000)).toBe("!- 🤖 Explore · find the suspend callers!\n!- 1 step · 38.2k tokens · 1:05 · now: 📖 Read SuspendChat.swift!")
  })

  it("shows an agent stalled 2 minutes after its last step ended, and 10 minutes while a step runs", () => {
    const agents = new Agents()
    const agent = agents.start("a1", start, 0)
    agents.step("a1", {id: "s1", text: "💻 cabal build"}, 0)
    const running = cardText(agent, 590_000)
    agents.step("a1", {id: "s1", text: "💻 cabal build", end: true}, 600_000)
    expect([running, cardText(agent, 730_000)]).toEqual(["!- 🤖 Explore · find the suspend callers!\n!- 1 step · 9:50 · now: 💻 cabal build!", "!- 🤖 Explore · find the suspend callers!\n!- 1 step · 12:10 · ⚠️ no step for 2:10 · last: 💻 cabal build!"])
  })

  it("shows an ended agent with its status, time, steps and tokens of its task, and its report with ǃ for each !", () => {
    const agents = new Agents()
    const agent = agents.start("a1", start, 0)
    agents.report("a1", "Found 6 callers!")
    agents.stop("a1", "Done.", 160_000)
    agents.task({toolUse: "toolu_1", status: "completed", tokens: 61_500, steps: 22, ms: 160_000}, 161_000)
    expect(cardText(agent, 200_000)).toBe("!- 🤖 Explore · find the suspend callers · ✅ 2:40 · 22 steps · 61.5k tokens!\n!- Found 6 callersǃ!")
  })

  it("shows a workflow's phases in order of their first agent, done ones marked, its working agents, tokens and time", () => {
    const agents = new Agents()
    const workflow = agents.workflow({toolUse: "toolu_2", run: "wf_1", name: "review-changes", folder: "/t/wf_1"}, 0)
    applyJournal(workflow, journal([{type: "launched"}, {type: "started", key: "k1", agentId: "b1", phase: "Review"}, {type: "result", key: "k1", agentId: "b1"}, {type: "started", key: "k2", agentId: "b2", phase: "Verify"}, {type: "started", key: "k3", agentId: "b3", phase: "Verify"}]))
    workflow.tokens.set("b1", 400_000).set("b2", 12_000)
    expect(cardText(workflow, 390_000)).toBe("!- ⚙️ review-changes · Review 1/1 ✅ · Verify 0/2 · 2 working · 412k tokens · 6:30!")
  })

  it("counts working agents, the agents of workflows and stalled agents, with the longest stall and each workflow's phase", () => {
    const agents = new Agents()
    agents.start("a1", start, 0)
    agents.start("a2", start, 125_000)
    const workflow = agents.workflow({toolUse: "toolu_2", run: "wf_1", name: "review", folder: "/t/wf_1"}, 0)
    applyJournal(workflow, journal([{type: "started", key: "k1", agentId: "b1", phase: "Verify"}]))
    expect(agentsLine(agents.all(), 130_000)).toBe("🤖 3 agents working · 1 stalled 2:10 · ⚙️ review Verify 0/1")
  })

  it("writes tokens with one decimal under 100k and times as M:SS or H:MM:SS", () => {
    expect([tokensText(850), tokensText(38_249), tokensText(412_000), tokensText(1_250_000), clock(65_000), clock(3_605_000)]).toEqual(["850 tokens", "38.2k tokens", "412k tokens", "1.2M tokens", "1:05", "1:00:05"])
  })
})
