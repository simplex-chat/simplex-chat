import {Activity, ActivityState} from "../../src/activity"

function event(name: string, fields: object = {}, session = "s1"): object {
  return {session_id: session, hook_event_name: name, ...fields}
}

function ownCall(session: string): object {
  return event("PreToolUse", {tool_name: "mcp__simplex-agent__receive", tool_use_id: `receive ${session}`}, session)
}

function joined(...sessions: string[]): Activity {
  const activity = new Activity("agent")
  for (const session of sessions) activity.record(ownCall(session))
  return activity
}

function runningTool(name: unknown, input: unknown = {}): ActivityState["tool"] {
  const activity = joined("s1")
  activity.record(event("PreToolUse", {tool_name: name, tool_use_id: "t1", tool_input: input}))
  return activity.state().tool
}

describe("Claude Code activity", () => {
  it("tracks a session from its first event with a tool of this server, and ignores other sessions and the servers of other agents", () => {
    const activity = new Activity("agent")
    activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "t1"}))
    activity.record(event("PreToolUse", {tool_name: "mcp__simplex-agent2__receive", tool_use_id: "t2"}))
    activity.record(event("PreToolUse", {tool_name: "mcp__simplex-other__receive", tool_use_id: "t3"}))
    activity.record(event("PermissionRequest", {tool_name: "Bash"}))
    expect(activity.state()).toEqual({asking: false, compacting: false})
    activity.record(event("PermissionRequest", {tool_name: "mcp__simplex-agent__send"}))
    activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "t4"}))
    activity.record(event("PreToolUse", {tool_name: "Grep", tool_use_id: "t5"}, "s2"))
    expect(activity.state()).toEqual({asking: true, compacting: false, tool: {session: "s1", name: "Bash"}})
  })

  it("shows the latest running tool other than this server's tools and Agent, and the one before it when the latest ends", () => {
    const activity = joined("s1")
    activity.record(event("PreToolUse", {tool_name: "Read", tool_use_id: "t1", tool_input: {file_path: "/work/src/index.ts"}}))
    activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "t2", tool_input: {command: "npm test"}}))
    activity.record(event("PreToolUse", {tool_name: "Agent", tool_use_id: "t3", tool_input: {subagent_type: "Explore"}}))
    activity.record(event("PreToolUse", {tool_name: "mcp__simplex-agent__send", tool_use_id: "t4"}))
    expect(activity.state().tool).toEqual({session: "s1", name: "Bash"})
    activity.record(event("PostToolUse", {tool_name: "Bash", tool_use_id: "t2"}))
    expect(activity.state().tool).toEqual({session: "s1", name: "Read", file: "index.ts"})
    activity.record(event("PostToolUseFailure", {tool_name: "Read", tool_use_id: "t1"}))
    activity.record(event("PreToolUse", {tool_name: "WebFetch", tool_use_id: "t5"}))
    activity.record(event("PermissionDenied", {tool_name: "WebFetch", tool_use_id: "t5"}))
    expect(activity.state().tool).toBeUndefined()
  })

  it("drops the running tools of an agent at its PostToolBatch, as a denied, blocked or cancelled call has no end event, and keeps those of other agents and sessions", () => {
    const activity = joined("s1", "s2")
    activity.record(event("PreToolUse", {tool_name: "Grep", tool_use_id: "t1"}, "s2"))
    activity.record(event("PreToolUse", {tool_name: "Read", tool_use_id: "t2", agent_id: "a1", agent_type: "Explore"}))
    activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "t3"}))
    activity.record(event("PostToolBatch", {tool_calls: [{tool_name: "Bash", tool_use_id: "t3"}]}))
    expect(activity.state().tool).toEqual({session: "s1", agent: "a1", name: "Read"})
    activity.record(event("PostToolBatch", {agent_id: "a1", agent_type: "Explore", tool_calls: []}))
    expect(activity.state().tool).toEqual({session: "s2", name: "Grep"})
  })

  it("ignores a PreToolUse that arrives after its PostToolUse, also after Stop", () => {
    const activity = joined("s1")
    activity.record(event("PostToolUse", {tool_name: "Read", tool_use_id: "t1"}))
    activity.record(event("PreToolUse", {tool_name: "Read", tool_use_id: "t1"}))
    activity.record(event("PostToolUseFailure", {tool_name: "Bash", tool_use_id: "t2"}))
    activity.record(event("Stop"))
    activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "t2"}))
    expect(activity.state().tool).toBeUndefined()
    activity.record(event("PreToolUse", {tool_name: "Grep", tool_use_id: "t3"}))
    expect(activity.state().tool).toEqual({session: "s1", name: "Grep"})
  })

  it("shows a permission prompt from its request until a tool of its session ends, fails or is denied, a tool batch of its session ends, or the turn stops", () => {
    const activity = joined("s1", "s2")
    const asking: boolean[] = []
    for (const end of ["PostToolUse", "PostToolUseFailure", "PermissionDenied", "PostToolBatch", "Stop", "StopFailure"]) {
      activity.record(event("PermissionRequest", {tool_name: "Bash"}))
      activity.record(event(end, {tool_name: "Bash", tool_use_id: end}, "s2"))
      asking.push(activity.state().asking)
      activity.record(event(end, {tool_name: "Bash", tool_use_id: end}))
      asking.push(activity.state().asking)
    }
    expect(asking).toEqual([true, false, true, false, true, false, true, false, true, false, true, false])
  })

  it("shows compaction until PostCompact or a PreToolUse of the main agent of its session, and the latest running subagent until the SubagentStop of its agent_id", () => {
    const activity = joined("s1", "s2")
    activity.record(event("PreCompact", {trigger: "auto"}))
    expect(activity.state().compacting).toBe(true)
    activity.record(event("PostCompact", {trigger: "auto"}))
    expect(activity.state().compacting).toBe(false)
    activity.record(event("PreCompact", {trigger: "auto"}))
    activity.record(event("PreToolUse", {tool_name: "Read", tool_use_id: "t1", agent_id: "a1", agent_type: "Explore"}))
    expect(activity.state().compacting).toBe(true)
    activity.record(event("PreCompact", {trigger: "auto"}, "s2"))
    activity.record(ownCall("s1"))
    expect(activity.state().compacting).toBe(true)
    activity.record(event("PostCompact", {trigger: "auto"}, "s2"))
    expect(activity.state().compacting).toBe(false)
    activity.record(event("SubagentStart", {agent_id: "a1", agent_type: "Explore"}))
    activity.record(event("SubagentStart", {agent_id: "a2", agent_type: "Plan"}))
    activity.record(event("SubagentStop", {agent_id: "a3", agent_type: "Plan"}))
    expect(activity.state().subagent).toEqual({session: "s1", name: "Plan"})
    activity.record(event("SubagentStop", {agent_id: "a2", agent_type: "Plan"}))
    expect(activity.state().subagent).toEqual({session: "s1", name: "Explore"})
    activity.record(event("SubagentStop", {agent_id: "a1", agent_type: "Explore"}))
    expect(activity.state().subagent).toBeUndefined()
  })

  it("clears the tools, subagents, permission prompt and compaction of a session at UserPromptSubmit, Stop, StopFailure and SessionEnd, and keeps those of other sessions", () => {
    const ends: [string, object][] = [["UserPromptSubmit", {prompt: "next"}], ["Stop", {stop_hook_active: false}], ["StopFailure", {error: "rate_limit"}], ["SessionEnd", {reason: "clear"}]]
    const states: ActivityState[] = []
    for (const [end, fields] of ends) {
      const activity = joined("s1", "s2")
      activity.record(event("SubagentStart", {agent_id: "a1", agent_type: "Explore"}, "s2"))
      activity.record(event("PreToolUse", {tool_name: "Grep", tool_use_id: "t1"}, "s2"))
      activity.record(event("PermissionRequest", {tool_name: "Grep"}, "s2"))
      activity.record(event("PreCompact", {trigger: "auto"}, "s2"))
      activity.record(event("SubagentStart", {agent_id: "a2", agent_type: "Plan"}))
      activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "t2"}))
      activity.record(event("PermissionRequest", {tool_name: "Bash"}))
      activity.record(event("PreCompact", {trigger: "manual"}))
      activity.record(event(end, fields))
      states.push(activity.state())
      activity.record(event(end, fields, "s2"))
      states.push(activity.state())
    }
    const kept = {asking: true, compacting: true, tool: {session: "s2", name: "Grep"}, subagent: {session: "s2", name: "Explore"}}
    expect(states).toEqual(ends.flatMap(() => [kept, {asking: false, compacting: false}]))
  })

  it("keeps 16 sessions and drops the oldest with its activity", () => {
    const activity = joined(...Array.from({length: 16}, (_, n) => `s${n}`))
    activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "t1"}, "s0"))
    activity.record(event("SubagentStart", {agent_id: "a1", agent_type: "Explore"}, "s0"))
    activity.record(event("PermissionRequest", {tool_name: "Bash"}, "s0"))
    activity.record(event("PreCompact", {trigger: "auto"}, "s0"))
    activity.record(ownCall("s16"))
    expect(activity.state()).toEqual({asking: false, compacting: false})
    activity.record(event("PreToolUse", {tool_name: "Grep", tool_use_id: "t2"}, "s1"))
    activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "t3"}, "s0"))
    expect(activity.state().tool).toEqual({session: "s1", name: "Grep"})
  })

  it("keeps 64 running tools, 64 subagents and the last 256 ended IDs", () => {
    const activity = joined("s1")
    for (let n = 0; n <= 64; n++) {
      activity.record(event("PreToolUse", {tool_name: `tool${n}`, tool_use_id: `t${n}`}))
      activity.record(event("SubagentStart", {agent_id: `a${n}`, agent_type: `type${n}`}))
    }
    for (let n = 64; n >= 2; n--) {
      activity.record(event("PostToolUse", {tool_name: `tool${n}`, tool_use_id: `t${n}`}))
      activity.record(event("SubagentStop", {agent_id: `a${n}`, agent_type: `type${n}`}))
    }
    expect(activity.state()).toEqual({asking: false, compacting: false, tool: {session: "s1", name: "tool1"}, subagent: {session: "s1", name: "type1"}})
    activity.record(event("PostToolUse", {tool_name: "tool1", tool_use_id: "t1"}))
    activity.record(event("SubagentStop", {agent_id: "a1", agent_type: "type1"}))
    expect(activity.state()).toEqual({asking: false, compacting: false})
    for (let n = 0; n <= 256; n++) activity.record(event("PostToolUse", {tool_name: "Bash", tool_use_id: `e${n}`}))
    activity.record(event("PreToolUse", {tool_name: "Bash", tool_use_id: "e0"}))
    activity.record(event("PreToolUse", {tool_name: "Grep", tool_use_id: "e1"}))
    expect(activity.state().tool).toEqual({session: "s1", name: "Bash"})
  })

  it("keeps file base names, tool names and subagent types only when they pass their patterns", () => {
    const fileOf = (input: unknown): string | undefined => runningTool("Read", input)?.file
    expect(fileOf({file_path: "/work/src/index.ts"})).toBe("index.ts")
    expect(fileOf({notebook_path: "/work/analysis.ipynb"})).toBe("analysis.ipynb")
    expect(fileOf({file_path: 7, notebook_path: "/work/notes.md"})).toBe("notes.md")
    expect(fileOf({file_path: `/work/${"a".repeat(61)}.md`})).toBe(`${"a".repeat(61)}.md`)
    const filtered = [{file_path: "/work/my notes.md"}, {file_path: "/work/a`b.md"}, {file_path: "/work/über.md"}, {file_path: `/work/${"a".repeat(62)}.md`}, {file_path: "/"}, {}, "/work/index.ts"]
    expect(filtered.map(fileOf)).toEqual(Array(filtered.length).fill(undefined))
    expect(runningTool("mcp__github__search_repositories")).toEqual({session: "s1", name: "mcp__github__search_repositories"})
    expect(runningTool("a".repeat(64))).toEqual({session: "s1", name: "a".repeat(64)})
    expect(["Run rm", "Read`", "a".repeat(65), 42].map(name => runningTool(name))).toEqual(Array(4).fill({session: "s1"}))
    const subagentOf = (type: unknown): ActivityState["subagent"] => {
      const activity = joined("s1")
      activity.record(event("SubagentStart", {agent_id: "a1", agent_type: type}))
      return activity.state().subagent
    }
    expect(["Explore", "my-plugin:reviewer", ""].map(subagentOf)).toEqual([{session: "s1", name: "Explore"}, {session: "s1"}, {session: "s1"}])
  })

  it("ignores bodies that are not objects, and fields of other types", () => {
    const activity = joined("s1")
    const bodies = [
      null,
      42,
      "PreCompact",
      [],
      {hook_event_name: "PreCompact"},
      {session_id: 1, hook_event_name: "PreCompact"},
      {session_id: "s1", hook_event_name: ["PreCompact"]},
      event("PreToolUse", {tool_name: "Bash", tool_use_id: 7}),
      event("SubagentStart", {agent_id: 7, agent_type: "Explore"}),
    ]
    for (const body of bodies) activity.record(body)
    expect(activity.state()).toEqual({asking: false, compacting: false})
    activity.record(event("PreCompact"))
    expect(activity.state().compacting).toBe(true)
  })
})
