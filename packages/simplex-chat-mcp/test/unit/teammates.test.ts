import {existsSync, mkdirSync, mkdtempSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {ChatEvent, ChatResponse, T} from "@simplex-chat/types"
import {Core, EventHandler} from "../../src/core"
import {Teammates, maxTeammates, removeStale} from "../../src/teammates"
import {FakeChat, contact, direct, member, until, user} from "./fixtures"

const hostGroup = 20
const session = 3

class FakeHost extends FakeChat {
  private readonly waits: {type: string; predicate?: (event: ChatEvent) => boolean; resolve: (event: ChatEvent | undefined) => void}[] = []
  readonly users = new Map<number, number>()
  nextConn = 900
  nextContact = 50
  leaves = true
  failAdd = false
  core?: FakeCore

  wait(type: string, predicate?: (event: ChatEvent) => boolean): Promise<ChatEvent | undefined> {
    if (type === "leftMember" && !this.leaves) return Promise.resolve(undefined)
    return new Promise(resolve => this.waits.push({type, predicate, resolve}))
  }

  emit(event: ChatEvent): void {
    for (const wait of [...this.waits]) {
      if (wait.type !== event.type || (wait.predicate !== undefined && !wait.predicate(event))) continue
      this.waits.splice(this.waits.indexOf(wait), 1)
      wait.resolve(event)
    }
  }

  async sendChatCmd(cmd: string): Promise<ChatResponse> {
    if (cmd.startsWith("/_delete :")) {
      this.commands.push(cmd)
      return {type: "contactConnectionDeleted"} as unknown as ChatResponse
    }
    if (cmd !== `/_connect ${user.userId}`) return super.sendChatCmd(cmd)
    this.commands.push(cmd)
    const connId = this.nextConn++
    return {type: "invitation", connLinkInvitation: {connFullLink: `https://simplex.chat/invitation#${connId}`}, connection: {pccConnId: connId}} as unknown as ChatResponse
  }

  async apiAddMember(groupId: number, contactId: number, memberRole: T.GroupMemberRole): Promise<T.GroupMember> {
    if (this.failAdd) throw new Error("no relay")
    const added = await super.apiAddMember(groupId, contactId, memberRole)
    const userId = this.users.get(contactId)!
    setImmediate(() => this.core?.receive({type: "receivedGroupInvitation", user: {userId}, groupInfo: {groupId: 300 + contactId}} as unknown as ChatEvent))
    return added
  }
}

class FakeCore {
  readonly calls: string[] = []
  readonly sent: {userId: number; groupId: number; texts: string[]; priority?: number}[] = []
  readonly profiles: T.Profile[] = []
  handler?: EventHandler
  active = 1
  connects = true
  private nextUser = 2

  constructor(private readonly host: FakeHost) {
    host.core = this
  }

  receive(event: ChatEvent): void {
    void this.handler?.(event)
  }

  async apiCreateActiveUser(profile: T.Profile): Promise<T.User> {
    this.profiles.push(profile)
    this.active = this.nextUser++
    this.calls.push(`create ${this.active}`)
    return {userId: this.active, profile} as unknown as T.User
  }

  async apiSetActiveUser(userId: number): Promise<T.User> {
    this.active = userId
    return {userId} as unknown as T.User
  }

  async apiConnectPlan(userId: number, link: string): Promise<unknown[]> {
    this.calls.push(`plan ${userId} as ${this.active}`)
    return [{}, {connFullLink: link}]
  }

  async apiConnect(userId: number, _incognito: boolean, link: {connFullLink: string}): Promise<void> {
    this.calls.push(`connect ${userId} as ${this.active}`)
    const contactId = this.host.nextContact++
    const connId = Number(link.connFullLink.split("#")[1])
    const connected = {...contact(contactId, `teammate ${userId}`), activeConn: {connId, connStatus: {type: "ready"}}} as unknown as T.Contact
    this.host.chats.set(`@${contactId}`, {chatInfo: direct(connected), items: []})
    this.host.users.set(contactId, userId)
    if (this.connects) setImmediate(() => this.host.emit({type: "contactConnected", user, contact: connected} as unknown as ChatEvent))
  }

  async apiJoinGroup(groupId: number): Promise<void> {
    const userId = this.active
    this.calls.push(`join ${groupId} as ${userId}`)
    setImmediate(() => this.receive({type: "userJoinedGroup", user: {userId}, groupInfo: {groupId}} as unknown as ChatEvent))
  }

  async apiSendMessages([, groupId]: [T.ChatType, number], messages: (T.ComposedMessage & {priority?: number})[]): Promise<T.AChatItem[]> {
    this.sent.push({userId: this.active, groupId, texts: messages.map(({msgContent}) => msgContent.text), priority: messages[0]?.priority})
    return []
  }

  async apiLeaveGroup(groupId: number): Promise<void> {
    const userId = this.active
    this.calls.push(`leave ${groupId} as ${userId}`)
    const contactId = [...this.host.users].find(([, id]) => id === userId)![0]
    setImmediate(() => this.host.emit({type: "leftMember", member: {groupMemberId: 2000 + contactId}} as unknown as ChatEvent))
  }

  async apiDeleteUser(userId: number, delSMPQueues: boolean): Promise<void> {
    this.calls.push(`delete ${userId} as ${this.active}${delSMPQueues ? " with queues" : ""}`)
  }

  async close(): Promise<void> {
    this.calls.push("close")
  }
}

let root: string

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-teammates-"))
  jest.spyOn(process.stderr, "write").mockImplementation(() => true)
})

afterEach(() => {
  jest.restoreAllMocks()
  rmSync(root, {recursive: true, force: true})
})

function setup(): {host: FakeHost; core: FakeCore; teammates: Teammates; opened: string[]} {
  const host = new FakeHost()
  const core = new FakeCore(host)
  const opened: string[] = []
  const opener = async (folder: string): Promise<Core> => {
    opened.push(folder)
    return {chat: core as never, user: {userId: 1} as T.User, events: async handler => void (core.handler = handler)}
  }
  return {host, core, opened, teammates: new Teammates(join(root, "agents"), {chat: host.api(), userId: user.userId, groupId: hostGroup, session}, opener)}
}

describe("Teammates", () => {
  it("open one core in a new folder, join a teammate with its own profile as a member of the session's group, and post its messages under that profile, silently", async () => {
    const {host, core, teammates, opened} = setup()
    mkdirSync(join(root, "agents"))
    writeFileSync(join(root, "agents", "stale"), "")
    const description = `Review the diff ${"x".repeat(200)}`
    expect(teammates.start("reviewer", description)).toBe("started")
    expect(teammates.start("reviewer")).toBe("known")
    expect(await teammates.post("reviewer", ["The diff has no defect.", "Details follow."])).toBe(true)
    expect(opened).toEqual([join(root, "agents")])
    expect(existsSync(join(root, "agents", "stale"))).toBe(false)
    expect(core.profiles).toEqual([{displayName: "reviewer", fullName: "", peerType: T.ChatPeerType.Agent, shortDescr: [...description].slice(0, 160).join("")}])
    expect(core.calls).toEqual(["create 2", "plan 2 as 2", "connect 2 as 2", "join 350 as 2"])
    expect(host.customData).toEqual([{chat: "@50", customData: {simplexMcpTeammate: {session, name: "reviewer"}}}])
    expect(host.invitations).toEqual([{groupId: hostGroup, viaGroupId: 0, groupMemberId: 50, memberRole: T.GroupMemberRole.Member}])
    expect(core.sent).toEqual([{userId: 2, groupId: 350, texts: ["The diff has no defect.", "Details follow."], priority: -1}])
    expect(teammates.isMember(member(2050, "reviewer"))).toBe(true)
    expect(teammates.isMember(member(2051, "tester"))).toBe(false)
    expect(await teammates.post("tester", ["Tests pass."])).toBe(false)
  })

  it("send the messages of a joining teammate in order once it joins, also those that arrive while the earlier ones are sent", async () => {
    const {core, teammates} = setup()
    teammates.start("tester")
    const first = teammates.post("tester", ["one"])
    const second = teammates.post("tester", ["two"])
    await until(() => core.sent.length > 0)
    const third = teammates.post("tester", ["three"])
    expect(await Promise.all([first, second, third])).toEqual([true, true, true])
    expect(core.sent.map(({texts}) => texts[0])).toEqual(["one", "two", "three"])
  })

  it("leave the group as the teammate at its end, delete its profile as the core's own user and its contact, keep it known as a member, and refuse its later messages", async () => {
    const {host, core, teammates} = setup()
    teammates.start("reviewer", "Review")
    expect(await teammates.post("reviewer", ["Done."])).toBe(true)
    await teammates.end("reviewer")
    expect(core.calls.slice(-2)).toEqual(["leave 350 as 2", "delete 2 as 1 with queues"])
    expect(host.removed).toEqual([])
    expect(host.deleted).toEqual([{chatType: T.ChatType.Direct, chatId: 50, mode: {type: "full", notify: false}}])
    expect(host.commands.filter(command => command.startsWith("/_delete"))).toEqual([])
    expect(teammates.isMember(member(2050, "reviewer"))).toBe(true)
    expect(await teammates.post("reviewer", ["Late."])).toBe(false)
    expect(teammates.start("reviewer")).toBe("started")
  })

  it("remove a teammate as Claude when Claude does not see it leave", async () => {
    const {host, core, teammates} = setup()
    host.leaves = false
    teammates.start("reviewer")
    expect(await teammates.post("reviewer", ["Done."])).toBe(true)
    await teammates.end("reviewer")
    expect(host.removed).toEqual([{groupId: hostGroup, memberIds: [2050]}])
    expect(core.calls.at(-1)).toBe("delete 2 as 1 with queues")
  })

  it("keep posting a teammate's messages until the delay of its end, then leave", async () => {
    const {core, teammates} = setup()
    teammates.start("reviewer")
    await teammates.end("reviewer", 50)
    expect(await teammates.post("reviewer", ["The report."])).toBe(true)
    await until(() => core.calls.includes("delete 2 as 1 with queues"))
    expect(await teammates.post("reviewer", ["Late."])).toBe(false)
  })

  it("refuse the messages of a teammate whose join failed, waiting and later ones, and delete its profile and contact", async () => {
    const {host, core, teammates} = setup()
    host.failAdd = true
    teammates.start("reviewer")
    expect(await teammates.post("reviewer", ["Done."])).toBe(false)
    expect(await teammates.post("reviewer", ["Again."])).toBe(false)
    await until(() => core.calls.includes("delete 2 as 1 with queues"))
    expect(core.calls).not.toContain("leave 350 as 2")
    expect(host.deleted).toEqual([{chatType: T.ChatType.Direct, chatId: 50, mode: {type: "full", notify: false}}])
    expect(core.sent).toEqual([])
  })

  it("refuse the messages of a teammate whose core failed to open, and open the core for a later teammate", async () => {
    const host = new FakeHost()
    const core = new FakeCore(host)
    let opens = 0
    const teammates = new Teammates(join(root, "agents"), {chat: host.api(), userId: user.userId, groupId: hostGroup, session}, async () => {
      if (++opens === 1) throw new Error("no database")
      return {chat: core as never, user: {userId: 1} as T.User, events: async handler => void (core.handler = handler)}
    })
    teammates.start("reviewer")
    expect(await teammates.post("reviewer", ["Done."])).toBe(false)
    teammates.start("tester")
    expect(await teammates.post("tester", ["Tests pass."])).toBe(true)
    expect(opens).toBe(2)
  })

  it("stop the joins that wait at close, refuse their messages, delete their profiles and Claude's pending invitations", async () => {
    const {host, core, teammates} = setup()
    core.connects = false
    teammates.start("reviewer")
    const waiting = teammates.post("reviewer", ["Done."])
    await until(() => core.calls.includes("connect 2 as 2"))
    await teammates.close()
    expect(await waiting).toBe(false)
    expect(core.calls.slice(-2)).toEqual(["delete 2 as 1 with queues", "close"])
    expect(host.commands.filter(command => command.startsWith("/_delete"))).toEqual(["/_delete :900"])
    expect(host.deleted).toEqual([])
  })

  it("share one close between overlapping calls, so the second waits for the first, and remove the folder after the core closes", async () => {
    const {core, teammates} = setup()
    teammates.start("reviewer")
    expect(await teammates.post("reviewer", ["Done."])).toBe(true)
    mkdirSync(join(root, "agents"), {recursive: true})
    writeFileSync(join(root, "agents", "simplex_chat.db"), "")
    const first = teammates.close()
    const second = teammates.close()
    expect(second).toBe(first)
    await second
    expect(core.calls.filter(call => call === "close" || call.startsWith("delete"))).toEqual(["delete 2 as 1 with queues", "close"])
    expect(existsSync(join(root, "agents"))).toBe(false)
    await teammates.close()
    expect(core.calls.filter(call => call === "close")).toHaveLength(1)
  })

  it("join a teammate started during a close after that close, in a new core", async () => {
    const {core, teammates, opened} = setup()
    teammates.start("reviewer")
    expect(await teammates.post("reviewer", ["Done."])).toBe(true)
    const closing = teammates.close()
    teammates.start("tester")
    const posted = teammates.post("tester", ["Tests pass."])
    await closing
    expect(await posted).toBe(true)
    expect(opened).toHaveLength(2)
    expect(core.calls.indexOf("close")).toBeLessThan(core.calls.indexOf("create 3"))
  })

  it(`start ${maxTeammates} teammates at most at once`, async () => {
    const {teammates} = setup()
    for (let i = 1; i <= maxTeammates; i++) expect(teammates.start(`teammate ${i}`)).toBe("started")
    expect(teammates.start("one more")).toBe("full")
    await teammates.end("teammate 1")
    expect(teammates.start("one more")).toBe("started")
    await teammates.close()
  })

  it("end every teammate and close the core at close, and open a new core for a later teammate", async () => {
    const {core, teammates, opened} = setup()
    teammates.start("reviewer")
    teammates.start("tester")
    expect(await teammates.post("tester", ["Tests pass."])).toBe(true)
    await teammates.close()
    expect(core.calls.filter(call => call.startsWith("delete"))).toEqual(["delete 2 as 1 with queues", "delete 3 as 1 with queues"])
    expect(core.calls.at(-1)).toBe("close")
    expect(await teammates.post("reviewer", ["Late."])).toBe(false)
    teammates.start("reviewer")
    expect(await teammates.post("reviewer", ["Back."])).toBe(true)
    expect(opened).toHaveLength(2)
  })
})

describe("removeStale", () => {
  it("remove the teammates left by an earlier service from their sessions' groups and delete their contacts", async () => {
    const host = new FakeHost()
    const marked = (contactId: number, data: object | undefined) => host.chats.set(`@${contactId}`, {chatInfo: direct({...contact(contactId, `contact ${contactId}`), customData: data} as T.Contact), items: []})
    marked(5, {simplexMcpTeammate: {session: 1, name: "reviewer"}})
    marked(6, {simplexMcpTeammate: {session: 1, name: "tester"}})
    marked(7, {simplexMcpTeammate: {session: 9, name: "lost"}})
    marked(8, {simplexMcpUser: true})
    host.members.set(20, [
      {...member(105, "reviewer"), memberContactId: 5},
      {...member(106, "tester", T.GroupMemberRole.Member, T.GroupMemberStatus.Left), memberContactId: 6},
      {...member(108, "User"), memberContactId: 8},
    ])
    await removeStale(host.api(), user.userId, number => (number === 1 ? 20 : undefined))
    expect(host.removed).toEqual([{groupId: 20, memberIds: [105]}])
    expect(host.deleted.map(({chatId}) => chatId)).toEqual([5, 6, 7])
  })
})
