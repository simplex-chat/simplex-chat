import {test, expect} from "vitest"
import {mkdtempSync, rmSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {CEvt, T} from "@simplex-chat/types"
import {api, util} from "simplex-chat"
import {runCalculatorBot} from "./src/calculatorBot.js"

const isCalculator = (display: string) => (ci: T.AChatItem) => ci.chatItem.meta.itemText.startsWith(`*${display}*\n`)

const calculatorShows = (display: string) => ({chatItems}: CEvt.NewChatItems) => chatItems.some(isCalculator(display))

const hasText = (text: string) => ({chatItems}: CEvt.NewChatItems) =>
  chatItems.some(ci => ci.chatItem.meta.itemText === text)

test("calculator in business chat (uses preset servers)", async () => {
  const dir = mkdtempSync(join(tmpdir(), "calculator-bot-"))
  const [calculator, _botUser, address] = await runCalculatorBot({type: "sqlite", filePrefix: join(dir, "bot")})
  const alice = await api.ChatApi.init({type: "sqlite", filePrefix: join(dir, "alice")})
  const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
  await alice.startChat()
  try {
    const [_plan, link] = await alice.apiConnectPlan(aliceUser.userId, util.contactAddressStr(address!.connLinkContact))
    const firstCalculator = alice.wait("newChatItems", calculatorShows("0"), 30000)
    await alice.apiConnect(aliceUser.userId, false, link)
    const calculatorItem = (await firstCalculator)?.chatItems.find(isCalculator("0"))
    expect(calculatorItem?.chatInfo.type).toBe(T.ChatType.Group)
    const groupId = (calculatorItem!.chatInfo as T.ChatInfo.Group).groupInfo.groupId

    const logLine = alice.wait("newChatItems", hasText("2 + 2 = 4"), 30000)
    const four = alice.wait("newChatItems", calculatorShows("4"), 30000)
    for (const key of ["/2", "/+", "/2", "/="]) await alice.apiSendTextMessage([T.ChatType.Group, groupId], key)
    expect(await logLine).toBeDefined()
    expect(await four).toBeDefined()

    const forty = alice.wait("newChatItems", calculatorShows("40"), 30000)
    await alice.apiSendTextMessage([T.ChatType.Group, groupId], "12 × 3 + 4")
    expect(await forty).toBeDefined()
  } finally {
    await alice.close()
    await calculator.close()
    rmSync(dir, {recursive: true, force: true})
  }
}, 120000)
