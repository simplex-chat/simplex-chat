import {afterAll, beforeAll, test, expect} from "vitest"
import {mkdtempSync, rmSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {T} from "@simplex-chat/types"
import {api, bot, util} from "simplex-chat"
import {runCalculatorBot} from "./src/calculatorBot.js"
import {SmpServer, startSmpServer} from "./test/smpServer.js"

let smpServer: SmpServer

beforeAll(async () => { smpServer = await startSmpServer() }, 120000)

afterAll(() => smpServer?.stop())

async function useSmpServer(chat: api.ChatApi): Promise<void> {
  expect(await chat.sendChatCmd(`/smp ${smpServer.address}`)).toMatchObject({type: "cmdOk"})
}

async function prepareBotDatabase(dbOpts: bot.BotDbOpts): Promise<void> {
  const chat = await api.ChatApi.init(dbOpts)
  await chat.apiCreateActiveUser({displayName: "SimpleX Calculator", fullName: ""})
  await chat.startChat()
  await useSmpServer(chat)
  await chat.close()
}

type ItemCheck = (ci: T.AChatItem) => boolean

const calculatorShows = (display: string): ItemCheck => ci => ci.chatItem.meta.itemText.startsWith(`*${display}*\n`)

const hasText = (text: string): ItemCheck => ci => ci.chatItem.meta.itemText === text

const repliesTo = (quoted: string, text: string): ItemCheck => ({chatItem}) =>
  chatItem.meta.itemText === text && chatItem.quotedItem?.content.text === quoted

test("calculator in business chat", async () => {
  const dir = mkdtempSync(join(tmpdir(), "calculator-bot-"))
  const botDbOpts: bot.BotDbOpts = {type: "sqlite", filePrefix: join(dir, "bot")}
  await prepareBotDatabase(botDbOpts)
  const [calculator, _botUser, address] = await runCalculatorBot(botDbOpts)
  const alice = await api.ChatApi.init({type: "sqlite", filePrefix: join(dir, "alice")})
  const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
  await alice.startChat()
  await useSmpServer(alice)
  const receives = (check: ItemCheck) => alice.wait("newChatItems", ({chatItems}) => chatItems.some(check), 30000)
  const deletes = (text: string) =>
    alice.wait("chatItemsDeleted", ({chatItemDeletions}) => chatItemDeletions.some(({deletedChatItem}) => hasText(text)(deletedChatItem)), 30000)
  try {
    const [_plan, link] = await alice.apiConnectPlan(aliceUser.userId, util.contactAddressStr(address!.connLinkContact))
    const firstCalculator = receives(calculatorShows("0"))
    await alice.apiConnect(aliceUser.userId, false, link)
    const calculatorItem = (await firstCalculator)?.chatItems.find(calculatorShows("0"))
    expect(calculatorItem?.chatInfo.type).toBe(T.ChatType.Group)
    const groupId = (calculatorItem!.chatInfo as T.ChatInfo.Group).groupInfo.groupId
    const send = async (texts: string[], ...events: Promise<unknown>[]) => {
      for (const text of texts) await alice.apiSendTextMessage([T.ChatType.Group, groupId], text)
      for (const event of events) expect(await event).toBeDefined()
    }
    await send(["/2", "/+", "/2", "/="], receives(hasText("2 + 2 = 4")), receives(calculatorShows("4")), deletes("/+"))
    await send(["2 × (3 + 4) - 1"], receives(repliesTo("2 × (3 + 4) - 1", "13")), receives(calculatorShows("13")))
    await send(
      ["15", "+", "10", "+", "5", "="],
      receives(hasText("15 + 10 = 25")),
      receives(hasText("25 + 5 = 30")),
      receives(calculatorShows("30")),
      deletes("+"),
    )
  } finally {
    await alice.close()
    await calculator.close()
    rmSync(dir, {recursive: true, force: true})
  }
}, 120000)
