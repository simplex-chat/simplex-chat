import {T} from "@simplex-chat/types"
import {core} from "simplex-chat"
import {deliveries} from "../../src/core"
import {FakeChat} from "./fixtures"

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(() => jest.restoreAllMocks())

describe("delivery wait at the stop", () => {
  it("logs an error of the SQL query and ends", async () => {
    const errors: string[] = []
    jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text).trimEnd()), true))
    const chat = new FakeChat()
    chat.sendChatCmd = async () => {
      throw new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type: "dBException", message: "no such table: connections"}} as unknown as T.ChatError)
    }
    await deliveries(chat.api(), 10)
    expect(errors).toEqual(['delivery wait: SimpleX error: errorStore: dBException: {"message":"no such table: connections"}'])
  })
})
