import {T} from "@simplex-chat/types"
import {ciBotCommand} from "../src/util"

function rcvText(text: string): T.ChatItem {
  return {content: {type: "rcvMsgContent", msgContent: {type: "text", text}}} as T.ChatItem
}

describe("ciBotCommand", () => {
  it("parses a command at the start of the message", () => {
    expect(ciBotCommand(rcvText("/grok hello"))).toEqual({keyword: "grok", params: "hello"})
  })

  it("returns undefined for a slash in the middle of a word", () => {
    expect(ciBotCommand(rcvText("What follow/read blog posts?"))).toBeUndefined()
  })

  it("returns undefined for a slash after a space", () => {
    expect(ciBotCommand(rcvText("see /home for details"))).toBeUndefined()
  })

  it("strips leading whitespace before matching", () => {
    expect(ciBotCommand(rcvText("   /grok ask this"))).toEqual({keyword: "grok", params: "ask this"})
  })

  it("returns command with empty params when only the keyword is present", () => {
    expect(ciBotCommand(rcvText("/team"))).toEqual({keyword: "team", params: ""})
  })

  it("returns undefined for plain text without slash", () => {
    expect(ciBotCommand(rcvText("hello there"))).toBeUndefined()
  })

  it("returns undefined for non-text chat item content", () => {
    const ci = {content: {type: "rcvDeleted"}} as T.ChatItem
    expect(ciBotCommand(ci)).toBeUndefined()
  })
})
