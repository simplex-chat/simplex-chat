import {CC} from "@simplex-chat/types"

describe("APIConnect.cmdString", () => {
  const preparedLink_ = {connFullLink: "L"}

  it("renders incognito=on", () => {
    expect(CC.APIConnect.cmdString({userId: 1, incognito: true, preparedLink_})).toBe("/_connect 1 incognito=on L")
  })

  it("omits incognito when off", () => {
    expect(CC.APIConnect.cmdString({userId: 1, incognito: false, preparedLink_})).toBe("/_connect 1 L")
  })
})
