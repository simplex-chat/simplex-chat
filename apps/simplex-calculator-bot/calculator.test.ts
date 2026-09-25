import {describe, test, expect} from "vitest"
import {Calc, Key, calculatorText, initialCalc, press, textInput} from "./src/calculator.js"

type Input = {tap: string} | {text: string}

function run(inputs: Input[]): {calc: Calc, logLines: string[]} {
  const logLines: string[] = []
  const calc = inputs.reduce((current, input) => {
    const [next, logLine] = "tap" in input ? press(current, input.tap as Key) : textInput(input.text)!(current)
    if (logLine) logLines.push(logLine)
    return next
  }, initialCalc)
  return {calc, logLines}
}

const tap = (keys: string) => run(keys.split(" ").map(key => ({tap: key})))
const type = (...texts: string[]) => run(texts.map(text => ({text})))
const display = (keys: string) => tap(keys).calc.display

describe("keys", () => {
  test("enter numbers", () => {
    expect(display("1 2 . 5")).toBe("12.5")
    expect(display("0 0 7")).toBe("7")
    expect(display(". 5")).toBe("0.5")
    expect(display("1 . . 5")).toBe("1.5")
  })

  test("compute left to right and log after equals", () => {
    expect(tap("2 + 2 =")).toEqual({calc: expect.objectContaining({display: "4"}), logLines: ["2 + 2 = 4"]})
    expect(tap("2 + 3 × 4 =").logLines).toEqual(["2 + 3 × 4 = 20"])
    expect(display("2 + 3 ×")).toBe("5")
  })

  test("log only after equals", () => {
    expect(tap("2 + 3 × 4").logLines).toEqual([])
    expect(tap("5 =").logLines).toEqual([])
  })

  test("replace operator", () => {
    expect(tap("2 + × 3 =").logLines).toEqual(["2 × 3 = 6"])
  })

  test("continue from result", () => {
    expect(tap("2 + 2 = + 3 =").logLines).toEqual(["2 + 2 = 4", "4 + 3 = 7"])
    expect(display("2 + 2 = 5")).toBe("5")
  })

  test("percent", () => {
    expect(display("1 0 0 + 1 5 %")).toBe("15")
    expect(tap("1 0 0 + 1 5 % =").logLines).toEqual(["100 + 15% = 115"])
    expect(display("1 0 0 - 1 5 %")).toBe("15")
    expect(display("1 0 0 - 1 5 % =")).toBe("85")
    expect(display("5 0 × 1 0 %")).toBe("0.1")
    expect(display("5 0 × 1 0 % =")).toBe("5")
    expect(display("5 0 ÷ 1 0 %")).toBe("0.1")
    expect(display("5 0 ÷ 1 0 % =")).toBe("500")
    expect(display("1 5 %")).toBe("0.15")
  })

  test("square root and sign", () => {
    expect(display("9 √")).toBe("3")
    expect(tap("1 + 9 √ =").logLines).toEqual(["1 + √9 = 4"])
    expect(display("5 ±")).toBe("-5")
    expect(display("5 ± ±")).toBe("5")
    expect(display("± 5")).toBe("-5")
    expect(display("2 + 2 = ±")).toBe("-4")
  })

  test("clear entry, then clear all", () => {
    expect(tap("2 + 3 C 4 =").logLines).toEqual(["2 + 4 = 6"])
    expect(tap("2 + 3 C C").calc).toEqual(initialCalc)
  })

  test("display without exponent", () => {
    expect(display("0 . 1 + 0 . 2 =")).toBe("0.3")
    expect(display("1 ÷ 3 =")).toBe("0.33333333333333")
    expect(display("1 ÷ 1 0 0 0 0 0 0 0 =")).toBe("0.0000001")
    expect(display("1 ÷ 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 =")).toBe("0.00000000000001")
    expect(display(". 0 0 0 0 0 0 0 1 × . 0 0 0 0 0 0 0 1 =")).toBe("0")
  })

  test("limit digits and overflow", () => {
    expect(display("1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7")).toBe("123456789012345")
    expect(display("9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 + 1 =")).toBe("Error")
  })

  test("errors", () => {
    expect(display("1 ÷ 0 =")).toBe("Error")
    expect(display("2 ± √")).toBe("Error")
    expect(display("1 ÷ 0 = 5")).toBe("5")
    expect(display("1 ÷ 0 + 2 =")).toBe("Error")
    expect(tap("1 ÷ 0 = C").calc).toEqual(initialCalc)
  })
})

describe("typed messages", () => {
  test("numbers and keys", () => {
    expect(type("25", "+", "25", "=")).toEqual({calc: expect.objectContaining({display: "50"}), logLines: ["25 + 25 = 50"]})
    expect(type("25", "add", "25", "eq").logLines).toEqual(["25 + 25 = 50"])
    expect(type("6", "x", "7", "=").logLines).toEqual(["6 × 7 = 42"])
    expect(type("5", "*", "5", "/", "2", "=").logLines).toEqual(["5 × 5 ÷ 2 = 12.5"])
    expect(type("25", "+", "3", "c", "4", "=").logLines).toEqual(["25 + 4 = 29"])
  })

  test("mixed with taps", () => {
    expect(run([{text: "25"}, {tap: "+"}, {text: "25"}, {tap: "="}]).logLines).toEqual(["25 + 25 = 50"])
  })

  test("expressions are entered as a number", () => {
    const result = (text: string) => type(text).calc.display
    expect(result("2 + 2")).toBe("4")
    expect(result("12 × 3 + 4")).toBe("40")
    expect(result("12 * 3 + 4 =")).toBe("40")
    expect(result("8 / 2")).toBe("4")
    expect(result("3x3")).toBe("9")
    expect(result("-5")).toBe("-5")
    expect(result("2 × -3")).toBe("-6")
    expect(result("100 + 15%")).toBe("115")
    expect(result("42")).toBe("42")
    expect(type("10", "×", "2 + 3", "=").logLines).toEqual(["10 × 5 = 50"])
    expect(type("10", "+", "25=").calc.display).toBe("35")
  })

  test("overflow in typed numbers", () => {
    expect(type("1234567890123456").calc.display).toBe("Error")
    expect(type("123456789012345").calc.display).toBe("123456789012345")
  })

  test("reject other text", () => {
    expect(textInput("hello")).toBeUndefined()
    expect(textInput("2 ^ 3")).toBeUndefined()
    expect(textInput("2.3.4")).toBeUndefined()
  })
})

describe("calculator text", () => {
  const nbsp = String.fromCharCode(0xa0)

  test("symbol keys", () => {
    expect(calculatorText(initialCalc, true)).toBe([
      "*0*",
      "/C   /±   /%   /÷",
      "/7   /8   /9   /×",
      "/4   /5   /6   /-",
      "/1   /2   /3   /+",
      "/√   /0   /.   /=",
    ].join("\n"))
  })

  test("switched off: keys without number", () => {
    const [displayLine, ...rows] = calculatorText(undefined, true).split("\n")
    expect(displayLine).toBe(`*${nbsp}*`)
    expect(rows).toEqual(calculatorText(initialCalc, true).split("\n").slice(1))
  })

  test("word keys padded to equal width", () => {
    const pad = (n: number) => `\`${nbsp.repeat(n)}\``
    const [displayLine, firstRow] = calculatorText(initialCalc, false).split("\n")
    expect(displayLine).toBe("*0*")
    expect(firstRow).toBe(`/C ${pad(4)}/neg ${pad(2)}/pct ${pad(2)}/div ${pad(2)}`)
  })
})
