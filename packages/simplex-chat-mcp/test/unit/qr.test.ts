import * as qrcode from "qrcode"
import {qrCodeText} from "../../src/qr"

describe("QR codes", () => {
  const link = "https://smp4.simplex.im/g#1ai9d8WS140_IxvPDYWLigJNZU-z7GsDUhp0JoJ7kbU"
  const cells = (line: string): string[] => [...line.matchAll(/\x1b\[[0-9;]+m[^\x1b]+/g)].map(([cell]) => cell)
  const visible = (line: string): string => line.replace(/\x1b\[[0-9;]*m/g, "")

  it("draws two module rows per line with lower half blocks in ansi", () => {
    const lines = qrCodeText(link, "ansi").split("\n")
    expect(lines).toHaveLength(19)
    for (const line of lines) {
      expect(visible(line)).toHaveLength(37)
      expect(line.endsWith("\x1b[0m")).toBe(true)
      expect([...cells(line).slice(0, 2), ...cells(line).slice(-2)]).toEqual(Array(4).fill("\x1b[47m "))
    }
    for (const line of [lines[0], lines.at(-1)!]) expect(cells(line)).toEqual(Array(37).fill("\x1b[47m "))
    expect(cells(lines[1]).slice(2, 4)).toEqual(["\x1b[40m ", "\x1b[37;40m▄"])
    const {modules} = qrcode.create(link, {errorCorrectionLevel: "L"})
    const row = Array.from({length: 16}, (_, i) => 2 * i).find(r => Array.from({length: modules.size}, (_, c) => c).some(c => !modules.get(r, c) && modules.get(r + 1, c)))!
    const col = Array.from({length: modules.size}, (_, c) => c).find(c => !modules.get(row, c) && modules.get(row + 1, c))!
    expect(cells(lines[row / 2 + 1])[col + 2]).toBe("\x1b[30;47m▄")
  })

  it("draws light modules filled with plain half blocks, two module rows per line", () => {
    const link = "https://smp4.simplex.im/g#test"
    const {modules} = qrcode.create(link, {errorCorrectionLevel: "L"})
    const lines = qrCodeText(link, "plain").split("\n")
    const width = modules.size + 4
    expect(lines).toHaveLength(Math.ceil(width / 2))
    for (const line of lines) expect([...line]).toHaveLength(width)
    expect(qrCodeText(link, "plain")).toMatch(/^[█▀▄ \n]+$/)
    expect(lines[0]).toBe("█".repeat(width))
    const light = (row: number, col: number): boolean => row < 0 || col < 0 || row >= modules.size || col >= modules.size || modules.get(row, col) === 0
    const expected = [...lines[1]].map((_, i) => {
      const top = light(0, i - 2)
      const bottom = light(1, i - 2)
      return top && bottom ? "█" : top ? "▀" : bottom ? "▄" : " "
    })
    expect([...lines[1]]).toEqual(expected)
  })
})
