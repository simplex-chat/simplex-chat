import * as qrcode from "qrcode"

export type QrStyle = "small" | "large" | "plain"

const qrMargin = 2
const darkCell = "\x1b[40m  "
const lightCell = "\x1b[47m  "
const darkHalfCell = "\x1b[40m "
const lightHalfCell = "\x1b[47m "
const darkTopCell = "\x1b[37;40m▄"
const darkBottomCell = "\x1b[30;47m▄"
const resetColors = "\x1b[0m"

// spec: spec/architecture.md#qr-qrcodetext
export function qrCodeText(link: string, style: QrStyle): string {
  const {modules} = qrcode.create(link, {errorCorrectionLevel: "L"})
  const positions = Array.from({length: modules.size + 2 * qrMargin}, (_, i) => i - qrMargin)
  const inside = (i: number): boolean => i >= 0 && i < modules.size
  const dark = (row: number, col: number): boolean => inside(row) && inside(col) && modules.get(row, col) !== 0
  if (style === "large") return positions.map(row => positions.map(col => (dark(row, col) ? darkCell : lightCell)).join("") + resetColors).join("\n")
  const cell = style === "small" ? ansiCell : plainCell
  const lines = positions.filter((_, i) => i % 2 === 0).map(row => positions.map(col => cell(dark(row, col), dark(row + 1, col))).join(""))
  return lines.map(line => (style === "small" ? line + resetColors : line)).join("\n")
}

function ansiCell(top: boolean, bottom: boolean): string {
  if (top === bottom) return top ? darkHalfCell : lightHalfCell
  return top ? darkTopCell : darkBottomCell
}

function plainCell(topDark: boolean, bottomDark: boolean): string {
  if (topDark && bottomDark) return " "
  if (topDark) return "▄"
  return bottomDark ? "▀" : "█"
}
