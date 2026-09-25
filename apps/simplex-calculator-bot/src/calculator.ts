export type Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
export type Operator = "+" | "-" | "×" | "÷"
export type Key = Digit | Operator | "C" | "±" | "%" | "√" | "." | "="
export type Update = (calc: Calc) => [Calc, string?]

export interface Calc {
  display: string
  operand: string
  terms: string[]
  pending?: {acc: number, op: Operator}
  mode: "typing" | "result" | "operator"
}

export const initialCalc: Calc = {display: "0", operand: "0", terms: [], mode: "typing"}

export const keypad: Key[][] = [
  ["C", "±", "%", "÷"],
  ["7", "8", "9", "×"],
  ["4", "5", "6", "-"],
  ["1", "2", "3", "+"],
  ["√", "0", ".", "="],
]

const keyWords: Partial<Record<Key, string>> = {
  "±": "neg",
  "%": "pct",
  "÷": "div",
  "×": "mul",
  "-": "sub",
  "+": "add",
  "√": "sqrt",
  ".": "dot",
  "=": "eq",
}

export function keyWord(key: Key): string {
  return keyWords[key] ?? key
}

const keyAliases: Record<string, Key> = {"c": "C", "*": "×", "x": "×", "/": "÷", "−": "-"}

export const keyNames = new Map<string, Key>([
  ...keypad.flat().flatMap((key): [string, Key][] => [[key, key], [keyWord(key), key]]),
  ...Object.entries(keyAliases),
])

const maxDigits = 15
const errorDisplay = "Error"

export function press(calc: Calc, key: Key): [Calc, string?] {
  switch (key) {
    case "C": return [calc.display === "0" || calc.display === errorDisplay ? initialCalc : {...calc, display: "0", operand: "0", mode: "typing"}]
    case "=": return equals(calc)
    case "+": case "-": case "×": case "÷": return [operator(calc, key)]
    case "%": return [result(calc, percent(calc), `${calc.operand}%`)]
    case "√": return [result(calc, Math.sqrt(value(calc)), `√${calc.operand}`)]
    case "±": return [negate(calc)]
    default: return [enter(calc, key)]
  }
}

function enter(calc: Calc, key: Digit | "."): Calc {
  const display = calc.mode === "typing" ? append(calc.display, key) : key === "." ? "0." : key
  return {...calc, display, operand: display, mode: "typing"}
}

function append(display: string, key: Digit | "."): string {
  if (key === ".") return display.includes(".") ? display : `${display}.`
  if (display.replace(/\D/g, "").length >= maxDigits) return display
  return display.replace(/^(-?)0$/, "$1") + key
}

function negate(calc: Calc): Calc {
  if (calc.mode !== "typing") return enterNumber(calc, -value(calc))
  const display = calc.display.startsWith("-") ? calc.display.slice(1) : `-${calc.display}`
  return {...calc, display, operand: display}
}

function percent(calc: Calc): number {
  const {pending} = calc
  const b = value(calc)
  return pending && (pending.op === "+" || pending.op === "-") ? pending.acc * b / 100 : b / 100
}

function enterNumber(calc: Calc, n: number): Calc {
  return result(calc, n, format(n))
}

function result(calc: Calc, n: number, operand: string): Calc {
  return {...calc, display: format(n), operand, mode: "result"}
}

function operator(calc: Calc, op: Operator): Calc {
  if (calc.mode === "operator" && calc.pending) {
    return {...calc, pending: {...calc.pending, op}, terms: [...calc.terms.slice(0, -1), op]}
  }
  const display = format(calc.pending ? apply(calc.pending, value(calc)) : value(calc))
  return {display, operand: display, terms: [...calc.terms, calc.operand, op], pending: {acc: Number(display), op}, mode: "operator"}
}

function equals(calc: Calc): [Calc, string?] {
  if (!calc.pending) return [calc]
  const display = format(apply(calc.pending, value(calc)))
  const logLine = [...calc.terms, calc.operand, "=", display].join(" ")
  return [{display, operand: display, terms: [], mode: "result"}, logLine]
}

function apply({acc, op}: {acc: number, op: Operator}, b: number): number {
  switch (op) {
    case "+": return acc + b
    case "-": return acc - b
    case "×": return acc * b
    case "÷": return acc / b
  }
}

function value(calc: Calc): number {
  return Number(calc.display)
}

function format(n: number): string {
  const rounded = Number(n.toPrecision(maxDigits))
  if (!Number.isFinite(rounded) || Math.abs(rounded) >= 10 ** maxDigits) return errorDisplay
  const integerDigits = Math.trunc(Math.abs(rounded)).toString().length
  const fixed = rounded.toFixed(maxDigits - integerDigits)
  const trimmed = fixed.includes(".") ? fixed.replace(/\.?0+$/, "") : fixed
  return trimmed === "-0" ? "0" : trimmed
}

export function textInput(text: string): Update | undefined {
  const input = text.replace(/\s/g, "")
  const n = expressionValue(input.replace(/=$/, ""))
  if (n !== undefined) {
    return calc => {
      const entered = enterNumber(calc, n)
      return input.endsWith("=") ? press(entered, "=") : [entered]
    }
  }
  const key = keyNames.get(input.toLowerCase())
  return key ? calc => press(calc, key) : undefined
}

const termPattern = /([-+−×x*÷\/]?)([-−]?)(\d+(?:\.\d*)?|\.\d+)(%?)/g

function expressionValue(expression: string): number | undefined {
  const terms = [...expression.matchAll(termPattern)]
  const valid = terms.length > 0
    && terms.map(([term]) => term).join("") === expression
    && terms.every(([, op], i) => i === 0 || op !== "")
  if (!valid) return undefined
  const calc = terms.reduce((current, [, op, sign, number, percentSign]) => {
    const withOperator = op ? press(current, keyAliases[op] ?? op as Key)[0] : current
    const entered = enterNumber(withOperator, sign ? -Number(number) : Number(number))
    return percentSign ? press(entered, "%")[0] : entered
  }, initialCalc)
  return value(press(calc, "=")[0])
}

const nbsp = " "
const wordWidth = Math.max(...keypad.flat().map(key => keyWord(key).length))

export function calculatorText(calc: Calc | undefined, symbolKeys: boolean): string {
  const rows = keypad.map(row => symbolKeys ? row.map(key => `/${key}`).join("   ") : row.map(wordKey).join(""))
  return [`*${calc?.display ?? nbsp}*`, ...rows].join("\n")
}

function wordKey(key: Key): string {
  const word = keyWord(key)
  return `/${word} \`${nbsp.repeat(wordWidth + 1 - word.length)}\``
}
