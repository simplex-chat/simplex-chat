type Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
type Operator = "+" | "-" | "×" | "÷"
export type Key = Digit | Operator | "C" | "±" | "%" | "√" | "." | "="
export type Update = (calc: Calc) => [Calc, string?]

export interface Calc {
  display: string
  operand: string
  terms: Term[]
  mode: "typing" | "result" | "operator"
}

interface Operand {
  text: string
  value: number
}

interface Term extends Operand {
  op: Operator
}

export const initialCalc: Calc = {display: "0", operand: "0", terms: [], mode: "typing"}

const keypad: Key[][] = [
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

function keyWord(key: Key): string {
  return keyWords[key] ?? key
}

const operatorNames = new Map<string, Operator>([
  ["+", "+"], ["-", "-"], ["−", "-"], ["×", "×"], ["x", "×"], ["*", "×"], ["÷", "÷"], ["/", "÷"],
])

export const keyNames = new Map<string, Key>([
  ...keypad.flat().flatMap((key): [string, Key][] => [[key, key], [keyWord(key), key]]),
  ...operatorNames,
  ["c", "C"],
])

const maxDigits = 15
const errorDisplay = "Error"

export function press(calc: Calc, key: Key): [Calc, string?] {
  switch (key) {
    case "C": return [clear(calc)]
    case "=": return equals(calc)
    case "+": case "-": case "×": case "÷": return operator(calc, key)
    case "%": return [showNumber(calc, percent(calc), `${calc.operand}%`)]
    case "√": return [showNumber(calc, Math.sqrt(value(calc)), `√${calc.operand}`)]
    case "±": return [negate(calc)]
    default: return [enter(calc, key)]
  }
}

function clear(calc: Calc): Calc {
  if (calc.display === "0" || calc.display === errorDisplay) return initialCalc
  return {...calc, display: "0", operand: "0", mode: "typing"}
}

function enter(calc: Calc, key: Digit | "."): Calc {
  const display = append(calc.mode === "typing" ? calc.display : "0", key)
  return {...calc, display, operand: display, mode: "typing"}
}

function append(display: string, key: Digit | "."): string {
  if (key === ".") return display.includes(".") ? display : `${display}.`
  if (display.replace(/\D/g, "").length >= maxDigits) return display
  return display.replace(/^(-?)0$/, "$1") + key
}

function negate(calc: Calc): Calc {
  if (calc.mode !== "typing") return showNumber(calc, -value(calc))
  const display = calc.display.startsWith("-") ? calc.display.slice(1) : `-${calc.display}`
  return {...calc, display, operand: display}
}

function percent(calc: Calc): number {
  const last = calc.terms.at(-1)
  const share = value(calc) / 100
  return last && isAdditive(last.op) ? last.value * share : share
}

function showNumber(calc: Calc, n: number, operand?: string): Calc {
  const display = format(n)
  return {...calc, display, operand: operand ?? display, mode: "result"}
}

function operator(calc: Calc, op: Operator): [Calc, string?] {
  const terms = calc.mode === "operator" ? calc.terms.slice(0, -1) : calc.terms
  const reduced = reduce(terms, {text: calc.operand, value: value(calc)}, precedence(op))
  const display = format(reduced.right.value)
  const logLine = reduced.lines.length > 0 ? reduced.lines.join("\n") : undefined
  return [{display, operand: display, terms: [...reduced.terms, {...reduced.right, op}], mode: "operator"}, logLine]
}

function equals(calc: Calc): [Calc, string?] {
  if (calc.terms.length === 0) return [calc]
  const {right, lines} = reduce(calc.terms, {text: calc.operand, value: value(calc)}, 0)
  return [{display: right.text, operand: right.text, terms: [], mode: "result"}, lines.join("\n")]
}

function reduce(terms: Term[], right: Operand, minPrecedence: number): {terms: Term[], right: Operand, lines: string[]} {
  const left = terms.at(-1)
  if (!left || precedence(left.op) < minPrecedence) return {terms, right, lines: []}
  const text = format(apply(left.value, left.op, right.value))
  const reduced = reduce(terms.slice(0, -1), {text, value: Number(text)}, minPrecedence)
  return {...reduced, lines: [`${left.text} ${left.op} ${right.text} = ${text}`, ...reduced.lines]}
}

function precedence(op: Operator): number {
  return isAdditive(op) ? 1 : 2
}

function isAdditive(op: Operator): boolean {
  return op === "+" || op === "-"
}

function apply(a: number, op: Operator, b: number): number {
  switch (op) {
    case "+": return a + b
    case "-": return a - b
    case "×": return a * b
    case "÷": return a / b
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

export function textInput(text: string): {update: Update, result?: string} | undefined {
  const input = text.replace(/\s/g, "")
  const entry = input.replace(/=$/, "")
  const n = expressionValue(entry)
  if (n !== undefined) {
    const update: Update = calc => {
      const entered = showNumber(calc, n)
      return input.endsWith("=") ? press(entered, "=") : [entered]
    }
    return {update, result: Number.isNaN(Number(entry)) ? format(n) : undefined}
  }
  const key = keyNames.get(input.toLowerCase())
  return key ? {update: calc => press(calc, key)} : undefined
}

const tokenPattern = /\d+(?:\.\d*)?|\.\d+|[-+−×x*÷\/%()]/g

interface Parsed {
  value: number
  rest: string[]
}

function expressionValue(text: string): number | undefined {
  const tokens = text.match(tokenPattern) ?? []
  const parsed = tokens.join("") === text ? expression(tokens, initialCalc) : undefined
  return parsed?.rest.length === 0 ? parsed.value : undefined
}

function expression(tokens: string[], calc: Calc): Parsed | undefined {
  const operand = term(tokens)
  if (!operand) return undefined
  const percent = operand.rest[0] === "%"
  const shown = showNumber(calc, operand.value)
  const entered = percent ? press(shown, "%")[0] : shown
  const rest = percent ? operand.rest.slice(1) : operand.rest
  const op = operatorNames.get(rest[0])
  return op ? expression(rest.slice(1), press(entered, op)[0]) : {value: value(press(entered, "=")[0]), rest}
}

function term([first, ...rest]: string[]): Parsed | undefined {
  if (first === "(") {
    const inner = expression(rest, initialCalc)
    return inner?.rest[0] === ")" ? {value: inner.value, rest: inner.rest.slice(1)} : undefined
  }
  if (operatorNames.get(first) === "-") {
    const operand = term(rest)
    return operand && {value: -operand.value, rest: operand.rest}
  }
  const n = Number(first)
  return Number.isNaN(n) ? undefined : {value: n, rest}
}

const nbsp = String.fromCharCode(0xa0)
const wordWidth = Math.max(...keypad.flat().map(key => keyWord(key).length))

export function calculatorText(calc: Calc | undefined, symbolKeys: boolean): string {
  const rows = keypad.map(row => symbolKeys ? row.map(key => `/${key}`).join("   ") : row.map(wordKey).join(""))
  return [`*${calc?.display ?? nbsp}*`, ...rows].join("\n")
}

function wordKey(key: Key): string {
  const word = keyWord(key)
  return `/${word} \`${nbsp.repeat(wordWidth + 1 - word.length)}\``
}
