export type Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
export type Operator = "+" | "-" | "×" | "÷"
export type Key = Digit | Operator | "C" | "±" | "%" | "√" | "." | "="

export interface Calc {
  display: string
  operand: string
  terms: string[]
  pending?: {acc: number, op: Operator}
  mode: "typing" | "result" | "operator"
}

export const initialCalc: Calc = {display: "0", operand: "0", terms: [], mode: "result"}

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

const maxDigits = 12
const errorDisplay = "Error"

export function press(calc: Calc, key: Key): [Calc, string?] {
  const current = calc.display === errorDisplay ? initialCalc : calc
  switch (key) {
    case "C": return [initialCalc]
    case "=": return equals(current)
    case "+": case "-": case "×": case "÷": return [operator(current, key)]
    case "%": return [result(current, percent(current), `${current.operand}%`)]
    case "√": return [result(current, Math.sqrt(value(current)), `√${current.operand}`)]
    case "±": return [negate(current)]
    default: return [enter(current, key)]
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
  const display = calc.display.startsWith("-") ? calc.display.slice(1) : `-${calc.display}`
  return {...calc, display, operand: display, mode: calc.mode === "typing" ? "typing" : "result"}
}

function percent(calc: Calc): number {
  const {pending} = calc
  const b = value(calc)
  return pending && (pending.op === "+" || pending.op === "-") ? pending.acc * b / 100 : b / 100
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
  return Number.isFinite(n) ? String(Number(n.toPrecision(maxDigits))) : errorDisplay
}

const typedKeys: Record<string, Key> = {"*": "×", "x": "×", "/": "÷", "−": "-"}

export function textKeys(text: string): Key[] | undefined {
  const expression = text.replace(/\s/g, "").replace(/=$/, "")
  if (!/^[\d.+\-−×x*÷\/%]+$/.test(expression) || !/\d/.test(expression)) return undefined
  return [...expression].map(c => typedKeys[c] ?? c as Key)
}

export function evaluate(keys: Key[]): Calc {
  const allKeys: Key[] = [...keys, "="]
  return allKeys.reduce((calc, key) => press(calc, key)[0], initialCalc)
}

const nbsp = " "
const wordWidth = Math.max(...keypad.flat().map(key => keyWord(key).length))

export function calculatorText(calc: Calc, symbolKeys: boolean): string {
  const rows = keypad.map(row => symbolKeys ? row.map(key => `/${key}`).join("   ") : row.map(wordKey).join(""))
  return [`*${calc.display}*`, ...rows].join("\n")
}

function wordKey(key: Key): string {
  const word = keyWord(key)
  return `/${word} \`${nbsp.repeat(wordWidth + 1 - word.length)}\``
}
