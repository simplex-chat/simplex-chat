import {mkdirSync} from "fs"
import {parseArgs} from "util"
import {runCalculatorBot} from "./calculatorBot.js"

const {values: {domain}} = parseArgs({options: {domain: {type: "string"}}})

mkdirSync("data", {recursive: true})

runCalculatorBot({type: "sqlite", filePrefix: "./data/calculator"}, domain).catch(e => {
  console.log("fatal error", e)
  process.exit(1)
})
