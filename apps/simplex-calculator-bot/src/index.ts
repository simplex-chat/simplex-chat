import {mkdirSync} from "fs"
import {runCalculatorBot} from "./calculatorBot.js"

mkdirSync("data", {recursive: true})

runCalculatorBot({type: "sqlite", filePrefix: "./data/calculator"}).catch(e => {
  console.log("fatal error", e)
  process.exit(1)
})
