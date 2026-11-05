#!/usr/bin/env node
const {spawn} = require("child_process")
const {version} = require("../package.json")

const windows = process.platform === "win32"
// npx is npx.cmd on Windows, which only a shell can run
const child = spawn(windows ? "npx.cmd" : "npx", ["-y", `@simplex-chat/mcp@${version}`, ...process.argv.slice(2)], {stdio: "inherit", shell: windows})

for (const signal of ["SIGINT", "SIGTERM"]) process.on(signal, () => child.kill(signal))
child.on("error", error => {
  process.stderr.write(`simplex-mcp: ${error.message}\n`)
  process.exit(1)
})
child.on("exit", (code, signal) => process.exit(code ?? (signal ? 1 : 0)))
