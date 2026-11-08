const {copyFileSync, existsSync, readFileSync, renameSync, writeFileSync} = require("fs")

const files = ["package.json", "npm-shrinkwrap.json"]

function restore() {
  for (const file of files) if (existsSync(`${file}.orig`)) renameSync(`${file}.orig`, file)
}

function strip() {
  restore()
  for (const file of files) copyFileSync(file, `${file}.orig`)
  const manifest = JSON.parse(readFileSync("package.json", "utf8"))
  delete manifest.devDependencies
  writeFileSync("package.json", `${JSON.stringify(manifest, null, 2)}\n`)
  const lock = JSON.parse(readFileSync("npm-shrinkwrap.json", "utf8"))
  lock.packages = Object.fromEntries(Object.entries(lock.packages).filter(([, entry]) => !entry.dev))
  delete lock.packages[""].devDependencies
  writeFileSync("npm-shrinkwrap.json", `${JSON.stringify(lock, null, 2)}\n`)
}

if (process.argv[2] === "--restore") restore()
else strip()
