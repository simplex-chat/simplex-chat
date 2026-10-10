const fs = require("fs")
const path = require("path")

const tag = process.argv[2] || `v${require("../package.json").version}`
const url = `https://api.github.com/repos/simplex-chat/simplex-chat-libs/releases/tags/${tag}`

async function main() {
  const res = await fetch(url, {headers: {"User-Agent": "simplex-chat"}})
  if (!res.ok) throw new Error(`HTTP ${res.status} ${url}`)
  const {assets} = await res.json()
  const hashes = assets
    .map(({name, digest}) => {
      if (!digest?.startsWith("sha256:")) throw new Error(`${name}: digest ${digest}`)
      return `  ${JSON.stringify(name)}: ${JSON.stringify(digest.slice("sha256:".length))},`
    })
    .sort()
  fs.writeFileSync(
    path.join(__dirname, "..", "src", "release.ts"),
    `export const release = ${JSON.stringify(tag)}\n\nexport const sha256: {[asset: string]: string | undefined} = {\n${hashes.join("\n")}\n}\n`
  )
}

main().catch((e) => {
  console.error(e.message)
  process.exit(1)
})
