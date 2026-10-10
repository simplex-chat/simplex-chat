import * as path from "path"

export default async function setup(): Promise<void> {
  process.env.SIMPLEX_ADDON_PATH ??= path.join(__dirname, "..", "build", "Release", "simplex.node")
  const {install} = require("../src/download-libs")
  await install()
}
