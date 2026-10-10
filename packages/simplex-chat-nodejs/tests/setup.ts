import * as path from "path"
import {nativePaths} from "../src/native"

export default async function setup(): Promise<void> {
  process.env.SIMPLEX_ADDON_PATH ??= path.join(__dirname, "..", "build", "Release", "simplex.node")
  await nativePaths("sqlite")
}
