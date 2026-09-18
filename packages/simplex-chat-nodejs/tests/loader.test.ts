import {spawnSync} from "child_process"
import * as core from "../src/core"

function runAddon(script: string): string {
  const r = spawnSync(process.execPath, ["-e", `const simplex = require("./build/Release/simplex.node");\n${script}`], {encoding: "utf8"})
  return r.stdout + r.stderr
}

describe("runtime loader", () => {
  it("requires load before FFI calls", () => {
    const out = runAddon(`try { simplex.chat_send_cmd(1n, "/v") } catch (e) { console.log(e.message) }`)
    expect(out).toContain("libsimplex is not loaded, call core.loadLibrary(backend) first")
  })

  it("reports the path of a library that cannot be opened", () => {
    const out = runAddon(`try { simplex.load("/nonexistent/libsimplex.so") } catch (e) { console.log(e.message) }`)
    expect(out).toContain("cannot load /nonexistent/libsimplex.so")
  })

  it("refuses to switch backend", async () => {
    await core.loadLibrary("sqlite")
    await expect(core.loadLibrary("postgres")).rejects.toThrow("libsimplex already loaded with backend=sqlite; cannot switch to postgres in the same process")
  })
})
