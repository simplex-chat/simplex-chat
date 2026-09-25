import {spawnSync} from "child_process"
import * as path from "path"

jest.mock("../src/simplex", () => ({load: jest.fn()}))
jest.mock("../src/libs", () => ({libPath: jest.requireActual("../src/libs").libPath, resolveLibsDir: jest.fn()}))

const PACKAGE_DIR = path.join(__dirname, "..")
const ADDON = path.join(PACKAGE_DIR, "build", "Release", "simplex.node")
const CHILD_TIMEOUT_MS = 10000
const LIBS_DIR = path.resolve("libs")
const SWITCH_ERROR = "libsimplex already loaded with backend=sqlite; cannot switch to postgres in the same process"
const {libPath} = jest.requireActual<typeof import("../src/libs")>("../src/libs")

function runAddon(script: string): string {
  const r = spawnSync(process.execPath, ["-e", `const simplex = require(${JSON.stringify(ADDON)});\n${script}`], {encoding: "utf8", timeout: CHILD_TIMEOUT_MS})
  return `${r.stdout}${r.stderr}status=${r.status} signal=${r.signal}`
}

function freshCore(): {core: typeof import("../src/core"), resolveLibsDir: jest.Mock, load: jest.Mock} {
  let modules!: ReturnType<typeof freshCore>
  jest.isolateModules(() => {
    modules = {
      core: require("../src/core"),
      resolveLibsDir: require("../src/libs").resolveLibsDir,
      load: require("../src/simplex").load,
    }
  })
  return modules
}

describe("native load", () => {
  it("requires load before every FFI call", () => {
    const out = runAddon(`
      for (const name of Object.keys(simplex).filter(n => n !== "load").sort()) {
        try { simplex[name](); console.log(name + ": no error") } catch (e) { console.log(name + ": " + e.message) }
      }`)
    const bindings = ["chat_close_store", "chat_decrypt_file", "chat_encrypt_file", "chat_migrate_init", "chat_migrate_init_queue",
      "chat_read_file", "chat_recv_msg_wait", "chat_send_cmd", "chat_write_file"]
    const expected = bindings.map(name => `${name}: libsimplex is not loaded, call core.loadLibrary(backend) first\n`).join("")
    expect(out).toBe(`${expected}status=0 signal=null`)
  })

  it("reports the path of a library that cannot be opened", () => {
    const out = runAddon(`try { simplex.load("/nonexistent/libsimplex.so") } catch (e) { console.log(e.message) }`)
    expect(out).toContain("cannot load /nonexistent/libsimplex.so")
  })

  it("rejects a non-string library path", () => {
    const out = runAddon(`try { simplex.load(1) } catch (e) { console.log(e.name + ": " + e.message) }`)
    expect(out).toContain("TypeError: Expected string (libPath)")
  })

  it("rejects a library without the chat exports", () => {
    const out = runAddon(`try { simplex.load(${JSON.stringify(ADDON)}) } catch (e) { console.log(e.message) }`)
    expect(out).toContain(`${ADDON} does not export hs_init_with_rtsopts`)
  })
})

describe("loadLibrary", () => {
  it("shares one load between concurrent calls", async () => {
    const {core, resolveLibsDir, load} = freshCore()
    resolveLibsDir.mockResolvedValue(LIBS_DIR)
    const first = core.loadLibrary("sqlite")
    const second = core.loadLibrary("sqlite")
    expect(second).toBe(first)
    await Promise.all([first, second])
    expect(resolveLibsDir).toHaveBeenCalledTimes(1)
    expect(load.mock.calls).toEqual([[libPath(LIBS_DIR)]])
  })

  it("refuses to switch backend while the first load is in progress", async () => {
    const {core, resolveLibsDir} = freshCore()
    resolveLibsDir.mockResolvedValue(LIBS_DIR)
    const first = core.loadLibrary("sqlite")
    await expect(core.loadLibrary("postgres")).rejects.toThrow(SWITCH_ERROR)
    await first
  })

  it("keeps a completed load for the process", async () => {
    const {core, resolveLibsDir, load} = freshCore()
    resolveLibsDir.mockResolvedValue(LIBS_DIR)
    await core.loadLibrary("sqlite")
    await core.loadLibrary("sqlite")
    await expect(core.loadLibrary("postgres")).rejects.toThrow(SWITCH_ERROR)
    expect(resolveLibsDir).toHaveBeenCalledTimes(1)
    expect(load).toHaveBeenCalledTimes(1)
  })

  it("retries after a failed load", async () => {
    const {core, resolveLibsDir, load} = freshCore()
    resolveLibsDir.mockRejectedValueOnce(new Error("HTTP 503")).mockResolvedValue(LIBS_DIR)
    await expect(core.loadLibrary("sqlite")).rejects.toThrow("HTTP 503")
    await expect(core.loadLibrary("postgres")).resolves.toBeUndefined()
    expect(resolveLibsDir.mock.calls).toEqual([["sqlite"], ["postgres"]])
    expect(load).toHaveBeenCalledTimes(1)
  })
})
