import * as fs from "fs"
import * as http from "http"
import * as https from "https"
import * as os from "os"
import * as path from "path"
import {AddressInfo} from "net"
import {cacheRoot, installLibs, LIBS_VERSION, libsUrl, platformTag, resolveLibsDir} from "../src/libs"
import {main, parseInstallArgs} from "../src/cli"
import {storedZip} from "./zip"

const LIB = "libtest.so"

describe("paths", () => {
  it("uses the Python cache layout", () => {
    expect(cacheRoot("linux", {XDG_CACHE_HOME: "/x"}, "/h")).toBe(path.join("/x", "simplex-chat"))
    expect(cacheRoot("linux", {}, "/h")).toBe(path.join("/h", ".cache", "simplex-chat"))
    expect(cacheRoot("darwin", {}, "/h")).toBe(path.join("/h", "Library", "Caches", "simplex-chat"))
    expect(cacheRoot("win32", {LOCALAPPDATA: "C:\\L"}, "/h")).toBe(path.join("C:\\L", "simplex-chat"))
  })

  it("rejects unsupported platforms", () => {
    expect(platformTag("linux", "x64")).toBe("linux-x86_64")
    expect(() => platformTag("win32", "arm64")).toThrow("Unsupported platform")
  })

  it("resolves an absolute SIMPLEX_LIBS_DIR", async () => {
    await expect(resolveLibsDir("postgres", {SIMPLEX_LIBS_DIR: "/d"})).resolves.toBe(path.resolve("/d"))
  })

  it("returns a relative SIMPLEX_LIBS_DIR as absolute", async () => {
    await expect(resolveLibsDir("sqlite", {SIMPLEX_LIBS_DIR: "rel/libs"})).resolves.toBe(path.join(process.cwd(), "rel", "libs"))
  })

  it("rejects postgres outside linux-x86_64", async () => {
    await expect(resolveLibsDir("postgres", {}, "darwin", "arm64")).rejects.toThrow("postgres backend is only supported on linux-x86_64")
  })
})

describe("installLibs", () => {
  let server: http.Server
  let base: string
  let tmp: string
  const routes: {[p: string]: (res: http.ServerResponse) => void} = {}

  beforeAll(async () => {
    server = http.createServer((req, res) => {
      const route = routes[req.url ?? ""]
      if (route) route(res)
      else res.writeHead(404).end()
    })
    await new Promise<void>(resolve => server.listen(0, "127.0.0.1", resolve))
    base = `http://127.0.0.1:${(server.address() as AddressInfo).port}`
  })

  afterAll(() => new Promise<void>(resolve => server.close(() => resolve())))

  beforeEach(() => { tmp = fs.mkdtempSync(path.join(os.tmpdir(), "libs-test-")) })
  afterEach(() => fs.rmSync(tmp, {recursive: true, force: true}))

  const good = storedZip({[`libs/${LIB}`]: "lib", "libs/libHSdep.so": "dep"})
  routes["/good.zip"] = res => res.writeHead(200).end(good)
  routes["/redirect.zip"] = res => res.writeHead(302, {location: "/good.zip"}).end()
  routes["/nolib.zip"] = res => res.writeHead(200).end(storedZip({"other/file": "x"}))
  routes["/stall.zip"] = res => { res.writeHead(200); res.write(good.subarray(0, 10)) }

  function leftovers(): string[] {
    return fs.readdirSync(tmp).filter(f => f.startsWith(".download-"))
  }

  it("follows a redirect and installs libs/", async () => {
    const target = path.join(tmp, "v1", "sqlite")
    await installLibs(`${base}/redirect.zip`, target, LIB)
    expect(fs.readFileSync(path.join(target, LIB), "utf8")).toBe("lib")
    expect(fs.existsSync(path.join(target, "libHSdep.so"))).toBe(true)
  })

  it("rejects 404 and cleans up", async () => {
    const target = path.join(tmp, "sqlite")
    await expect(installLibs(`${base}/missing.zip`, target, LIB)).rejects.toThrow("HTTP 404")
    expect(leftovers()).toEqual([])
    expect(fs.existsSync(target)).toBe(false)
  })

  it("rejects a zip without the lib", async () => {
    const target = path.join(tmp, "sqlite")
    await expect(installLibs(`${base}/nolib.zip`, target, LIB)).rejects.toThrow(`libs/${LIB} missing`)
    expect(leftovers()).toEqual([])
  })

  it("times out a stalled download", async () => {
    const target = path.join(tmp, "sqlite")
    await expect(installLibs(`${base}/stall.zip`, target, LIB, 200)).rejects.toThrow("timeout")
    expect(leftovers()).toEqual([])
  })

  it("installs into a relative XDG_CACHE_HOME and returns an absolute path", async () => {
    const zip = storedZip({"libs/libsimplex.so": "lib"})
    routes["/libsimplex.zip"] = res => res.writeHead(200).end(zip)
    const requested: string[] = []
    // resolveLibsDir always downloads from GitHub over https; redirect it to the local server.
    const get = jest.spyOn(https, "get").mockImplementation(((url: string, options: http.RequestOptions, cb: (res: http.IncomingMessage) => void) => {
      requested.push(url)
      return http.get(`${base}/libsimplex.zip`, options, cb)
    }) as unknown as typeof https.get)
    try {
      const cache = path.relative(process.cwd(), path.join(tmp, "cache"))
      const dir = await resolveLibsDir("sqlite", {XDG_CACHE_HOME: cache}, "linux", "x64")
      expect(requested).toEqual([libsUrl("sqlite", "linux-x86_64")])
      expect(path.isAbsolute(dir)).toBe(true)
      expect(dir).toBe(path.join(tmp, "cache", "simplex-chat", `v${LIBS_VERSION}`, "sqlite"))
      expect(fs.readFileSync(path.join(dir, "libsimplex.so"), "utf8")).toBe("lib")
    } finally {
      get.mockRestore()
    }
  })

  it("lets concurrent installs share one target", async () => {
    const target = path.join(tmp, "sqlite")
    await Promise.all([
      installLibs(`${base}/good.zip`, target, LIB),
      installLibs(`${base}/good.zip`, target, LIB),
    ])
    expect(fs.readFileSync(path.join(target, LIB), "utf8")).toBe("lib")
    expect(leftovers()).toEqual([])
  })
})

describe("cli", () => {
  it("parses install arguments", () => {
    expect(parseInstallArgs(["install"])).toBe("sqlite")
    expect(parseInstallArgs(["install", "--backend", "postgres"])).toBe("postgres")
    expect(parseInstallArgs(["install", "--backend=postgres"])).toBe("postgres")
    expect(() => parseInstallArgs(["install", "--backend", "mysql"])).toThrow("invalid backend: mysql")
    expect(() => parseInstallArgs(["run"])).toThrow("usage: simplex-chat install")
  })

  it("returns 1 on failure", async () => {
    jest.spyOn(console, "error").mockImplementation(() => {})
    await expect(main(["install", "--backend", "mysql"])).resolves.toBe(1)
  })
})
