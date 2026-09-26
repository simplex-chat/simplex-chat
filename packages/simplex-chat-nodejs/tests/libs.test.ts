import * as fs from "fs"
import * as http from "http"
import * as https from "https"
import * as os from "os"
import * as path from "path"
import {AddressInfo} from "net"
import {cacheRoot, installLibs, LIBS_VERSION, libPath, libsUrl, MAX_REDIRECTS, platformTag, resolveLibsDir} from "../src/libs"
import {main, parseInstallArgs} from "../src/cli"
import * as libs from "../src/libs"
import {storedZip} from "./zip"

const LIB = "libtest.so"
const LINUX_LIB = "libsimplex.so"
const {XDG_CACHE_HOME, LOCALAPPDATA, SIMPLEX_LIBS_DIR} = process.env

function setEnv(name: string, value: string | undefined): void {
  if (value === undefined) delete process.env[name]
  else process.env[name] = value
}

// A path resolution regression must neither download nor write into the real user cache.
let home: string
beforeEach(() => {
  home = fs.mkdtempSync(path.join(os.tmpdir(), "libs-home-"))
  jest.spyOn(os, "homedir").mockReturnValue(home)
  jest.spyOn(https, "get").mockImplementation(() => { throw new Error("unexpected download") })
  setEnv("XDG_CACHE_HOME", home)
  setEnv("LOCALAPPDATA", home)
  setEnv("SIMPLEX_LIBS_DIR", undefined)
})
afterEach(() => {
  jest.restoreAllMocks()
  setEnv("XDG_CACHE_HOME", XDG_CACHE_HOME)
  setEnv("LOCALAPPDATA", LOCALAPPDATA)
  setEnv("SIMPLEX_LIBS_DIR", SIMPLEX_LIBS_DIR)
  fs.rmSync(home, {recursive: true, force: true})
})

describe("paths", () => {
  let libsDir: string

  beforeEach(() => {
    libsDir = fs.mkdtempSync(path.join(os.tmpdir(), "libs-dir-"))
    fs.writeFileSync(libPath(libsDir), "lib")
  })
  afterEach(() => fs.rmSync(libsDir, {recursive: true, force: true}))

  it("uses the Python cache layout", () => {
    expect(cacheRoot("linux", {XDG_CACHE_HOME: "/x"}, "/h")).toBe(path.join("/x", "simplex-chat"))
    expect(cacheRoot("linux", {}, "/h")).toBe(path.join("/h", ".cache", "simplex-chat"))
    expect(cacheRoot("darwin", {}, "/h")).toBe(path.join("/h", "Library", "Caches", "simplex-chat"))
    expect(cacheRoot("win32", {LOCALAPPDATA: "C:\\L"}, "/h")).toBe(path.join("C:\\L", "simplex-chat"))
    expect(() => cacheRoot("win32", {}, "/h")).toThrow("LOCALAPPDATA is not set")
  })

  it.each([
    ["linux", "x64", "linux-x86_64"],
    ["linux", "arm64", "linux-aarch64"],
    ["darwin", "x64", "macos-x86_64"],
    ["darwin", "arm64", "macos-aarch64"],
    ["win32", "x64", "windows-x86_64"],
  ])("tags %s/%s as %s", (platform, arch, tag) => {
    expect(platformTag(platform, arch)).toBe(tag)
  })

  it("rejects unsupported platforms", () => {
    expect(() => platformTag("win32", "arm64")).toThrow("Unsupported platform")
  })

  it("rejects an unsupported OS with its arch", async () => {
    await expect(resolveLibsDir("sqlite", {}, "freebsd", "riscv64")).rejects.toThrow("Unsupported platform: freebsd/riscv64; supported: ")
  })

  it("uses the release URLs", () => {
    const release = `https://github.com/simplex-chat/simplex-chat-libs/releases/download/v${LIBS_VERSION}`
    expect(libsUrl("sqlite", "macos-aarch64")).toBe(`${release}/simplex-chat-libs-macos-aarch64.zip`)
    expect(libsUrl("postgres", "linux-x86_64")).toBe(`${release}/simplex-chat-libs-linux-x86_64-postgres.zip`)
  })

  it("resolves an absolute SIMPLEX_LIBS_DIR", async () => {
    await expect(resolveLibsDir("postgres", {SIMPLEX_LIBS_DIR: libsDir})).resolves.toBe(libsDir)
  })

  it("uses SIMPLEX_LIBS_DIR on an unsupported CPU", async () => {
    fs.writeFileSync(path.join(libsDir, LINUX_LIB), "lib")
    await expect(resolveLibsDir("sqlite", {SIMPLEX_LIBS_DIR: libsDir}, "linux", "arm")).resolves.toBe(libsDir)
  })

  it("uses SIMPLEX_LIBS_DIR for postgres outside linux-x86_64", async () => {
    fs.writeFileSync(path.join(libsDir, LINUX_LIB), "lib")
    await expect(resolveLibsDir("postgres", {SIMPLEX_LIBS_DIR: libsDir}, "linux", "arm64")).resolves.toBe(libsDir)
  })

  it("returns a relative SIMPLEX_LIBS_DIR as absolute", async () => {
    await expect(resolveLibsDir("sqlite", {SIMPLEX_LIBS_DIR: path.relative(process.cwd(), libsDir)})).resolves.toBe(libsDir)
  })

  it.each([
    ["linux", "x64", "libsimplex.so"],
    ["darwin", "arm64", "libsimplex.dylib"],
    ["win32", "x64", "libsimplex.dll"],
  ])("looks for the %s library name", async (platform, arch, lib) => {
    fs.rmSync(libPath(libsDir))
    await expect(resolveLibsDir("sqlite", {SIMPLEX_LIBS_DIR: libsDir}, platform, arch)).rejects.toThrow(`SIMPLEX_LIBS_DIR has no ${lib}: `)
  })

  it("rejects a SIMPLEX_LIBS_DIR without libsimplex", async () => {
    fs.rmSync(libPath(libsDir))
    await expect(resolveLibsDir("sqlite", {SIMPLEX_LIBS_DIR: libsDir})).rejects.toThrow(`SIMPLEX_LIBS_DIR has no ${path.basename(libPath(libsDir))}: ${libsDir}`)
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
  let loopRequests = 0
  routes["/loop.zip"] = res => { loopRequests++; res.writeHead(302, {location: "/loop.zip"}).end() }
  routes["/to-http.zip"] = res => res.writeHead(302, {location: `${base}/good.zip`}).end()
  routes["/bad-location.zip"] = res => res.writeHead(302, {location: "http://[bad"}).end()
  routes["/no-location.zip"] = res => res.writeHead(302).end()
  routes["/hang.zip"] = () => {}
  routes["/cross-host.zip"] = res => res.writeHead(302, {location: "https://objects.libs.test/good.zip"}).end()
  let downloadDirs: string[] = []
  routes["/observed.zip"] = res => { downloadDirs = leftovers(); res.writeHead(200).end(good) }

  function leftovers(): string[] {
    return fs.readdirSync(tmp).filter(f => f.startsWith(".download-"))
  }

  // Serves https requests from a local http route (the requested path by default), recording the requested URLs.
  function mockHttps(route?: string): string[] {
    const requested: string[] = []
    jest.spyOn(https, "get").mockImplementation(((url: string, options: http.RequestOptions, cb: (res: http.IncomingMessage) => void) => {
      requested.push(url)
      return http.get(`${base}${route ?? new URL(url).pathname}`, options, cb)
    }) as unknown as typeof https.get)
    return requested
  }

  function renameFails(code: string): void {
    jest.spyOn(fs.promises, "rename").mockRejectedValue(Object.assign(new Error(code), {code}))
  }

  beforeEach(() => { jest.spyOn(console, "error").mockImplementation(() => {}) })

  it("follows a redirect and installs libs/", async () => {
    const target = path.join(tmp, "v1", "sqlite")
    await installLibs(`${base}/redirect.zip`, target, LIB)
    expect(fs.readFileSync(path.join(target, LIB), "utf8")).toBe("lib")
    expect(fs.existsSync(path.join(target, "libHSdep.so"))).toBe(true)
  })

  it("downloads into a temp dir next to the target", async () => {
    await installLibs(`${base}/observed.zip`, path.join(tmp, "sqlite"), LIB)
    expect(downloadDirs).toHaveLength(1)
    expect(leftovers()).toEqual([])
  })

  it("stops following a redirect loop", async () => {
    loopRequests = 0
    await expect(installLibs(`${base}/loop.zip`, path.join(tmp, "sqlite"), LIB)).rejects.toThrow("too many redirects")
    expect(loopRequests).toBe(MAX_REDIRECTS + 1)
    expect(leftovers()).toEqual([])
  })

  it("follows an https redirect to another host", async () => {
    const requested = mockHttps()
    const target = path.join(tmp, "sqlite")
    await installLibs("https://libs.test/cross-host.zip", target, LIB)
    expect(requested).toEqual(["https://libs.test/cross-host.zip", "https://objects.libs.test/good.zip"])
    expect(fs.readFileSync(path.join(target, LIB), "utf8")).toBe("lib")
  })

  it("rejects a redirect from https to http", async () => {
    mockHttps("/to-http.zip")
    await expect(installLibs("https://libs.test/libs.zip", path.join(tmp, "sqlite"), LIB)).rejects.toThrow("changes protocol")
    expect(leftovers()).toEqual([])
  })

  it("rejects an invalid redirect location", async () => {
    await expect(installLibs(`${base}/bad-location.zip`, path.join(tmp, "sqlite"), LIB)).rejects.toThrow("Invalid URL")
    expect(leftovers()).toEqual([])
  })

  it("rejects a redirect without a location", async () => {
    await expect(installLibs(`${base}/no-location.zip`, path.join(tmp, "sqlite"), LIB)).rejects.toThrow("HTTP 302")
    expect(leftovers()).toEqual([])
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

  it("times out a request without a response", async () => {
    await expect(installLibs(`${base}/hang.zip`, path.join(tmp, "sqlite"), LIB, 200)).rejects.toThrow("timeout")
    expect(leftovers()).toEqual([])
  })

  it("times out a stalled download", async () => {
    const target = path.join(tmp, "sqlite")
    await expect(installLibs(`${base}/stall.zip`, target, LIB, 200)).rejects.toThrow("timeout")
    expect(leftovers()).toEqual([])
  })

  it.each(["sqlite", "postgres"] as const)("installs %s into a relative XDG_CACHE_HOME and returns an absolute path", async backend => {
    const zip = storedZip({[`libs/${LINUX_LIB}`]: "lib"})
    routes["/libsimplex.zip"] = res => res.writeHead(200).end(zip)
    const requested = mockHttps("/libsimplex.zip")
    const cache = path.relative(process.cwd(), path.join(tmp, "cache"))
    const dir = await resolveLibsDir(backend, {XDG_CACHE_HOME: cache}, "linux", "x64")
    expect(requested).toEqual([libsUrl(backend, "linux-x86_64")])
    expect(path.isAbsolute(dir)).toBe(true)
    expect(dir).toBe(path.join(tmp, "cache", "simplex-chat", `v${LIBS_VERSION}`, backend))
    expect(fs.readFileSync(path.join(dir, LINUX_LIB), "utf8")).toBe("lib")
  })

  it("does not download a cached lib", async () => {
    const requested = mockHttps("/good.zip")
    const cached = path.join(tmp, "simplex-chat", `v${LIBS_VERSION}`, "postgres")
    fs.mkdirSync(cached, {recursive: true})
    fs.writeFileSync(path.join(cached, LINUX_LIB), "lib")
    await expect(resolveLibsDir("postgres", {XDG_CACHE_HOME: tmp}, "linux", "x64")).resolves.toBe(cached)
    expect(requested).toEqual([])
  })

  it("rejects a target populated without the lib", async () => {
    const target = path.join(tmp, "sqlite")
    fs.mkdirSync(target)
    fs.writeFileSync(path.join(target, "other"), "x")
    await expect(installLibs(`${base}/good.zip`, target, LIB)).rejects.toThrow(`another process partially populated ${target}`)
    expect(leftovers()).toEqual([])
  })

  it.each([["linux", "arm64"], ["darwin", "arm64"]])("rejects postgres on %s/%s without downloading", async (platform, arch) => {
    const requested = mockHttps("/good.zip")
    await expect(resolveLibsDir("postgres", {XDG_CACHE_HOME: tmp}, platform, arch)).rejects.toThrow("postgres backend is only supported on linux-x86_64")
    expect(requested).toEqual([])
  })

  it.each(["EPERM", "EEXIST"])("accepts %s from rename onto an installed target", async code => {
    const target = path.join(tmp, "sqlite")
    fs.mkdirSync(target)
    fs.writeFileSync(path.join(target, LIB), "lib")
    renameFails(code)
    await expect(installLibs(`${base}/good.zip`, target, LIB)).resolves.toBeUndefined()
    expect(leftovers()).toEqual([])
  })

  it("rethrows EPERM from rename when the target does not exist", async () => {
    renameFails("EPERM")
    await expect(installLibs(`${base}/good.zip`, path.join(tmp, "sqlite"), LIB)).rejects.toThrow("EPERM")
    expect(leftovers()).toEqual([])
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
  function installWithLibsDir(dir: string): Promise<number> {
    setEnv("SIMPLEX_LIBS_DIR", dir)
    return main(["install"])
  }

  it("parses install arguments", () => {
    expect(parseInstallArgs(["install"])).toBe("sqlite")
    expect(parseInstallArgs(["install", "--backend", "postgres"])).toBe("postgres")
    expect(parseInstallArgs(["install", "--backend=postgres"])).toBe("postgres")
    expect(() => parseInstallArgs(["install", "--backend", "mysql"])).toThrow("invalid backend: mysql")
    expect(() => parseInstallArgs(["install", "--backend"])).toThrow("argument missing")
    expect(() => parseInstallArgs(["install", "--force"])).toThrow("Unknown option '--force'")
    expect(() => parseInstallArgs(["run"])).toThrow("expected command: install")
    expect(() => parseInstallArgs(["install", "extra"])).toThrow("expected command: install")
    expect(() => parseInstallArgs([])).toThrow("expected command: install")
  })

  it("prints the libs directory", async () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "libs-cli-"))
    fs.writeFileSync(libPath(dir), "lib")
    const log = jest.spyOn(console, "log").mockImplementation(() => {})
    try {
      await expect(installWithLibsDir(dir)).resolves.toBe(0)
      expect(log).toHaveBeenCalledWith(`libsimplex installed at: ${dir}`)
    } finally {
      fs.rmSync(dir, {recursive: true, force: true})
    }
  })

  it("returns 1 when install fails", async () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "libs-cli-"))
    const error = jest.spyOn(console, "error").mockImplementation(() => {})
    try {
      await expect(installWithLibsDir(dir)).resolves.toBe(1)
      expect(error).toHaveBeenCalledWith(expect.stringContaining("install failed: SIMPLEX_LIBS_DIR has no"))
    } finally {
      fs.rmSync(dir, {recursive: true, force: true})
    }
  })

  it("installs the requested backend", async () => {
    const resolve = jest.spyOn(libs, "resolveLibsDir").mockResolvedValue("/libs")
    jest.spyOn(console, "log").mockImplementation(() => {})
    await expect(main(["install", "--backend", "postgres"])).resolves.toBe(0)
    expect(resolve).toHaveBeenCalledWith("postgres")
  })

  it.each(["--help", "-h"])("prints usage on %s", async flag => {
    const log = jest.spyOn(console, "log").mockImplementation(() => {})
    await expect(main(["install", flag])).resolves.toBe(0)
    expect(log).toHaveBeenCalledWith("usage: simplex-chat install [--backend sqlite|postgres]")
  })

  it("returns 2 with usage on invalid arguments", async () => {
    const error = jest.spyOn(console, "error").mockImplementation(() => {})
    await expect(main(["install", "--backend", "mysql"])).resolves.toBe(2)
    expect(error).toHaveBeenCalledWith("invalid backend: mysql\nusage: simplex-chat install [--backend sqlite|postgres]")
  })
})
