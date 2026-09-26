import * as fs from "fs"
import * as http from "http"
import * as https from "https"
import * as os from "os"
import * as path from "path"
import {pipeline} from "stream/promises"
import extract = require("extract-zip")
import type {Backend} from "./core"

export const LIBS_VERSION = "7.1.0-beta.4"

const GITHUB_REPO = "simplex-chat/simplex-chat-libs"
const REQUEST_TIMEOUT_MS = 60_000
export const MAX_REDIRECTS = 5
const PLATFORMS: {[platform: string]: {name: string, lib: string} | undefined} = {
  linux: {name: "linux", lib: "libsimplex.so"},
  darwin: {name: "macos", lib: "libsimplex.dylib"},
  win32: {name: "windows", lib: "libsimplex.dll"},
}
const ARCHS: {[arch: string]: string | undefined} = {x64: "x86_64", arm64: "aarch64"}
const SUPPORTED = ["linux-x86_64", "linux-aarch64", "macos-x86_64", "macos-aarch64", "windows-x86_64"]

function unsupported(platform: string, arch: string): Error {
  return new Error(`Unsupported platform: ${platform}/${arch}; supported: ${SUPPORTED.join(", ")}`)
}

export function platformTag(platform: string, arch: string): string {
  const tag = `${PLATFORMS[platform]?.name}-${ARCHS[arch]}`
  if (!SUPPORTED.includes(tag)) throw unsupported(platform, arch)
  return tag
}

function libName(platform: string = process.platform, arch: string = process.arch): string {
  const lib = PLATFORMS[platform]?.lib
  if (!lib) throw unsupported(platform, arch)
  return lib
}

export function cacheRoot(platform: string, env: NodeJS.ProcessEnv, home: string = os.homedir()): string {
  if (platform === "darwin") return path.join(home, "Library", "Caches", "simplex-chat")
  if (platform === "win32") {
    if (!env.LOCALAPPDATA) throw new Error("LOCALAPPDATA is not set")
    return path.join(env.LOCALAPPDATA, "simplex-chat")
  }
  return path.join(env.XDG_CACHE_HOME || path.join(home, ".cache"), "simplex-chat")
}

export function libsUrl(backend: Backend, tag: string): string {
  const suffix = backend === "postgres" ? "-postgres" : ""
  return `https://github.com/${GITHUB_REPO}/releases/download/v${LIBS_VERSION}/simplex-chat-libs-${tag}${suffix}.zip`
}

export function libPath(dir: string): string {
  return path.join(dir, libName())
}

export async function resolveLibsDir(
  backend: Backend,
  env: NodeJS.ProcessEnv = process.env,
  platform: string = process.platform,
  arch: string = process.arch
): Promise<string> {
  const lib = libName(platform, arch)
  if (env.SIMPLEX_LIBS_DIR) {
    const dir = path.resolve(env.SIMPLEX_LIBS_DIR)
    if (!fs.existsSync(path.join(dir, lib))) throw new Error(`SIMPLEX_LIBS_DIR has no ${lib}: ${dir}`)
    return dir
  }
  const tag = platformTag(platform, arch)
  if (backend === "postgres" && tag !== "linux-x86_64") {
    throw new Error(`postgres backend is only supported on linux-x86_64; current platform is ${tag}`)
  }
  const target = path.resolve(cacheRoot(platform, env), `v${LIBS_VERSION}`, backend)
  if (!fs.existsSync(path.join(target, lib))) await installLibs(libsUrl(backend, tag), target, lib)
  return target
}

export async function installLibs(url: string, target: string, lib: string, timeoutMs: number = REQUEST_TIMEOUT_MS): Promise<void> {
  const parent = path.dirname(target)
  await fs.promises.mkdir(parent, {recursive: true})
  const tmp = await fs.promises.mkdtemp(path.join(parent, ".download-"))
  try {
    console.error(`Downloading libsimplex from ${url} ...`)
    const zipPath = path.join(tmp, "libs.zip")
    await download(url, zipPath, timeoutMs)
    await extract(zipPath, {dir: tmp})
    const extracted = path.join(tmp, "libs")
    if (!fs.existsSync(path.join(extracted, lib))) throw new Error(`libs/${lib} missing from ${url}`)
    try {
      await fs.promises.rename(extracted, target)
    } catch (e) {
      // Another process installed the same version first; its files are identical.
      // Windows reports renaming onto an existing directory as EPERM.
      const code = (e as NodeJS.ErrnoException).code
      const lost = (code === "EEXIST" || code === "ENOTEMPTY" || code === "EPERM") && fs.existsSync(target)
      if (!lost) throw e
      if (!fs.existsSync(path.join(target, lib))) {
        throw new Error(`another process partially populated ${target} but libsimplex is missing; remove the directory manually and retry`)
      }
    }
  } finally {
    await fs.promises.rm(tmp, {recursive: true, force: true})
  }
}

function download(url: string, dest: string, timeoutMs: number, redirects = 0): Promise<void> {
  return new Promise((resolve, reject) => {
    const get = url.startsWith("https:") ? https.get : http.get
    // Once a response arrives, request errors are ignored: a download settles via pipeline, which
    // waits for the file to close, so tmp cleanup cannot fail with EBUSY on Windows.
    let res: http.IncomingMessage | undefined
    const req = get(url, {headers: {"User-Agent": "simplex-chat-nodejs"}, timeout: timeoutMs}, response => {
      res = response
      const status = response.statusCode ?? 0
      const location = response.headers.location
      if (status >= 300 && status < 400 && location) {
        response.resume()
        if (redirects >= MAX_REDIRECTS) return reject(new Error(`too many redirects downloading ${url}`))
        let next: URL
        try {
          next = new URL(location, url)
        } catch (e) {
          return reject(e)
        }
        if (next.protocol !== new URL(url).protocol) return reject(new Error(`redirect from ${url} to ${next} changes protocol`))
        download(next.toString(), dest, timeoutMs, redirects + 1).then(resolve, reject)
        return
      }
      if (status !== 200) {
        response.resume()
        return reject(new Error(`HTTP ${status} downloading ${url}`))
      }
      pipeline(response, fs.createWriteStream(dest)).then(resolve, reject)
    })
    const abort = (err: Error) => (res ? res.destroy(err) : req.destroy(err))
    req.on("timeout", () => abort(new Error(`timeout downloading ${url}`)))
    req.on("error", e => { if (!res) reject(e) })
  })
}
