import {createHash} from "crypto"
import * as fs from "fs"
import * as http from "http"
import * as https from "https"
import * as os from "os"
import * as path from "path"
import {pipeline} from "stream/promises"
import extract = require("extract-zip")
import {release, sha256} from "./release"

export type Backend = "sqlite" | "postgres"

export interface Libsimplex {
  chat_migrate_init(dbPath: string, dbKey: string, confirm: string): Promise<[bigint, string]>
  chat_migrate_init_queue(dbPath: string, dbKey: string, confirm: string, queueSize: number): Promise<[bigint, string]>
  chat_close_store(ctrl: bigint): Promise<string>
  chat_send_cmd(ctrl: bigint, cmd: string): Promise<string>
  chat_recv_msg_wait(ctrl: bigint, wait: number): Promise<string>
  chat_write_file(ctrl: bigint, path: string, buffer: ArrayBuffer | Uint8Array): Promise<string>
  chat_read_file(path: string, key: string, nonce: string): Promise<Buffer>
  chat_encrypt_file(ctrl: bigint, fromPath: string, toPath: string): Promise<string>
  chat_decrypt_file(fromPath: string, key: string, nonce: string, toPath: string): Promise<string>
}

export interface NativePaths {
  addon: string
  libsimplex: string
}

const platforms: {[P in string]?: {name: string, libsimplex: string}} = {
  linux: {name: "linux", libsimplex: "libsimplex.so"},
  darwin: {name: "macos", libsimplex: "libsimplex.dylib"},
  win32: {name: "windows", libsimplex: "libsimplex.dll"}
}

const archs: {[A in string]?: string} = {x64: "x86_64", arm64: "aarch64"}

const addonFile = "simplex.node"

let loaded: {backend: Backend, libsimplex: Promise<Libsimplex>} | undefined

export function loadLibsimplex(backend?: Backend): Promise<Libsimplex> {
  if (!loaded) {
    const selected = backend ?? "sqlite"
    const current = {backend: selected, libsimplex: load(selected)}
    current.libsimplex.catch(() => {
      if (loaded === current) loaded = undefined
    })
    loaded = current
  } else if (backend && backend !== loaded.backend) {
    return Promise.reject(new Error(`libsimplex is loaded with ${loaded.backend} backend`))
  }
  return loaded.libsimplex
}

async function load(backend: Backend): Promise<Libsimplex> {
  const {addon, libsimplex} = await nativePaths(backend)
  const addonModule = {exports: {} as {load(libsimplex: string): Libsimplex}}
  process.dlopen(addonModule, addon)
  return addonModule.exports.load(libsimplex)
}

export async function nativePaths(backend: Backend): Promise<NativePaths> {
  const platform = platforms[process.platform]
  const arch = archs[process.arch]
  if (!platform || !arch) throw new Error(`Unsupported platform: ${process.platform} ${process.arch}`)
  const target = `${platform.name}-${arch}`
  const libsAsset = `simplex-chat-libs-${target}${backend === "postgres" ? "-postgres" : ""}.zip`
  const addonAsset = `simplex-chat-nodejs-${target}.node`
  const releaseDir = path.join(cacheDir(), release)
  const libsDir = process.env.SIMPLEX_LIBS_DIR
    || await cached(path.join(releaseDir, backend), (tmp) => downloadLibs(libsAsset, tmp))
  const addon = process.env.SIMPLEX_ADDON_PATH
    || path.join(await cached(path.join(releaseDir, "nodejs"), (tmp) => downloadAddon(addonAsset, tmp)), addonFile)
  return {addon: path.resolve(addon), libsimplex: path.resolve(libsDir, platform.libsimplex)}
}

function cacheDir(): string {
  return path.resolve(process.env.SIMPLEX_CACHE_DIR || path.join(userCacheDir(), "simplex-chat"))
}

function userCacheDir(): string {
  switch (process.platform) {
    case "darwin":
      return path.join(os.homedir(), "Library", "Caches")
    case "win32":
      return process.env.LOCALAPPDATA || path.join(os.homedir(), "AppData", "Local")
    default:
      return process.env.XDG_CACHE_HOME || path.join(os.homedir(), ".cache")
  }
}

async function cached(dir: string, install: (tmp: string) => Promise<string>): Promise<string> {
  if (fs.existsSync(dir)) return dir
  fs.mkdirSync(path.dirname(dir), {recursive: true})
  const tmp = fs.mkdtempSync(path.join(path.dirname(dir), ".download-"))
  try {
    const installed = await install(tmp)
    try {
      fs.renameSync(installed, dir)
    } catch (e) {
      if (!fs.existsSync(dir)) throw e
    }
  } finally {
    fs.rmSync(tmp, {recursive: true, force: true})
  }
  return dir
}

async function downloadLibs(asset: string, tmp: string): Promise<string> {
  const zip = path.join(tmp, asset)
  await download(asset, zip)
  await extract(zip, {dir: tmp})
  return path.join(tmp, "libs")
}

async function downloadAddon(asset: string, tmp: string): Promise<string> {
  await download(asset, path.join(tmp, addonFile))
  return tmp
}

async function download(asset: string, file: string): Promise<void> {
  const expected = sha256[asset]
  if (!expected) throw new Error(`${asset} is not in release ${release}`)
  const url = `https://github.com/simplex-chat/simplex-chat-libs/releases/download/${release}/${asset}`
  console.log(`Downloading ${url}`)
  await pipeline(await get(url), fs.createWriteStream(file))
  const actual = createHash("sha256").update(fs.readFileSync(file)).digest("hex")
  if (actual !== expected) throw new Error(`${asset}: SHA-256 ${actual}, expected ${expected}`)
}

function get(url: string, redirects = 5): Promise<http.IncomingMessage> {
  return new Promise((resolve, reject) => {
    const request = https.get(url, {headers: {"User-Agent": "simplex-chat"}}, (response) => {
      const {statusCode, headers: {location}} = response
      if (statusCode === 200) {
        resolve(response)
      } else if (statusCode && statusCode >= 300 && statusCode < 400 && location && redirects > 0) {
        response.resume()
        resolve(get(new URL(location, url).href, redirects - 1))
      } else {
        response.resume()
        reject(new Error(`HTTP ${statusCode} ${url}`))
      }
    })
    request.setTimeout(60_000, () => request.destroy(new Error(`Timeout ${url}`)))
    request.on("error", reject)
  })
}
