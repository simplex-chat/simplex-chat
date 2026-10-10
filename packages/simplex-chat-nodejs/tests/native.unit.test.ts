import {createHash} from "crypto"
import * as fs from "fs"
import * as https from "https"
import * as os from "os"
import * as path from "path"
import {Readable} from "stream"
import {nativePaths} from "../src/native"
import {sha256} from "../src/release"

jest.mock("../src/release", () => ({release: "v0.0.0", sha256: {}}))

const addon = Buffer.from("addon")
const addonHash = createHash("sha256").update(addon).digest("hex")
const targets = ["linux-x86_64", "linux-aarch64", "macos-x86_64", "macos-aarch64", "windows-x86_64", "windows-aarch64"]

interface MockResponse {
  statusCode: number
  location?: string
  body?: Buffer
}

function mockGet(...responses: MockResponse[]) {
  const get = jest.spyOn(https, "get")
  for (const {statusCode, location, body} of responses) {
    get.mockImplementationOnce(((_url: string, _options: object, callback: (response: Readable) => void) => {
      const response = Object.assign(Readable.from(body ? [body] : []), {statusCode, headers: {location}})
      process.nextTick(() => callback(response))
      return {setTimeout: () => undefined, on: () => undefined}
    }) as unknown as typeof https.get)
  }
  return get
}

describe("nativePaths", () => {
  const env = process.env
  let cacheDir: string

  beforeEach(() => {
    cacheDir = fs.mkdtempSync(path.join(os.tmpdir(), "simplex-native-"))
    process.env = {...env, SIMPLEX_CACHE_DIR: cacheDir, SIMPLEX_LIBS_DIR: cacheDir}
    delete process.env.SIMPLEX_ADDON_PATH
    for (const target of targets) sha256[`simplex-chat-nodejs-${target}.node`] = addonHash
    jest.spyOn(console, "log").mockImplementation(() => {})
  })

  afterEach(() => {
    process.env = env
    fs.rmSync(cacheDir, {recursive: true, force: true})
    jest.restoreAllMocks()
  })

  it("downloads the add-on once, following redirects", async () => {
    const get = mockGet({statusCode: 302, location: "https://example.com/addon"}, {statusCode: 200, body: addon})
    const {addon: addonPath} = await nativePaths("sqlite")
    expect(addonPath).toBe(path.join(cacheDir, "v0.0.0", "nodejs", "simplex.node"))
    expect(fs.readFileSync(addonPath)).toEqual(addon)
    await nativePaths("sqlite")
    expect(get.mock.calls.map(([url]) => url)).toEqual([
      expect.stringMatching(/^https:\/\/github\.com\/simplex-chat\/simplex-chat-libs\/releases\/download\/v0\.0\.0\/simplex-chat-nodejs-/),
      "https://example.com/addon"
    ])
  })

  it("rejects the add-on with a different SHA-256", async () => {
    mockGet({statusCode: 200, body: Buffer.from("other")})
    await expect(nativePaths("sqlite")).rejects.toThrow(`expected ${addonHash}`)
    expect(fs.readdirSync(path.join(cacheDir, "v0.0.0"))).toEqual([])
  })

  it("uses the add-on and the libraries set in the environment", async () => {
    process.env.SIMPLEX_ADDON_PATH = path.join(cacheDir, "simplex.node")
    const get = jest.spyOn(https, "get")
    const paths = await nativePaths("sqlite")
    expect(paths.addon).toBe(path.join(cacheDir, "simplex.node"))
    expect(path.dirname(paths.libsimplex)).toBe(cacheDir)
    expect(get).not.toHaveBeenCalled()
  })
})
