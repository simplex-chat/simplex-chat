import {Server, ServerResponse, createServer} from "http"
import {AddressInfo} from "net"
import {InferenceState, inferencePoller} from "../../src/inference"

type Answer = (response: ServerResponse) => void

interface Upstream {
  url: string
  port: number
  requests: {method: string; path: string; authorization?: string}[]
  answers: {[path: string]: Answer}
}

const servers: Server[] = []
const delays: NodeJS.Timeout[] = []

afterEach(async () => {
  jest.restoreAllMocks()
  for (const delay of delays.splice(0)) clearTimeout(delay)
  for (const server of servers.splice(0)) {
    server.closeAllConnections()
    await new Promise(resolve => server.close(resolve))
  }
})

async function upstream(answers: {[path: string]: Answer} = {}): Promise<Upstream> {
  const requests: Upstream["requests"] = []
  const server = createServer((request, response) => {
    const path = request.url ?? ""
    const {authorization} = request.headers
    requests.push({method: request.method ?? "", path, ...(authorization === undefined ? {} : {authorization})})
    const answer = answers[path] ?? json(404, {error: "Not Found"})
    answer(response)
  })
  servers.push(server)
  await new Promise<void>(resolve => server.listen(0, "127.0.0.1", resolve))
  const {port} = server.address() as AddressInfo
  return {url: `http://127.0.0.1:${port}`, port, requests, answers}
}

async function refusedUrl(): Promise<string> {
  const server = createServer()
  await new Promise<void>(resolve => server.listen(0, "127.0.0.1", resolve))
  const {port} = server.address() as AddressInfo
  await new Promise(resolve => server.close(resolve))
  return `http://127.0.0.1:${port}`
}

function json(status: number, body: unknown): Answer {
  return response => {
    response.writeHead(status, {"content-type": "application/json"})
    response.end(JSON.stringify(body))
  }
}

function later(ms: number, answer: Answer): Answer {
  return response => {
    delays.push(setTimeout(() => answer(response), ms))
  }
}

function running(...models: {model: unknown; state: string; proxy?: unknown}[]): Answer {
  return json(200, {running: models})
}

function awake(slots: object[] = []): {[path: string]: Answer} {
  return {"/props": json(200, {is_sleeping: false}), "/slots": json(200, slots)}
}

function swapFor(proxy: Upstream): Promise<Upstream> {
  return upstream({"/running": running({model: "qwen3-coder-30b", state: "ready", proxy: proxy.url})})
}

function slot(id: number, task: number, decoded: number, prompt = 0): object {
  return {id, id_task: task, is_processing: true, n_prompt_tokens_processed: prompt, next_token: [{n_decoded: decoded}]}
}

function poller(url: string, key?: string): () => Promise<InferenceState | undefined> {
  return inferencePoller({url, key}, 0)
}

function requested({requests}: Upstream): string[] {
  return requests.map(({method, path}) => `${method} ${path}`)
}

describe("inference poller", () => {
  it("returns loading with the model llama-swap starts, after /running only", async () => {
    const proxy = await upstream(awake())
    const swap = await upstream({"/running": running({model: "qwen3-coder-30b", state: "starting", proxy: proxy.url}, {model: "gemma-3-4b", state: "ready", proxy: proxy.url})})
    expect(await poller(swap.url)()).toEqual({type: "loading", model: "qwen3-coder-30b"})
    expect(requested(swap)).toEqual(["GET /running"])
    expect(proxy.requests).toEqual([])
  })

  it("returns loading without a model while one stops and none is ready, and nothing without models", async () => {
    const swap = await upstream({"/running": running({model: "qwen3-coder-30b", state: "stopping", proxy: "http://127.0.0.1:1"})})
    const poll = poller(swap.url)
    expect(await poll()).toEqual({type: "loading"})
    swap.answers["/running"] = running()
    expect(await poll()).toBeUndefined()
  })

  it("reads /props then /slots of ready models at distinct allowed proxies, without the key, at most 4", async () => {
    const proxies = await Promise.all(Array.from({length: 5}, () => upstream(awake())))
    const others = proxies.slice(1).map((proxy, i) => ({model: `model-${i}`, state: "ready", proxy: proxy.url}))
    const swap = await upstream({"/running": running({model: "qwen3-coder-30b", state: "ready", proxy: proxies[0].url}, {model: "qwen3-coder-30b-draft", state: "ready", proxy: proxies[0].url}, ...others)})
    expect(await poller(swap.url, "secret")()).toEqual({type: "idle"})
    expect(swap.requests).toEqual([{method: "GET", path: "/running", authorization: "Bearer secret"}])
    expect(proxies.map(({requests}) => requests)).toEqual([...Array(4).fill([{method: "GET", path: "/props"}, {method: "GET", path: "/slots"}]), []])
  })

  it("computes tokens/s between two polls for slots that keep their task, and sums them", async () => {
    const proxy = await upstream(awake([slot(0, 7, 10), slot(1, 8, 5)]))
    const swap = await swapFor(proxy)
    const poll = poller(swap.url)
    const now = jest.spyOn(Date, "now").mockReturnValue(10_000)
    expect(await poll()).toEqual({type: "output", model: "qwen3-coder-30b"})
    proxy.answers["/slots"] = json(200, [slot(0, 7, 30), slot(1, 8, 25)])
    now.mockReturnValue(12_000)
    expect(await poll()).toEqual({type: "output", model: "qwen3-coder-30b", rate: 20})
    proxy.answers["/slots"] = json(200, [slot(0, 9, 50), slot(1, 8, 45)])
    now.mockReturnValue(13_000)
    expect(await poll()).toEqual({type: "output", model: "qwen3-coder-30b", rate: 20})
    now.mockReturnValue(14_000)
    expect(await poll()).toEqual({type: "output", model: "qwen3-coder-30b"})
  })

  it("returns prompt with the tokens read, and idle when no slot processes", async () => {
    const idle = {id: 2, id_task: 3, is_processing: false, n_prompt_tokens_processed: 512, next_token: [{n_decoded: 40}]}
    const proxy = await upstream(awake([slot(0, 7, 0, 4096), slot(1, 8, 0, 2048), idle]))
    const swap = await swapFor(proxy)
    const poll = poller(swap.url)
    expect(await poll()).toEqual({type: "prompt", model: "qwen3-coder-30b", tokens: 6144})
    proxy.answers["/slots"] = json(200, [idle])
    expect(await poll()).toEqual({type: "idle"})
  })

  it("ignores fractional and negative counts", async () => {
    const proxy = await upstream(awake([slot(0, 7, 0, 4096), slot(1, 8, 2.5, 512.5), slot(2, 9, -3, -1024)]))
    const swap = await swapFor(proxy)
    expect(await poller(swap.url)()).toEqual({type: "prompt", model: "qwen3-coder-30b", tokens: 4096})
  })

  it("reads the first 64 slots of a proxy", async () => {
    const idle = (id: number): object => ({...slot(id, id, 0), is_processing: false})
    const proxy = await upstream(awake([...Array.from({length: 64}, (_, id) => idle(id)), slot(64, 70, 12)]))
    const swap = await swapFor(proxy)
    const poll = poller(swap.url)
    expect(await poll()).toEqual({type: "idle"})
    proxy.answers["/slots"] = json(200, [...Array.from({length: 63}, (_, id) => idle(id)), slot(63, 70, 12)])
    expect(await poll()).toEqual({type: "output", model: "qwen3-coder-30b"})
  })

  it("names the model when one model is busy, and omits it with two", async () => {
    const coder = await upstream(awake([slot(0, 7, 12)]))
    const small = await upstream(awake([{...slot(0, 3, 0), is_processing: false}]))
    const swap = await upstream({"/running": running({model: "qwen3-coder-30b", state: "ready", proxy: coder.url}, {model: "gemma-3-4b", state: "ready", proxy: small.url})})
    const poll = poller(swap.url)
    expect(await poll()).toEqual({type: "output", model: "qwen3-coder-30b"})
    small.answers["/slots"] = json(200, [slot(0, 3, 0, 1024)])
    expect(await poll()).toEqual({type: "output"})
  })

  it("skips /slots of a sleeping server, and returns loading for a 503 of /props or /slots", async () => {
    const proxy = await upstream({"/props": json(200, {is_sleeping: true}), "/slots": json(200, [slot(0, 7, 12)])})
    const swap = await swapFor(proxy)
    const poll = poller(swap.url)
    expect(await poll()).toEqual({type: "idle"})
    expect(requested(proxy)).toEqual(["GET /props"])
    proxy.answers["/props"] = json(503, {error: {code: 503, message: "Loading model"}})
    expect(await poll()).toEqual({type: "loading", model: "qwen3-coder-30b"})
    proxy.answers["/props"] = json(200, {is_sleeping: false})
    proxy.answers["/slots"] = json(503, {error: {code: 503, message: "Loading model"}})
    expect(await poll()).toEqual({type: "loading", model: "qwen3-coder-30b"})
    expect(requested(proxy)).toEqual(["GET /props", "GET /props", "GET /props", "GET /slots"])
  })

  it("reads a plain llama-server at URL with the key, and fails when it answers with an error", async () => {
    const server = await upstream(awake([slot(0, 7, 12)]))
    expect(await poller(server.url, "secret")()).toEqual({type: "output"})
    expect(server.requests).toEqual(["/running", "/props", "/slots"].map(path => ({method: "GET", path, authorization: "Bearer secret"})))
    server.answers["/slots"] = json(500, {error: "server error"})
    await expect(poller(server.url, "secret")()).rejects.toThrow(new Error(`GET ${server.url}/slots: HTTP 500`))
  })

  it("polls proxies on URL's host, or on loopback when URL is loopback, and no others", async () => {
    const local = await upstream(awake())
    const other = await upstream(awake())
    const refused = [
      {model: "b", state: "ready", proxy: `http://10.0.0.1:${other.port}`},
      {model: "c", state: "ready", proxy: `http://user:secret@127.0.0.1:${other.port}`},
      {model: "d", state: "ready", proxy: `ftp://127.0.0.1:${other.port}`},
      {model: "e", state: "ready", proxy: other.port},
    ]
    const swap = await upstream({"/running": running({model: "a", state: "ready", proxy: `http://localhost:${local.port}/v1/models?x=1`}, ...refused)})
    expect(await poller(swap.url)()).toEqual({type: "idle"})
    expect(requested(local)).toEqual(["GET /props", "GET /slots"])
    swap.answers["/running"] = running(...refused)
    expect(await poller(swap.url)()).toBeUndefined()
    expect(other.requests).toEqual([])
  })

  it("shares one poll for 3 s", async () => {
    const swap = await upstream({"/running": running()})
    const poll = inferencePoller({url: swap.url, key: undefined})
    const now = jest.spyOn(Date, "now").mockReturnValue(10_000)
    const first = poll()
    expect(poll()).toBe(first)
    expect(await first).toBeUndefined()
    now.mockReturnValue(12_999)
    expect(await poll()).toBeUndefined()
    expect(requested(swap)).toEqual(["GET /running"])
    now.mockReturnValue(13_000)
    expect(await poll()).toBeUndefined()
    expect(requested(swap)).toEqual(["GET /running", "GET /running"])
  })

  it("fails with the request and its status, invalid JSON, a body over 1 MiB, a redirect, a refused connection or no answer within 1 s, without the body in the text", async () => {
    const swap = await upstream()
    const poll = poller(swap.url)
    const failure = (text: string): Error => new Error(`GET ${swap.url}/running: ${text}`)
    swap.answers["/running"] = json(401, {error: "\u001b[31munauthorized"})
    await expect(poll()).rejects.toThrow(failure("HTTP 401"))
    swap.answers["/running"] = response => {
      response.writeHead(200, {"content-type": "application/json"})
      response.end("\u001b[31m{")
    }
    await expect(poll()).rejects.toThrow(failure("invalid JSON"))
    swap.answers["/running"] = response => response.end(Buffer.alloc(1_048_577, 32))
    await expect(poll()).rejects.toThrow(failure("body over 1 MiB"))
    swap.answers["/running"] = response => {
      response.write(Buffer.alloc(600_000, 32))
      response.end(Buffer.alloc(600_000, 32))
    }
    await expect(poll()).rejects.toThrow(failure("body over 1 MiB"))
    swap.answers["/running"] = response => response.end(`{"running":[],"pad":"${"x".repeat(1_048_576 - 23)}"}`)
    expect(await poll()).toBeUndefined()
    swap.answers["/running"] = response => {
      response.writeHead(302, {location: "/elsewhere"})
      response.end()
    }
    await expect(poll()).rejects.toThrow(failure("unexpected redirect"))
    expect(requested(swap)).not.toContain("GET /elsewhere")
    const refused = await refusedUrl()
    await expect(poller(refused)()).rejects.toThrow(new Error(`GET ${refused}/running: connect ECONNREFUSED ${new URL(refused).host}`))
    swap.answers["/running"] = () => undefined
    await expect(poll()).rejects.toThrow(failure("no answer within 1 s"))
  })

  it("folds the control characters of a failure text into spaces", async () => {
    jest.spyOn(globalThis, "fetch").mockRejectedValueOnce(new TypeError("fetch failed", {cause: new Error("bad\u001b]52;c;eA==\u0007header\r\nvalue")}))
    await expect(poller("http://127.0.0.1:8080")()).rejects.toThrow(new Error("GET http://127.0.0.1:8080/running: bad ]52;c;eA== header value"))
  })

  it("gives all requests of a poll one deadline of 1 s", async () => {
    const proxy = await upstream({"/props": later(600, json(200, {is_sleeping: false})), "/slots": json(200, [])})
    const swap = await upstream({"/running": later(600, running({model: "qwen3-coder-30b", state: "ready", proxy: proxy.url}))})
    await expect(poller(swap.url)()).rejects.toThrow(new Error(`GET ${proxy.url}/props: no answer within 1 s`))
    expect(requested(proxy)).toEqual(["GET /props"])
  })

  it("leaves out a failing proxy when another answers, and fails with the first proxy's error when all fail", async () => {
    const failing = await upstream({
      "/props": response => {
        response.writeHead(404)
        response.end("\u001b]52;c;eA==\u0007")
      },
    })
    const answering = await upstream(awake([slot(0, 7, 12)]))
    const swap = await upstream({"/running": running({model: "qwen3-coder-30b", state: "ready", proxy: failing.url}, {model: "gemma-3-4b", state: "ready", proxy: answering.url})})
    const poll = poller(swap.url)
    expect(await poll()).toEqual({type: "output", model: "gemma-3-4b"})
    answering.answers["/props"] = json(500, {error: "server error"})
    await expect(poll()).rejects.toThrow(new Error(`GET ${failing.url}/props: HTTP 404`))
  })

  it("drops model IDs outside the pattern", async () => {
    const swap = await upstream()
    const poll = poller(swap.url)
    const starting = (model: unknown): Promise<InferenceState | undefined> => {
      swap.answers["/running"] = running({model, state: "starting"})
      return poll()
    }
    for (const model of ["a`b", "x".repeat(65), "qwen 3", "", 42]) expect(await starting(model)).toEqual({type: "loading"})
    for (const model of ["x".repeat(64), "unsloth/Qwen3-Coder-30B-A3B:Q4_K_M+draft@v1.5"]) expect(await starting(model)).toEqual({type: "loading", model})
  })

  it("requests only GET /running, /props and /slots", async () => {
    const proxy = await upstream(awake([slot(0, 7, 12)]))
    const swap = await swapFor(proxy)
    await poller(swap.url, "secret")()
    expect([...requested(swap), ...requested(proxy)]).toEqual(["GET /running", "GET /props", "GET /slots"])
  })
})
