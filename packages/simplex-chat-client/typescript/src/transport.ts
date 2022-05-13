import {ABQueue, NextIter} from "./queue"
import {ChatResponse} from "./response"

export class TransportError extends Error {}

export abstract class Transport<W, R> {
  readonly queue: ABQueue<R>

  protected constructor(qSize: number) {
    this.queue = new ABQueue(qSize)
  }

  [Symbol.asyncIterator](): Transport<W, R> {
    return this
  }

  abstract close(): Promise<void>

  abstract write(bytes: W): Promise<void>

  async read(): Promise<R> {
    return this.queue.dequeue()
  }

  async next(): Promise<NextIter<R>> {
    return this.queue.next()
  }
}

type WSData = Uint8Array | string

export class WSTransport extends Transport<WSData, WSData> {
  private constructor(private readonly sock: WebSocket, readonly timeout: number, qSize: number) {
    super(qSize)
  }

  static connect(url: string, timeout: number, qSize: number): Promise<WSTransport> {
    const sock = new WebSocket(url)
    const t = new WSTransport(sock, timeout, qSize)
    sock.onmessage = async ({data}: MessageEvent<WSData>) => await t.queue.enqueue(data)
    sock.onclose = async () => await t.queue.close()
    sock.onerror = () => sock.close()
    return withTimeout(timeout, () => new Promise((r) => (sock.onopen = () => r(t))))
  }

  close(): Promise<void> {
    this.sock.close()
    return Promise.resolve()
  }

  write(data: WSData): Promise<void> {
    const buffered = this.sock.bufferedAmount
    this.sock.send(data)
    return withTimeout(this.timeout, async () => {
      while (this.sock.bufferedAmount > buffered) await delay()
    })
  }

  async readBinary(size: number): Promise<Uint8Array> {
    const data = await this.read()
    if (typeof data == "string") throw new TransportError("invalid text block: expected binary")
    if (data.byteLength !== size) throw new TransportError("invalid block size")
    return data
  }
}

function withTimeout<T>(ms: number, action: () => Promise<T>): Promise<T> {
  return Promise.race([
    action(),
    (async () => {
      await delay(ms)
      throw new Error("timeout")
    })(),
  ])
}

export interface ChatServer {
  readonly host: string
  readonly port?: string
}

export const localServer: ChatServer = {
  host: "localhost",
  port: "5225",
}

export interface ChatSrvRequest {
  corrId: string
  cmd: string
}

export interface ChatSrvResponse {
  corrId?: string
  resp: ChatResponse
}

interface ParsedChatSrvResponse {
  corrId?: string
  resp?: ChatResponse
}

export class ChatResponseError extends Error {
  constructor(public message: string, public data?: string) {
    super(message)
  }
}

export class ChatTransport extends Transport<ChatSrvRequest, ChatSrvResponse | ChatResponseError> {
  private constructor(private readonly ws: WSTransport, readonly timeout: number, qSize: number) {
    super(qSize)
  }

  static async connect(srv: ChatServer | string, timeout: number, qSize: number): Promise<ChatTransport> {
    const uri = typeof srv == "string" ? srv : `ws://${srv.host}:${srv.port || "5225"}`
    const ws = await WSTransport.connect(uri, timeout, qSize)
    const c = new ChatTransport(ws, timeout, qSize)
    processWSQueue(c, ws).then(noop, noop)
    return c
  }

  async close(): Promise<void> {
    await this.ws.close()
  }

  async write(cmd: ChatSrvRequest): Promise<void> {
    return this.ws.write(JSON.stringify(cmd))
  }
}

export function noop(): void {}

async function processWSQueue(c: ChatTransport, ws: WSTransport): Promise<void> {
  for await (const data of ws) {
    const str = (data instanceof Promise ? await data : data) as WSData
    if (typeof str != "string") {
      await c.queue.enqueue(new ChatResponseError("websocket data is not a string"))
      continue
    }
    let resp: ChatSrvResponse | ChatResponseError
    try {
      const json = JSON.parse(str) as ParsedChatSrvResponse | undefined
      if (typeof json?.resp?.type == "string") {
        resp = json as ChatSrvResponse
      } else {
        resp = new ChatResponseError("invalid response format", str)
      }
    } catch (err) {
      resp = new ChatResponseError((err as Error).message, str)
    }
    await c.queue.enqueue(resp)
  }
  await c.queue.close()
}

function delay(ms?: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms))
}
