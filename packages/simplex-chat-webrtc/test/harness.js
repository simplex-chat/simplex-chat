// Runs two headless Chromium peers with call.js; the test plays the native app and the chat core.
// Media can only flow through UDP relays controlled by the test, so outages and path changes are simulated
// by dropping packets in the relays.
const http = require("http")
const fs = require("fs")
const path = require("path")
const dgram = require("dgram")
const {chromium} = require("playwright-core")
const LZString = require("lz-string")

const ROOT = path.join(__dirname, "..")
const PAGE_FILES = {
  "/android/call.html": path.join(ROOT, "src/android/call.html"),
  "/android/style.css": path.join(ROOT, "src/android/style.css"),
  "/call.js": path.join(ROOT, "dist/call.js"),
  "/lz-string.min.js": path.join(ROOT, "node_modules/lz-string/libs/lz-string.min.js"),
}
const CONTENT_TYPES = {".html": "text/html", ".js": "text/javascript", ".css": "text/css"}

const startTime = Date.now()
function log(...args) {
  if (process.env.VERBOSE) console.log(((Date.now() - startTime) / 1000).toFixed(1).padStart(6), ...args)
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms))
const decode = (s) => JSON.parse(LZString.decompressFromBase64(s))
const encode = (x) => LZString.compressToBase64(JSON.stringify(x))

function servePage() {
  const server = http.createServer((req, res) => {
    const file = PAGE_FILES[new URL(req.url, "http://localhost").pathname]
    if (!file) return res.writeHead(404).end()
    res.writeHead(200, {"Content-Type": CONTENT_TYPES[path.extname(file)]}).end(fs.readFileSync(file))
  })
  return new Promise((resolve) => server.listen(0, "127.0.0.1", () => resolve(server)))
}

function bindUdp(ip) {
  return new Promise((resolve) => {
    const socket = dgram.createSocket("udp4")
    socket.bind(0, ip, () => resolve(socket))
  })
}

// Relays one ICE generation: each peer is told that the other peer is at the relay socket.
class Relay {
  constructor(network, ip) {
    this.network = network
    this.ip = ip
    this.ufrag = {A: undefined, B: undefined}
    this.real = {A: undefined, B: undefined}
    this.dead = false
    this.forwarded = 0
  }

  async open() {
    this.sockets = {A: await bindUdp(this.ip), B: await bindUdp(this.ip)}
    this.sockets.A.on("message", (msg, from) => this.forward(msg, from, "B", "A"))
    this.sockets.B.on("message", (msg, from) => this.forward(msg, from, "A", "B"))
  }

  // socket A is advertised to B as A's address, so packets on it come from B and go to A via socket B
  forward(msg, from, sender, receiver) {
    if (this.dead || this.network.down) return
    this.real[sender] = from
    const to = this.real[receiver]
    if (!to) return
    this.forwarded++
    this.sockets[sender].send(msg, to.port, to.address)
  }

  port(side) {
    return this.sockets[side].address().port
  }

  close() {
    this.sockets.A.close()
    this.sockets.B.close()
  }
}

class Network {
  constructor() {
    this.down = false
    this.relays = []
  }

  // Returns the rewritten candidate, or undefined if the candidate cannot be relayed.
  async route(side, candidate) {
    const [, component, protocol, , ip, port, , type] = candidate.candidate.split(" ")
    if (protocol.toLowerCase() !== "udp" || type !== "host" || ip.includes(":") || component !== "1") return
    const ufrag = candidate.usernameFragment
    if (this.relays.some((r) => r.ufrag[side] === ufrag)) return
    let relay = this.relays.find((r) => !r.dead && r.ufrag[side] === undefined)
    if (!relay) {
      relay = new Relay(this, ip)
      await relay.open()
      this.relays.push(relay)
    }
    relay.ufrag[side] = ufrag
    relay.real[side] = {address: ip, port: +port}
    log(`relay ${this.relays.indexOf(relay)}: ${side} ${ip}:${port} -> ${relay.ip}:${relay.port(side)}`)
    const parts = candidate.candidate.split(" ")
    parts[4] = relay.ip
    parts[5] = String(relay.port(side))
    return {...candidate, candidate: parts.join(" ")}
  }

  // Takes and returns compressed session description and candidates, as in signaling messages
  async routeSignal(side, session, iceCandidates) {
    // candidates in SDP would connect the peers directly, bypassing the relays
    if (session && decode(session).sdp.includes("a=candidate:")) throw new Error(`${side}: SDP has ICE candidates`)
    const routed = []
    for (const c of decode(iceCandidates)) {
      const r = c?.candidate && (await this.route(side, c))
      if (r) routed.push(r)
    }
    return {session, iceCandidates: encode(routed)}
  }

  // Addresses of all current ICE generations become unreachable, as when a device switches networks.
  changePath() {
    for (const relay of this.relays) relay.dead = true
  }

  close() {
    for (const relay of this.relays) relay.close()
  }
}

class Peer {
  constructor(name, page, {ignoresIceRestart = false} = {}) {
    this.name = name
    this.page = page
    this.ignoresIceRestart = ignoresIceRestart
    this.messages = []
    this.waiters = []
    this.onResponse = async () => {}
  }

  async open(url) {
    await this.page.exposeFunction("sendToTest", (json) => this.receive(JSON.parse(json)))
    this.page.on("console", (m) => log(`${this.name} console:`, m.text().slice(0, 200)))
    this.page.on("pageerror", (e) => log(`${this.name} page error:`, e.message))
    await this.page.goto(url)
    await this.page.evaluate(() => {
      sendMessageToNative = (msg) => window.sendToTest(JSON.stringify(msg))
    })
  }

  receive(msg) {
    const resp = msg.resp
    log(`${this.name} <-`, resp.type, resp.state?.connectionState ?? "", resp.session ? "with session" : "")
    this.messages.push(msg)
    if (!msg.command) this.onResponse(resp).catch((e) => log(`${this.name} response error`, e))
    this.waiters = this.waiters.filter((w) => !(w.pred(msg) && (w.resolve(msg), true)))
  }

  connectionStates() {
    return this.messages.filter((m) => m.resp.type === "connection").map((m) => m.resp.state.connectionState)
  }

  sessionsSent() {
    return this.messages.filter((m) => m.resp.type === "ice" && !m.command && m.resp.session).length
  }

  ended() {
    return this.messages.some((m) => m.resp.type === "ended")
  }

  waitFor(pred, timeout, what) {
    const found = this.messages.find(pred)
    if (found) return Promise.resolve(found)
    return new Promise((resolve, reject) => {
      const waiter = {
        pred,
        resolve: (msg) => {
          clearTimeout(timer)
          resolve(msg)
        },
      }
      this.waiters.push(waiter)
      const timer = setTimeout(() => {
        this.waiters = this.waiters.filter((w) => w !== waiter)
        reject(new Error(`${this.name}: no ${what} in ${timeout}ms, connection states: ${this.connectionStates()}`))
      }, timeout)
    })
  }

  waitForState(state, timeout, after = 0) {
    const pred = (m) => m.resp.type === "connection" && m.resp.state.connectionState === state && this.messages.indexOf(m) >= after
    return this.waitFor(pred, timeout, `"${state}" state`)
  }

  waitForEnded(timeout) {
    return this.waitFor((m) => m.resp.type === "ended", timeout, "ended")
  }

  command(command) {
    log(`${this.name} ->`, command.type, command.session ? "with session" : "")
    return this.page.evaluate((c) => processCommand({corrId: 0, command: c}), command)
  }

  async setTimeouts(timeouts) {
    await this.page.evaluate((t) => {
      if (t.reconnectTimeout) reconnectTimeout = t.reconnectTimeout
      if (t.iceRestartInterval) iceRestartInterval = t.iceRestartInterval
    }, timeouts)
  }

  async receivedBytes() {
    return (await this.inboundAudio()).bytesReceived
  }

  // energy grows only when received audio is decoded, i.e. it is decrypted with the right key
  async receivedAudioEnergy() {
    return (await this.inboundAudio()).totalAudioEnergy
  }

  inboundAudio() {
    return this.page.evaluate(async () => {
      let audio
      ;(await activeCall.connection.getStats()).forEach((s) => {
        if (s.type === "inbound-rtp" && s.kind === "audio" && (!audio || s.bytesReceived > audio.bytesReceived)) audio = s
      })
      return {bytesReceived: audio?.bytesReceived ?? 0, totalAudioEnergy: audio?.totalAudioEnergy ?? 0}
    })
  }
}

// Signaling stops while the network is down and resumes in order, as messages queued in SMP servers.
class Signaling {
  constructor(network) {
    this.network = network
    this.queue = Promise.resolve()
    this.closed = false
  }

  send(deliver) {
    this.queue = this.queue.then(async () => {
      while (this.network.down && !this.closed) await sleep(100)
      if (!this.closed) await deliver().catch((e) => log("signaling error", e.message))
    })
  }
}

// useWorker encrypts media in a worker with RTCRtpScriptTransform, as the desktop app does
async function openPeers({answerer = {}, useWorker = false} = {}) {
  const server = await servePage()
  const url = `http://localhost:${server.address().port}/android/call.html`
  const browser = await chromium.launch({
    executablePath: process.env.CHROME,
    args: [
      "--use-fake-ui-for-media-stream",
      "--use-fake-device-for-media-stream",
      "--disable-features=WebRtcHideLocalIpsWithMdns",
      "--autoplay-policy=no-user-gesture-required",
    ],
  })
  const newPeer = async (name, opts) => {
    const context = await browser.newContext({permissions: ["camera", "microphone"]})
    const peer = new Peer(name, await context.newPage(), opts)
    await peer.open(url)
    if (useWorker) await peer.page.evaluate(() => (useWorker = true))
    return peer
  }
  // A sends the call invitation and B accepts it, so B sends the WebRTC offer, as "start" command in the apps
  const a = await newPeer("A", answerer)
  const b = await newPeer("B")
  const network = new Network()
  const signaling = new Signaling(network)
  const close = async () => {
    signaling.closed = true
    await browser.close()
    network.close()
    server.close()
  }
  return {a, b, network, signaling, close}
}

// The test relays signaling between the peers directly, as the apps and the chat core would do.
async function startCall({media = "audio", answerer = {}} = {}) {
  const peers = await openPeers({answerer})
  const {a, b, network, signaling, close} = peers
  const relayIce = (from, to) => async (resp) => {
    if (resp.type !== "ice") return
    const {session, iceCandidates} = await network.routeSignal(from.name, resp.session, resp.iceCandidates)
    signaling.send(() => to.command({type: "ice", iceCandidates, session: to.ignoresIceRestart ? undefined : session}))
  }
  a.onResponse = relayIce(a, b)
  b.onResponse = relayIce(b, a)

  try {
    await a.command({type: "capabilities", media})
    const offer = await b.command({type: "start", media, iceServers: []})
    if (offer.resp.type !== "offer") throw new Error(`B start: ${JSON.stringify(offer.resp)}`)
    const offerSignal = await network.routeSignal("B", offer.resp.offer, offer.resp.iceCandidates)
    const answer = await a.command({
      type: "offer",
      offer: offerSignal.session,
      iceCandidates: offerSignal.iceCandidates,
      media,
      iceServers: [],
    })
    if (answer.resp.type !== "answer") throw new Error(`A offer: ${JSON.stringify(answer.resp)}`)
    const answerSignal = await network.routeSignal("A", answer.resp.answer, answer.resp.iceCandidates)
    const ok = await b.command({type: "answer", answer: answerSignal.session, iceCandidates: answerSignal.iceCandidates})
    if (ok.resp.type !== "ok") throw new Error(`B answer: ${JSON.stringify(ok.resp)}`)
    await Promise.all([a.waitForState("connected", 20000), b.waitForState("connected", 20000)])
  } catch (e) {
    await close()
    throw e
  }
  return peers
}

module.exports = {openPeers, startCall, sleep, log}
