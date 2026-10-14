// Runs a local SMP server and chat cores with WebSocket API, so that call signaling goes via the chat protocol.
const {spawn, execFileSync} = require("child_process")
const fs = require("fs")
const net = require("net")
const path = require("path")
const {sleep, log} = require("./harness")

const START_TIMEOUT = 30000

function freePort() {
  return new Promise((resolve, reject) => {
    const server = net.createServer()
    server.on("error", reject)
    server.listen(0, "127.0.0.1", () => {
      const {port} = server.address()
      server.close(() => resolve(port))
    })
  })
}

async function waitUntil(what, check) {
  const deadline = Date.now() + START_TIMEOUT
  while (!(await check())) {
    if (Date.now() > deadline) throw new Error(`${what} did not start in ${START_TIMEOUT}ms`)
    await sleep(200)
  }
}

function canConnect(port) {
  return new Promise((resolve) => {
    const socket = net.connect(port, "127.0.0.1", () => {
      socket.destroy()
      resolve(true)
    })
    socket.on("error", () => resolve(false))
  })
}

function spawnLogged(name, bin, args, env) {
  const proc = spawn(bin, args, {env, stdio: ["ignore", "pipe", "pipe"]})
  const output = (data) => log(`${name}:`, data.toString().trimEnd())
  proc.stdout.on("data", output)
  proc.stderr.on("data", output)
  return proc
}

async function startSmpServer(bin, dir) {
  const env = {...process.env, SMP_SERVER_CFG_PATH: path.join(dir, "cfg"), SMP_SERVER_LOG_PATH: path.join(dir, "log")}
  const init = ["init", "--ip", "127.0.0.1", "--no-password", "--disable-web", "--disable-store-log", "-y"]
  const fingerprint = execFileSync(bin, init, {env, stdio: ["ignore", "pipe", "pipe"]})
    .toString()
    .match(/Fingerprint: (\S+)/)[1]
  const port = await freePort()
  const iniFile = path.join(dir, "cfg", "smp-server.ini")
  const ini = fs
    .readFileSync(iniFile, "utf8")
    .replace(/^port = .*$/m, `port = ${port}`)
    .replace(/^host = .*$/m, "host = localhost")
  fs.writeFileSync(iniFile, ini)
  const proc = spawnLogged("smp-server", bin, ["start"], env)
  await waitUntil("SMP server", () => canConnect(port))
  return {address: `smp://${fingerprint}@localhost:${port}`, stop: () => proc.kill()}
}

class ChatClient {
  constructor(name, proc, ws) {
    this.name = name
    this.proc = proc
    this.ws = ws
    this.corrId = 0
    this.pending = new Map()
    this.events = []
    this.waiters = []
    this.onEvent = async () => {}
    ws.addEventListener("message", (e) => this.receive(JSON.parse(e.data)))
  }

  static async start(bin, dir, name, smpServer) {
    const port = await freePort()
    const args = ["-d", path.join(dir, name), "-s", smpServer, "-p", String(port), "--user-display-name", name]
    const proc = spawnLogged(`chat ${name}`, bin, args, process.env)
    await waitUntil(`chat ${name}`, () => canConnect(port))
    const ws = new WebSocket(`ws://127.0.0.1:${port}`)
    await new Promise((resolve, reject) => {
      ws.addEventListener("open", resolve, {once: true})
      ws.addEventListener("error", reject, {once: true})
    })
    return new ChatClient(name, proc, ws)
  }

  receive({corrId, resp}) {
    if (corrId) {
      this.pending.get(corrId)?.(resp)
      this.pending.delete(corrId)
      return
    }
    log(`${this.name} event:`, resp.type)
    this.events.push(resp)
    this.onEvent(resp).catch((e) => log(`${this.name} event error`, e))
    this.waiters = this.waiters.filter((w) => !(w.pred(resp) && (w.resolve(resp), true)))
  }

  async cmd(cmd) {
    const corrId = String(++this.corrId)
    log(`${this.name} cmd:`, cmd.slice(0, 100))
    const resp = await new Promise((resolve) => {
      this.pending.set(corrId, resolve)
      this.ws.send(JSON.stringify({corrId, cmd}))
    })
    if (resp.type === "chatCmdError") throw new Error(`${this.name} ${cmd.split(" ", 2).join(" ")}: ${JSON.stringify(resp)}`)
    return resp
  }

  waitEvent(pred, timeout, what) {
    const found = this.events.find(pred)
    if (found) return Promise.resolve(found)
    return new Promise((resolve, reject) => {
      const waiter = {
        pred,
        resolve: (resp) => {
          clearTimeout(timer)
          resolve(resp)
        },
      }
      this.waiters.push(waiter)
      const timer = setTimeout(() => {
        this.waiters = this.waiters.filter((w) => w !== waiter)
        reject(new Error(`${this.name}: no ${what} event in ${timeout}ms`))
      }, timeout)
    })
  }

  stop() {
    this.ws.close()
    this.proc.kill()
  }
}

async function connectContacts(alice, bob) {
  const inv = await alice.cmd("/c")
  await bob.cmd(`/c ${inv.connLinkInvitation.connFullLink}`)
  const connected = (c) => c.waitEvent((e) => e.type === "contactConnected", START_TIMEOUT, "contactConnected")
  const [a, b] = await Promise.all([connected(alice), connected(bob)])
  return [a.contact.contactId, b.contact.contactId]
}

module.exports = {startSmpServer, ChatClient, connectContacts}
