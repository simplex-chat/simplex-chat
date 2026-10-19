const {test} = require("node:test")
const assert = require("node:assert/strict")
const {startCall, sleep} = require("./harness")

// Chrome reports "disconnected" about 6 seconds after packets stop
const DISCONNECT_DETECTION = 10000
const RECONNECT = 20000

async function assertMediaFlows(a, b) {
  const before = [await a.receivedBytes(), await b.receivedBytes()]
  await sleep(2000)
  const after = [await a.receivedBytes(), await b.receivedBytes()]
  assert.ok(after[0] > before[0], `A received no media: ${before[0]} -> ${after[0]}`)
  assert.ok(after[1] > before[1], `B received no media: ${before[1]} -> ${after[1]}`)
}

async function assertReconnects(a, b, outage) {
  const from = [a.messages.length, b.messages.length]
  await Promise.all([
    a.waitForState("reconnecting", DISCONNECT_DETECTION, from[0]),
    b.waitForState("reconnecting", DISCONNECT_DETECTION, from[1]),
  ])
  await outage()
  await Promise.all([a.waitForState("connected", RECONNECT, from[0]), b.waitForState("connected", RECONNECT, from[1])])
  await assertMediaFlows(a, b)
  assert.ok(!a.ended() && !b.ended(), "call ended")
  assert.ok(!a.connectionStates().includes("disconnected"), `A states: ${a.connectionStates()}`)
  assert.ok(!b.connectionStates().includes("disconnected"), `B states: ${b.connectionStates()}`)
}

async function withCall(opts, run) {
  const call = await startCall(opts)
  try {
    await run(call)
  } finally {
    await call.close()
  }
}

test("call reconnection", {concurrency: true}, async (t) => {
  await t.test("recovers after a brief outage", () =>
    withCall({}, async ({a, b, network}) => {
      await assertMediaFlows(a, b)
      network.down = true
      await assertReconnects(a, b, async () => {
        network.down = false
      })
    })
  )

  await t.test("recovers after an outage longer than ICE consent timeout", () =>
    withCall({}, async ({a, b, network}) => {
      network.down = true
      await assertReconnects(a, b, async () => {
        await sleep(15000)
        network.down = false
      })
      assert.ok(b.sessionsSent() > 0, "offerer did not restart ICE")
      assert.ok(a.sessionsSent() > 0, "answerer did not answer ICE restart")
    })
  )

  await t.test("recovers when the network path changes", () =>
    withCall({}, async ({a, b, network}) => {
      network.down = true
      network.changePath()
      await assertReconnects(a, b, async () => {
        network.down = false
      })
      const relay = network.relays[network.relays.length - 1]
      assert.ok(!relay.dead && relay.forwarded > 0, "media does not use the new path")
    })
  )

  await t.test("ends the call when reconnection times out", () =>
    withCall({}, async ({a, b, network}) => {
      await Promise.all([a.setTimeouts({reconnectTimeout: 5000}), b.setTimeouts({reconnectTimeout: 5000})])
      network.down = true
      await Promise.all([a.waitForState("reconnecting", DISCONNECT_DETECTION), b.waitForState("reconnecting", DISCONNECT_DETECTION)])
      await Promise.all([a.waitForEnded(10000), b.waitForEnded(10000)])
      assert.deepEqual(a.connectionStates().slice(-2), ["reconnecting", "disconnected"])
      assert.deepEqual(b.connectionStates().slice(-2), ["reconnecting", "disconnected"])
    })
  )

  await t.test("ends the call when the peer does not support ICE restart", () =>
    withCall({answerer: {ignoresIceRestart: true}}, async ({a, b, network}) => {
      await Promise.all([a.setTimeouts({reconnectTimeout: 10000}), b.setTimeouts({reconnectTimeout: 10000})])
      network.changePath()
      await Promise.all([a.waitForEnded(DISCONNECT_DETECTION + 15000), b.waitForEnded(DISCONNECT_DETECTION + 15000)])
      assert.ok(b.sessionsSent() > 0, "offerer did not restart ICE")
      assert.equal(a.sessionsSent(), 0)
    })
  )
})
