const {test} = require("node:test")
const assert = require("node:assert/strict")
const {startCall, sleep} = require("./harness")

const END_DELIVERY = 8000
const PEER_CLOSE = 2000
const DISCONNECT_DETECTION = 10000

async function withCall(run) {
  const call = await startCall()
  try {
    await run(call)
  } finally {
    await call.close()
  }
}

async function waitClosed(peer, timeout) {
  const deadline = Date.now() + timeout
  while (!(await peer.connectionClosed())) {
    assert.ok(Date.now() < deadline, `${peer.name}'s connection is not closed in ${timeout}ms`)
    await sleep(100)
  }
}

test("call hang-up", {concurrency: true}, async (t) => {
  await t.test("the peer does not reconnect while the end of the call is delivered", () =>
    withCall(async ({a, b}) => {
      await a.setTimeouts({peerCloseTimeout: END_DELIVERY + 20000})
      await b.setTimeouts({peerCloseTimeout: PEER_CLOSE})
      await a.keepConnection()
      const from = b.messages.length
      await a.command({type: "end"})
      await sleep(1000)
      const received = await b.receivedBytes()
      await sleep(END_DELIVERY - 1000)
      assert.equal(await b.receivedBytes(), received, "A sends media after ending the call")
      const states = b.connectionStates().slice(from)
      assert.ok(!states.includes("reconnecting") && !states.includes("disconnected"), `B states: ${states}`)
      assert.ok(!b.ended(), "B's call ended before the end of the call was delivered")
      assert.ok(!(await a.connectionClosed()), "A closed the connection before B ended the call")
      await b.command({type: "end"})
      await waitClosed(a, PEER_CLOSE + DISCONNECT_DETECTION)
    })
  )

  await t.test("closes the connection when the peer does not end the call", () =>
    withCall(async ({a}) => {
      await a.setTimeouts({peerCloseTimeout: PEER_CLOSE})
      await a.keepConnection()
      await a.command({type: "end"})
      await sleep(PEER_CLOSE / 2)
      assert.ok(!(await a.connectionClosed()), "A closed the connection before the timeout")
      await waitClosed(a, PEER_CLOSE)
    })
  )

  await t.test("closes the connection at once when the network is down", () =>
    withCall(async ({a, network}) => {
      await a.keepConnection()
      network.down = true
      await a.waitForState("reconnecting", DISCONNECT_DETECTION)
      await a.command({type: "end"})
      await waitClosed(a, 500)
    })
  )
})
