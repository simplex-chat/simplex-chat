// Calls between two chat cores connected via a local SMP server, with the test playing the native app:
// it passes messages between call.js and the core API as the Android and desktop apps do.
const {test} = require("node:test")
const assert = require("node:assert/strict")
const fs = require("fs")
const os = require("os")
const path = require("path")
const {openPeers, sleep} = require("./harness")
const {startSmpServer, ChatClient, connectContacts} = require("./chat")

const {SIMPLEX_CHAT, SMP_SERVER} = process.env
const MEDIA = "audio"
const DISCONNECT_DETECTION = 10000
const RECONNECT = 20000
const SIGNALING = 15000
const CORE_CALL_STATUSES = ["connecting", "connected", "disconnected", "failed"]
const CALL_RECONNECT_VERSION = 22

async function withChatCall({useWorker = false}, run) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "simplex-call-test-"))
  const stops = []
  try {
    const smp = await startSmpServer(SMP_SERVER, path.join(dir, "smp"))
    stops.push(smp.stop)
    const alice = await ChatClient.start(SIMPLEX_CHAT, dir, "alice", smp.address)
    stops.push(() => alice.stop())
    const bob = await ChatClient.start(SIMPLEX_CHAT, dir, "bob", smp.address)
    stops.push(() => bob.stop())
    const [aliceContact, bobContact] = await connectContacts(alice, bob)
    const peers = await openPeers({useWorker})
    stops.push(peers.close)
    // alice calls bob: a is alice's call.js, b is bob's call.js
    const call = {...peers, alice, bob, aliceContact, bobContact}
    connectApp(call.a, alice, aliceContact, peers)
    connectApp(call.b, bob, bobContact, peers)
    await startCall(call)
    await run(call)
  } finally {
    for (const stop of stops.reverse()) await stop()
    fs.rmSync(dir, {recursive: true, force: true})
  }
}

function connectApp(peer, chat, contactId, {network, signaling}) {
  peer.onResponse = async (resp) => {
    switch (resp.type) {
      case "ice": {
        const {session, iceCandidates} = await network.routeSignal(peer.name, resp.session, resp.iceCandidates)
        const extraInfo = JSON.stringify({rtcSession: session, rtcIceCandidates: iceCandidates})
        signaling.send(() => chat.cmd(`/_call extra @${contactId} ${extraInfo}`))
        break
      }
      case "connection":
        if (CORE_CALL_STATUSES.includes(resp.state.connectionState)) {
          await chat.cmd(`/_call status @${contactId} ${resp.state.connectionState}`)
        }
        break
      case "ended":
        await chat.cmd(`/_call end @${contactId}`).catch(() => {})
        break
    }
  }
  chat.onEvent = async (event) => {
    switch (event.type) {
      case "callExtraInfo":
        await peer.command({type: "ice", iceCandidates: event.extraInfo.rtcIceCandidates, session: event.extraInfo.rtcSession})
        break
      case "callEnded":
        await peer.command({type: "end"})
        break
    }
  }
}

async function startCall({a, b, network, alice, bob, aliceContact, bobContact}) {
  const capabilities = await a.command({type: "capabilities", media: MEDIA})
  assert.equal(capabilities.resp.type, "capabilities")
  const callType = {media: MEDIA, capabilities: capabilities.resp.capabilities}
  await alice.cmd(`/_call invite @${aliceContact} ${JSON.stringify(callType)}`)

  const {callInvitation} = await bob.waitEvent((e) => e.type === "callInvitation", SIGNALING, "callInvitation")
  const offer = await b.command({
    type: "start",
    media: MEDIA,
    aesKey: callInvitation.sharedKey,
    iceServers: [],
    canReconnect: canReconnect(callInvitation.contact),
  })
  assert.equal(offer.resp.type, "offer", JSON.stringify(offer.resp))
  const offerSignal = await network.routeSignal("B", offer.resp.offer, offer.resp.iceCandidates)
  const callOffer = {
    callType: {media: MEDIA, capabilities: offer.resp.capabilities},
    rtcSession: {rtcSession: offerSignal.session, rtcIceCandidates: offerSignal.iceCandidates},
  }
  await bob.cmd(`/_call offer @${bobContact} ${JSON.stringify(callOffer)}`)

  const offered = await alice.waitEvent((e) => e.type === "callOffer", SIGNALING, "callOffer")
  const answer = await a.command({
    type: "offer",
    offer: offered.offer.rtcSession,
    iceCandidates: offered.offer.rtcIceCandidates,
    media: MEDIA,
    aesKey: offered.sharedKey,
    iceServers: [],
    canReconnect: canReconnect(offered.contact),
  })
  assert.equal(answer.resp.type, "answer", JSON.stringify(answer.resp))
  const answerSignal = await network.routeSignal("A", answer.resp.answer, answer.resp.iceCandidates)
  const callAnswer = {rtcSession: answerSignal.session, rtcIceCandidates: answerSignal.iceCandidates}
  await alice.cmd(`/_call answer @${aliceContact} ${JSON.stringify(callAnswer)}`)

  const answered = await bob.waitEvent((e) => e.type === "callAnswer", SIGNALING, "callAnswer")
  const ok = await b.command({type: "answer", answer: answered.answer.rtcSession, iceCandidates: answered.answer.rtcIceCandidates})
  assert.equal(ok.resp.type, "ok", JSON.stringify(ok.resp))
  await Promise.all([a.waitForState("connected", RECONNECT), b.waitForState("connected", RECONNECT)])
}

function canReconnect(contact) {
  return contact.activeConn.peerChatVRange.maxVersion >= CALL_RECONNECT_VERSION
}

async function assertAudioDecoded(a, b) {
  const before = [await a.receivedAudioEnergy(), await b.receivedAudioEnergy()]
  await sleep(2000)
  const after = [await a.receivedAudioEnergy(), await b.receivedAudioEnergy()]
  assert.ok(after[0] > before[0], `A decoded no audio: ${before[0]} -> ${after[0]}`)
  assert.ok(after[1] > before[1], `B decoded no audio: ${before[1]} -> ${after[1]}`)
}

async function callItem(chat, contactId) {
  const r = await chat.cmd(`/_get chat @${contactId} count=1`)
  return r.chat.chatItems[0].content
}

function restartSessionsReceived(chat) {
  return chat.events.filter((e) => e.type === "callExtraInfo" && e.extraInfo.rtcSession).length
}

const skip = !(SIMPLEX_CHAT && SMP_SERVER) && "set SIMPLEX_CHAT and SMP_SERVER to chat CLI and SMP server executables"

test("calls via chat core", {skip}, async (t) => {
  for (const [app, useWorker] of [
    ["android", false],
    ["desktop", true],
  ]) {
    await t.test(`${app}: encrypted call reconnects with ICE restart after network path change`, () =>
      withChatCall({useWorker}, async ({a, b, network, alice, bob, aliceContact, bobContact}) => {
        assert.ok(a.messages.find((m) => m.resp.type === "capabilities").resp.capabilities.encryption, "call is not encrypted")
        assert.equal(await a.page.evaluate(() => !!activeCall.worker), useWorker, "encryption worker")
        await assertAudioDecoded(a, b)
        assert.deepEqual(await callItem(alice, aliceContact), {type: "sndCall", status: "progress", duration: 0})
        const connectedAt = Date.now()

        const from = [a.messages.length, b.messages.length]
        network.down = true
        network.changePath()
        await Promise.all([
          a.waitForState("reconnecting", DISCONNECT_DETECTION, from[0]),
          b.waitForState("reconnecting", DISCONNECT_DETECTION, from[1]),
        ])
        await sleep(3000)
        network.down = false
        await Promise.all([a.waitForState("connected", RECONNECT, from[0]), b.waitForState("connected", RECONNECT, from[1])])
        await assertAudioDecoded(a, b)
        assert.ok(restartSessionsReceived(alice) > 0, "alice received no ICE restart offer")
        assert.ok(restartSessionsReceived(bob) > 0, "bob received no ICE restart answer")
        assert.equal((await callItem(alice, aliceContact)).status, "progress")
        assert.equal((await callItem(bob, bobContact)).status, "progress")

        await alice.cmd(`/_call end @${aliceContact}`)
        await a.command({type: "end"})
        await bob.waitEvent((e) => e.type === "callEnded", SIGNALING, "callEnded")
        const minDuration = Math.floor((Date.now() - connectedAt) / 1000) - 1
        const aliceItem = await callItem(alice, aliceContact)
        assert.equal(aliceItem.status, "ended")
        assert.ok(aliceItem.duration >= minDuration, `call duration ${aliceItem.duration} < ${minDuration}`)
      })
    )
  }

  await t.test("call ends in both chats when reconnection times out", () =>
    withChatCall({}, async ({a, b, network, alice, bob, aliceContact, bobContact}) => {
      await Promise.all([a.setTimeouts({reconnectTimeout: 5000}), b.setTimeouts({reconnectTimeout: 5000})])
      network.down = true
      // the first party to time out ends the call, and the other party ends it on "callEnded" event
      const endedBy = (peer, other) => peer.waitForEnded(DISCONNECT_DETECTION + 10000).then(() => other)
      const other = await Promise.any([endedBy(a, bob), endedBy(b, alice)])
      await other.waitEvent((e) => e.type === "callEnded", SIGNALING, "callEnded")
      network.down = false
      for (const [chat, contactId] of [
        [alice, aliceContact],
        [bob, bobContact],
      ]) {
        const item = await callItem(chat, contactId)
        assert.equal(item.status, "ended", `${chat.name} call item: ${JSON.stringify(item)}`)
        assert.ok(item.duration >= 5, `${chat.name} call duration ${item.duration}`)
      }
    })
  )
})
