package chat.simplex.app

import chat.simplex.common.views.call.*
import dev.onvoid.webrtc.RTCSdpType
import dev.onvoid.webrtc.RTCSessionDescription
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.serialization.Serializable
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json
import java.net.DatagramPacket
import java.net.DatagramSocket
import java.net.InetSocketAddress
import java.net.SocketAddress
import java.net.SocketException
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import kotlin.concurrent.thread
import kotlin.test.*

class NativeCallEngineTest {
  @Serializable
  private data class Vector(val input: String, val compressed: String)

  // The last two lzstring.json vectors are a session description and ICE candidates serialized as call.ts does
  private val vectors = Json.decodeFromString(ListSerializer(Vector.serializer()), javaClass.getResource("/call/lzstring.json")!!.readText())
  private val aesKey = "6r8EhZnO_fUPaa5xK92wY9qfCr2pk36gq5pgtJd2Ong="
  // On one host the peers connect directly, either side may see the other as peer-reflexive
  private val directConnections = setOf("peer-to-peer", "host / prflx", "prflx / host")

  companion object {
    private const val DISCONNECT_DETECTION_MS = 15_000L
    private const val SIGNALING_MS = 10_000L
    private const val RECONNECT_MS = 20_000L
    private const val RECONNECT_TIMEOUT_MS = 30_000L
    private const val UDP_BUFFER_SIZE = 65_536
  }

  @Test
  fun testParsesSessionDescriptionFromCallJs() {
    val d = NativeCallEngine.parseSdp(vectors[vectors.size - 2].compressed)
    assertEquals(RTCSdpType.OFFER, d.sdpType)
    assertTrue(d.sdp.startsWith("v=0\r\n"), d.sdp)
    assertTrue(d.sdp.contains("a=group:BUNDLE 0 1 2 3"), d.sdp)
  }

  @Test
  fun testSerializesSessionDescriptionForCallJs() {
    val serialized = NativeCallEngine.serializeSdp(RTCSessionDescription(RTCSdpType.ANSWER, "v=0\r\n"))
    assertEquals("""{"type":"answer","sdp":"v=0\r\n"}""", LZString.decompressFromBase64(serialized))
  }

  @Test
  fun testParsesIceCandidatesFromCallJs() {
    val candidates = NativeCallEngine.parseIceCandidates(vectors.last().compressed)
    assertEquals(listOf(IceCandidateJson("candidate:1 1 udp 2122260223 192.168.1.2 51000 typ host generation 0", "0", 0)), candidates)
  }

  @Test
  fun testTwoEnginesNegotiateEncryptedCall() = runBlocking<Unit> {
    val callerInbox = Inbox()
    val calleeInbox = Inbox()
    val caller = NativeCallEngine(callerInbox::post)
    val callee = NativeCallEngine(calleeInbox::post)
    try {
      withTimeout(30_000) {
        // The callee of the invitation creates the offer, as CallManager.justAcceptIncomingCall does
        callee.process(WCallCommand.Start(CallMediaType.Audio, aesKey, iceServers = emptyList(), relay = false))
        val offer = calleeInbox.awaitResponse<WCallResponse.Offer>()
        assertTrue(offer.capabilities.encryption)
        caller.process(WCallCommand.Offer(offer.offer, offer.iceCandidates, CallMediaType.Audio, aesKey, iceServers = emptyList(), relay = false))
        val answer = callerInbox.awaitResponse<WCallResponse.Answer>()
        callee.process(WCallCommand.Answer(answer.answer, answer.iceCandidates))
        calleeInbox.awaitResponse<WCallResponse.Ok>()
        val connected = listOf(
          async { calleeInbox.relayUntilConnected(caller) },
          async { callerInbox.relayUntilConnected(callee) }
        ).awaitAll()
        connected.forEach { info -> assertTrue(info.text in directConnections, info.text) }

        caller.process(WCallCommand.End)
        callerInbox.awaitResponse<WCallResponse.Ok>()
      }
    } finally {
      listOf(caller.close(), callee.close()).joinAll()
    }
  }

  @Test
  fun testRejectsOutOfOrderCommands() = runBlocking<Unit> {
    val inbox = Inbox()
    val engine = NativeCallEngine(inbox::post)
    try {
      withTimeout(30_000) {
        engine.process(WCallCommand.Answer("x", "y"))
        assertEquals(WCallResponse.Error("answer: call not started"), inbox.awaitResponse<WCallResponse.Error>())
        engine.process(WCallCommand.Ice(LZString.compressToBase64("[]")))
        assertEquals(WCallResponse.Error("ice: call not started yet, will add candidates later"), inbox.awaitResponse<WCallResponse.Error>())
        engine.process(WCallCommand.Start(CallMediaType.Audio, aesKey, iceServers = emptyList(), relay = false))
        val offer = inbox.awaitResponse<WCallResponse.Offer>()
        engine.process(WCallCommand.Offer(offer.offer, offer.iceCandidates, CallMediaType.Audio, aesKey, iceServers = emptyList(), relay = false))
        assertEquals(WCallResponse.Error("accept: call already started"), inbox.awaitResponse<WCallResponse.Error>())
      }
    } finally {
      engine.close().join()
    }
  }

  @Test
  fun testOffererRestartsIceAndCallRecovers() = runBlocking<Unit> {
    withRelayedCall { caller, callee, network ->
      network.down = true
      network.changePath()
      assertEquals("reconnecting", caller.nextState(DISCONNECT_DETECTION_MS))
      assertEquals("reconnecting", callee.nextState(DISCONNECT_DETECTION_MS))
      // The callee of the invitation sent the initial offer, so it restarts ICE and the caller answers
      assertEquals(RTCSdpType.OFFER, callee.nextSession(SIGNALING_MS))
      assertEquals(RTCSdpType.ANSWER, caller.nextSession(SIGNALING_MS))
      network.down = false
      assertEquals("connected", caller.nextState(RECONNECT_MS))
      assertEquals("connected", callee.nextState(RECONNECT_MS))
      assertTrue(network.newestRelayForwarded(), "media does not use the new path")
    }
  }

  @Test
  fun testEndsCallWhenReconnectionTimesOut() = runBlocking<Unit> {
    withRelayedCall { caller, callee, network ->
      network.down = true
      for (side in listOf(caller, callee)) {
        assertEquals("reconnecting", side.nextState(DISCONNECT_DETECTION_MS), side.name)
      }
      for (side in listOf(caller, callee)) {
        assertEquals("disconnected", side.nextState(RECONNECT_TIMEOUT_MS + SIGNALING_MS), side.name)
        assertEquals("ended", side.nextState(SIGNALING_MS), side.name)
      }
    }
  }

  @Test
  fun testRejectsUnexpectedIceRestartSessions() = runBlocking<Unit> {
    val callerInbox = Inbox()
    val calleeInbox = Inbox()
    val caller = NativeCallEngine(callerInbox::post)
    val callee = NativeCallEngine(calleeInbox::post)
    try {
      withTimeout(30_000) {
        callee.process(WCallCommand.Start(CallMediaType.Audio, aesKey, iceServers = emptyList(), relay = false))
        val offer = calleeInbox.awaitResponse<WCallResponse.Offer>()
        caller.process(WCallCommand.Offer(offer.offer, offer.iceCandidates, CallMediaType.Audio, aesKey, iceServers = emptyList(), relay = false))
        val answer = callerInbox.awaitResponse<WCallResponse.Answer>()
        callee.process(WCallCommand.Answer(answer.answer, answer.iceCandidates))
        calleeInbox.awaitResponse<WCallResponse.Ok>()
        val noCandidates = LZString.compressToBase64("[]")
        // the offerer restarts ICE, so it does not accept restart offers, and nobody accepts an answer without a restart offer
        callee.process(WCallCommand.Ice(noCandidates, offer.offer))
        assertEquals(WCallResponse.Error("ice: unexpected ICE restart offer in stable state"), calleeInbox.awaitResponse<WCallResponse.Error>())
        caller.process(WCallCommand.Ice(noCandidates, answer.answer))
        assertEquals(WCallResponse.Error("ice: unexpected ICE restart answer in stable state"), callerInbox.awaitResponse<WCallResponse.Error>())
      }
    } finally {
      listOf(caller.close(), callee.close()).joinAll()
    }
  }

  private class Side(val name: String) {
    val inbox = Inbox()
    val engine = NativeCallEngine(inbox::post)
    val states = Channel<String>(Channel.UNLIMITED)
    val sessions = Channel<RTCSdpType>(Channel.UNLIMITED)

    suspend fun nextState(timeoutMs: Long): String = withTimeout(timeoutMs) { states.receive() }

    suspend fun nextSession(timeoutMs: Long): RTCSdpType = withTimeout(timeoutMs) { sessions.receive() }
  }

  // A call where media can only flow through the test's UDP relays, as in packages/simplex-chat-webrtc/test/harness.js
  private suspend fun withRelayedCall(run: suspend (caller: Side, callee: Side, network: Network) -> Unit) = coroutineScope {
    val caller = Side("caller")
    val callee = Side("callee")
    val network = Network(listOf(caller, callee))
    try {
      withTimeout(30_000) {
        callee.engine.process(WCallCommand.Start(CallMediaType.Audio, aesKey, iceServers = emptyList(), relay = false))
        val offer = callee.inbox.awaitResponse<WCallResponse.Offer>()
        caller.engine.process(WCallCommand.Offer(offer.offer, network.route(callee, offer.iceCandidates), CallMediaType.Audio, aesKey, iceServers = emptyList(), relay = false))
        val answer = caller.inbox.awaitResponse<WCallResponse.Answer>()
        callee.engine.process(WCallCommand.Answer(answer.answer, network.route(caller, answer.iceCandidates)))
        callee.inbox.awaitResponse<WCallResponse.Ok>()
      }
      val relays = listOf(launch { caller.inbox.relay(caller, callee, network) }, launch { callee.inbox.relay(callee, caller, network) })
      for (side in listOf(caller, callee)) {
        withTimeout(30_000) { while (side.states.receive() != "connected") continue }
      }
      run(caller, callee, network)
      relays.forEach { it.cancel() }
    } finally {
      listOf(caller.engine.close(), callee.engine.close()).joinAll()
      network.close()
    }
  }

  // Each relay connects one ICE generation of both sides, and each side is told that the other side is at the relay socket
  private class Relay(private val network: Network, sides: List<Side>, ip: String) {
    val ufrags = HashMap<Side, String>()
    val real = ConcurrentHashMap<Side, SocketAddress>()
    val sockets = sides.associateWith { DatagramSocket(InetSocketAddress(ip, 0)) }
    val forwarded = AtomicInteger()
    @Volatile var dead = false

    init {
      for (side in sides) thread(isDaemon = true) { forward(receiver = side, sender = sides.first { it !== side }) }
    }

    // packets on the receiver's socket come from the sender, as this socket is advertised to the sender as the receiver's address
    private fun forward(receiver: Side, sender: Side) {
      val socket = sockets.getValue(receiver)
      val buffer = ByteArray(UDP_BUFFER_SIZE)
      while (true) {
        val packet = DatagramPacket(buffer, buffer.size)
        try {
          socket.receive(packet)
        } catch (e: SocketException) {
          return
        }
        if (dead || network.down) continue
        real[sender] = packet.socketAddress
        val to = real[receiver] ?: continue
        sockets.getValue(sender).send(DatagramPacket(packet.data, packet.length, to))
        forwarded.incrementAndGet()
      }
    }
  }

  private class Network(private val sides: List<Side>) : AutoCloseable {
    @Volatile var down = false
    private val relays = ArrayList<Relay>()

    // Routes the first IPv4 UDP host candidate of each ICE generation through a relay and drops the other candidates
    @Synchronized fun route(side: Side, iceCandidates: String): String {
      val routed = NativeCallEngine.parseIceCandidates(iceCandidates).mapNotNull { c ->
        val parts = c.candidate.split(" ")
        val ufrag = parts.getOrNull(parts.indexOf("ufrag") + 1)
        val relayable = parts[1] == "1" && parts[2].equals("udp", ignoreCase = true) && parts[7] == "host" && ':' !in parts[4]
        if (!relayable || ufrag == null || relays.any { it.ufrags[side] == ufrag }) return@mapNotNull null
        val relay = relays.firstOrNull { side !in it.ufrags } ?: Relay(this, sides, parts[4]).also(relays::add)
        relay.ufrags[side] = ufrag
        relay.real[side] = InetSocketAddress(parts[4], parts[5].toInt())
        c.copy(candidate = parts.toMutableList().also { it[5] = relay.sockets.getValue(side).localPort.toString() }.joinToString(" "))
      }
      return LZString.compressToBase64(Json.encodeToString(ListSerializer(IceCandidateJson.serializer()), routed))
    }

    // Addresses of all current ICE generations become unreachable, as when a device switches networks
    @Synchronized fun changePath() {
      relays.forEach { it.dead = true }
    }

    @Synchronized fun newestRelayForwarded(): Boolean = relays.last().let { !it.dead && it.forwarded.get() > 0 }

    @Synchronized override fun close() {
      relays.forEach { r -> r.sockets.values.forEach { it.close() } }
    }
  }

  // Command responses carry the command; ICE, connection state and peer media notifications don't and are kept for the relay
  private class Inbox {
    private val channel = Channel<WVAPIMessage>(Channel.UNLIMITED)
    private val notifications = ArrayDeque<WVAPIMessage>()

    fun post(msg: WVAPIMessage) {
      channel.trySend(msg)
    }

    suspend inline fun <reified T : WCallResponse> awaitResponse(): T {
      while (true) {
        val msg = channel.receive()
        if (msg.command == null) notifications.addLast(msg) else return assertIs<T>(msg.resp, "response to ${msg.command}")
      }
    }

    // Forwards ICE candidates and restart sessions to the peer through the network and records connection states
    suspend fun relay(side: Side, peer: Side, network: Network) {
      while (true) {
        val msg = notifications.removeFirstOrNull() ?: channel.receive()
        when (val resp = msg.resp) {
          is WCallResponse.Ice -> {
            resp.session?.let { side.sessions.send(NativeCallEngine.parseSdp(it).sdpType) }
            peer.engine.process(WCallCommand.Ice(network.route(side, resp.iceCandidates), resp.session))
          }
          is WCallResponse.Connection -> side.states.send(resp.state.connectionState)
          is WCallResponse.Ended -> side.states.send("ended")
          else -> {}
        }
      }
    }

    // Forwards ICE candidates to the peer and returns the connection info once this side reports a connected call
    suspend fun relayUntilConnected(peer: NativeCallEngine): ConnectionInfo {
      var connectionReported = false
      while (true) {
        val msg = notifications.removeFirstOrNull() ?: channel.receive()
        when (val resp = msg.resp) {
          is WCallResponse.Ice -> peer.process(WCallCommand.Ice(resp.iceCandidates))
          is WCallResponse.Connection -> if (resp.state.connectionState == "connected") connectionReported = true
          is WCallResponse.Connected -> {
            assertTrue(connectionReported, "connection state is reported before connection info")
            return resp.connectionInfo
          }
          is WCallResponse.Ended -> fail("call ended before connecting")
          else -> {}
        }
      }
    }
  }
}
