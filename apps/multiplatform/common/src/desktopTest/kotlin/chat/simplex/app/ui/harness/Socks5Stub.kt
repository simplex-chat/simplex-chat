package chat.simplex.app.ui.harness

import java.io.DataInputStream
import java.io.InputStream
import java.io.OutputStream
import java.net.InetAddress
import java.net.InetSocketAddress
import java.net.ServerSocket
import java.net.Socket
import java.util.concurrent.ConcurrentHashMap

class Socks5Stub(val port: Int) : AutoCloseable {
  private val server = ServerSocket().apply { reuseAddress = true; bind(InetSocketAddress(InetAddress.getLoopbackAddress(), port)) }
  private val sockets: MutableSet<Socket> = ConcurrentHashMap.newKeySet()
  private val recorded = ArrayList<SocksConnection>()

  init {
    Thread({
      while (!server.isClosed) {
        val client = runCatching { server.accept() }.getOrNull() ?: break
        sockets += client
        Thread({ serve(client) }, "socks-stub-client").apply { isDaemon = true; start() }
      }
    }, "socks-stub-$port").apply { isDaemon = true; start() }
  }

  fun connections(): List<SocksConnection> = synchronized(recorded) { recorded.toList() }

  override fun close() {
    runCatching { server.close() }
    sockets.forEach { runCatching { it.close() } }
    sockets.clear()
  }

  private fun serve(client: Socket) {
    val target = runCatching { handshake(client) }.getOrNull()
    if (target == null) {
      runCatching { client.close() }
      sockets.remove(client)
      return
    }
    sockets += target
    Thread({ pump(target.getInputStream(), client.getOutputStream(), client, target) }, "socks-stub-down").apply { isDaemon = true; start() }
    pump(client.getInputStream(), target.getOutputStream(), client, target)
  }

  private fun handshake(client: Socket): Socket? {
    val input = DataInputStream(client.getInputStream())
    val out = client.getOutputStream()
    if (input.readUnsignedByte() != VERSION) return null
    val methods = ByteArray(input.readUnsignedByte()).also { input.readFully(it) }.map { it.toInt() and 0xFF }
    val username = when {
      AUTH_USERNAME in methods -> {
        out.write(byteArrayOf(VERSION.toByte(), AUTH_USERNAME.toByte())); out.flush()
        input.readUnsignedByte()
        val user = String(ByteArray(input.readUnsignedByte()).also { input.readFully(it) }, Charsets.UTF_8)
        input.readFully(ByteArray(input.readUnsignedByte()))
        out.write(byteArrayOf(1, 0)); out.flush()
        user
      }
      AUTH_NONE in methods -> { out.write(byteArrayOf(VERSION.toByte(), AUTH_NONE.toByte())); out.flush(); null }
      else -> { out.write(byteArrayOf(VERSION.toByte(), 0xFF.toByte())); out.flush(); return null }
    }
    if (input.readUnsignedByte() != VERSION) return null
    val command = input.readUnsignedByte()
    input.readUnsignedByte()
    val host = when (input.readUnsignedByte()) {
      ATYP_IPV4 -> InetAddress.getByAddress(ByteArray(4).also { input.readFully(it) }).hostAddress
      ATYP_DOMAIN -> String(ByteArray(input.readUnsignedByte()).also { input.readFully(it) }, Charsets.US_ASCII)
      ATYP_IPV6 -> InetAddress.getByAddress(ByteArray(16).also { input.readFully(it) }).hostAddress
      else -> return reply(out, REP_ADDRESS_TYPE)
    }
    val targetPort = input.readUnsignedShort()
    if (command != CMD_CONNECT) return reply(out, REP_COMMAND)
    synchronized(recorded) { recorded += SocksConnection(host, targetPort, username) }
    val target = runCatching { Socket().apply { connect(InetSocketAddress(host, targetPort), CONNECT_TIMEOUT_MS) } }.getOrNull()
      ?: return reply(out, REP_REFUSED)
    reply(out, REP_OK)
    return target
  }

  private fun pump(from: InputStream, to: OutputStream, a: Socket, b: Socket) {
    val buf = ByteArray(16384)
    try {
      while (true) {
        val n = from.read(buf)
        if (n < 0) break
        to.write(buf, 0, n)
        to.flush()
      }
    } catch (_: Exception) {
    } finally {
      for (s in listOf(a, b)) { runCatching { s.close() }; sockets.remove(s) }
    }
  }

  private fun reply(out: OutputStream, code: Int): Socket? {
    out.write(byteArrayOf(VERSION.toByte(), code.toByte(), 0, ATYP_IPV4.toByte(), 0, 0, 0, 0, 0, 0))
    out.flush()
    return null
  }

  private companion object {
    const val VERSION = 5
    const val AUTH_NONE = 0
    const val AUTH_USERNAME = 2
    const val CMD_CONNECT = 1
    const val ATYP_IPV4 = 1
    const val ATYP_DOMAIN = 3
    const val ATYP_IPV6 = 4
    const val REP_OK = 0
    const val REP_REFUSED = 5
    const val REP_COMMAND = 7
    const val REP_ADDRESS_TYPE = 8
    const val CONNECT_TIMEOUT_MS = 10_000
  }
}

object SocksProxy {
  private var stub: Socks5Stub? = null

  val port: Int get() = TestServers.socksPort

  @Synchronized fun start() {
    if (stub == null) stub = Socks5Stub(port)
  }

  @Synchronized fun connections(): List<SocksConnection> = stub?.connections() ?: error("the SOCKS proxy is not started; SocksProxy.start() starts it")

  @Synchronized fun stop() {
    stub?.close()
    stub = null
  }
}
