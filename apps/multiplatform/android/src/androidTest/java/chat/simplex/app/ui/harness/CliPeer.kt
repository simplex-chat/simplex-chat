package chat.simplex.app.ui.harness

import java.io.BufferedReader
import java.io.BufferedWriter
import java.net.Socket

object Bridge {
  private val port = androidx.test.platform.app.InstrumentationRegistry.getArguments().let { it.getString("bridgePort") ?: it.getString("UITEST_BRIDGE_PORT") }?.toInt() ?: 7777
  private const val READ_TIMEOUT_MS = 150_000
  private var socket: Socket? = null
  private lateinit var input: BufferedReader
  private lateinit var output: BufferedWriter

  @Synchronized fun call(cmd: String): List<String> =
    try {
      exchange(cmd)
    } catch (e: java.io.IOException) {
      runCatching { socket?.close() }
      socket = null
      throw e
    }

  private fun exchange(cmd: String): List<String> {
    if (socket == null) {
      val s = Socket("127.0.0.1", port).apply { soTimeout = READ_TIMEOUT_MS }
      socket = s
      input = s.getInputStream().bufferedReader(Charsets.UTF_8)
      output = s.getOutputStream().bufferedWriter(Charsets.UTF_8)
    }
    output.write(cmd); output.write("\n"); output.flush()
    val head = input.readLine() ?: throw java.io.EOFException("bridge closed the connection (command: $cmd)")
    if (head.startsWith("err ")) throw AssertionError("bridge: ${head.removePrefix("err ")} (command: $cmd)")
    if (head.startsWith("lines ")) return List(head.removePrefix("lines ").toInt()) { input.readLine() }
    return listOf(head)
  }

  fun servers(): Pair<String, String> = call("servers").first().split(" ").let { it[1] to it[2] }

  fun remoteCtrlPort(): Int = call("remotePort").first().removePrefix("ok ").toInt()
}

object TestServers {
  fun start(name: String): String = Bridge.call("server start $name").first().removePrefix("ok ")

  fun stop(name: String) { Bridge.call("server stop $name") }

  fun reset(name: String): String = Bridge.call("server reset $name").first().removePrefix("ok ")

  fun address(name: String): String = Bridge.call("server address $name").first().removePrefix("ok ")

  fun restoreBaseline() { Bridge.call("server baseline") }
}

object SocksProxy {
  private var started: Int? = null

  val port: Int get() = started ?: error("the SOCKS proxy is not started; SocksProxy.start() starts it")

  fun start() {
    if (started == null) started = Bridge.call("socks start").first().removePrefix("ok ").toInt()
  }

  fun connections(): List<SocksConnection> =
    Bridge.call("socks connections").map { line ->
      val (host, port, username) = line.split(" ", limit = 3)
      SocksConnection(host, port.toInt(), username.takeIf { it != "-" })
    }

  fun stop() {
    if (started == null) return
    started = null
    Bridge.call("socks stop")
  }
}

class CliPeer(name: String, private val flags: PeerFlags, waiter: Waiter) : PeerLines(name, waiter) {
  private var relayLink: String? = null

  val relayAddress: String
    get() = relayLink ?: error("$name printed no chat relay address; start it with PeerFlags(relay = true) on a new database")

  fun start() {
    val f = listOfNotNull(
      "reactions".takeIf { flags.reactions }, "markRead".takeIf { flags.markRead }, "fullLinks".takeIf { flags.fullLinks }, "relay".takeIf { flags.relay },
      "smp=${flags.smpServer}".takeIf { flags.smpServer != PeerFlags().smpServer }, "xftp=${flags.xftpServer}".takeIf { flags.xftpServer != PeerFlags().xftpServer }
    )
    Bridge.call("start $name ${f.joinToString(",")}").first().removePrefix("ok").trim().takeIf { it.isNotEmpty() }?.let { relayLink = it }
  }

  fun stop() { runCatching { Bridge.call("stop $name") } }

  fun dispose() { runCatching { Bridge.call("forget $name") } }

  fun createFile(fileName: String, content: String): String =
    Bridge.call("file $name $fileName ${java.util.Base64.getEncoder().encodeToString(content.toByteArray(Charsets.UTF_8))}").first()

  override fun fill() {
    val ls = Bridge.call("drain $name")
    synchronized(lines) { lines.addAll(ls) }
  }

  override fun type(text: String) { Bridge.call("type $name $text") }
}
