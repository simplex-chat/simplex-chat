package chat.simplex.app.ui.harness

import java.io.BufferedReader
import java.io.BufferedWriter
import java.net.Socket

object Bridge {
  private const val PORT = 7777
  private const val READ_TIMEOUT_MS = 60_000
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
      val s = Socket("127.0.0.1", PORT).apply { soTimeout = READ_TIMEOUT_MS }
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
}

class CliPeer(name: String, private val flags: PeerFlags, waiter: Waiter) : PeerLines(name, waiter) {
  fun start() {
    val f = listOfNotNull("reactions".takeIf { flags.reactions }, "markRead".takeIf { flags.markRead }, "fullLinks".takeIf { flags.fullLinks })
    Bridge.call("start $name ${f.joinToString(",")}")
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
