package chat.simplex.app.ui.harness

import java.io.File
import java.net.InetAddress
import java.net.ServerSocket

object UiBridge {
  private val peers = HashMap<String, CliPeer>()
  private val peerDirs = HashMap<String, File>()
  private val sleepWaiter = Waiter { timeoutMs, what, cond ->
    val deadline = System.currentTimeMillis() + timeoutMs
    while (!cond()) {
      if (System.currentTimeMillis() > deadline) throw AssertionError("timed out after $timeoutMs ms waiting for $what")
      Thread.sleep(20)
    }
  }

  @JvmStatic fun main(args: Array<String>) {
    val port = args.getOrNull(0)?.toInt() ?: 7777
    UiJvm.setup()
    TestServers.start()
    val dir = File(UiEnv.jvmDir, "bridge-peers").apply { mkdirs() }
    println("uitest-bridge: smp ${TestServers.smp1}, port base ${TestServers.portBase}, listening on 127.0.0.1:$port, pid ${ProcessHandle.current().pid()}")
    ServerSocket(port, 1, InetAddress.getLoopbackAddress()).use { server ->
      while (true) {
        server.accept().use { socket ->
          val input = socket.getInputStream().bufferedReader(Charsets.UTF_8)
          val out = socket.getOutputStream().bufferedWriter(Charsets.UTF_8)
          runCatching {
            while (true) {
              val line = input.readLine() ?: break
              val reply = runCatching { handle(line, dir) }.getOrElse { listOf("err " + (it.message ?: it.toString()).replace("\n", " ")) }
              reply.forEach { out.write(it); out.write("\n") }
              out.flush()
            }
          }
          Peers.stopAll()
          peers.clear()
          peerDirs.clear()
        }
      }
    }
  }

  private fun handle(line: String, dir: File): List<String> {
    val parts = line.split(" ", limit = 3)
    return when (parts[0]) {
      "servers" -> listOf("ok ${TestServers.smp1} ${TestServers.xftp1}")
      "start" -> {
        val name = parts[1]
        val f = parts.getOrElse(2) { "" }.split(",").toSet()
        val peer = CliPeer(name, peerDirs.getOrPut(name) { File(dir, "${System.nanoTime()}-$name") }, TestServers.smp1, PeerFlags("reactions" in f, "markRead" in f, "fullLinks" in f), sleepWaiter)
        Peers.add(peer); peers[name] = peer
        peer.start(); peer.awaitStart()
        listOf("ok")
      }
      "type" -> { peers.getValue(parts[1]).types(parts.getOrElse(2) { "" }); listOf("ok") }
      "file" -> {
        val (fileName, b64) = parts.getOrElse(2) { "" }.split(" ", limit = 2)
        listOf(peers.getValue(parts[1]).createFile(fileName, String(java.util.Base64.getDecoder().decode(b64), Charsets.UTF_8)))
      }
      "drain" -> { val ls = peers.getValue(parts[1]).drainLines(); listOf("lines ${ls.size}") + ls }
      "stop" -> { peers.remove(parts[1])?.stop(); listOf("ok") }
      "forget" -> { peers.remove(parts[1])?.stop(); peerDirs.remove(parts[1]); listOf("ok") }
      else -> listOf("err unknown command ${parts[0]}")
    }
  }
}
