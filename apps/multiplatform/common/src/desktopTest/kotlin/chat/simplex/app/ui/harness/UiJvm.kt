package chat.simplex.app.ui.harness

import java.io.File

object UiJvm {
  fun setup() {
    val tmp = File(UiEnv.jvmDir, "tmp")
    check(tmp.mkdirs() || tmp.isDirectory) { "cannot create ${tmp.absolutePath}" }
    System.setProperty("user.home", UiEnv.jvmDir.absolutePath)
    System.setProperty("java.io.tmpdir", tmp.absolutePath)
    Runtime.getRuntime().addShutdownHook(Thread {
      runCatching { UiTestApp.stopCore() }
      Peers.stopAll()
      TestServers.stop()
      ProcessHandle.current().descendants().forEach { it.destroyForcibly() }
    })
  }
}

object Peers {
  private val running = mutableListOf<CliPeer>()
  @Synchronized fun add(p: CliPeer) { running += p }
  @Synchronized fun all(): List<CliPeer> = running.toList()
  @Synchronized fun stopAll() { running.forEach { runCatching { it.stop() } }; running.clear() }
}
