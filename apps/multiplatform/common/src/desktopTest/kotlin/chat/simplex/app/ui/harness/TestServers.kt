package chat.simplex.app.ui.harness

import java.io.File
import java.io.RandomAccessFile
import java.nio.channels.FileChannel
import java.nio.channels.FileLock
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit

object TestServers {
  private const val SLOTS = 120
  private const val KEY_HASH = "LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI="

  private class SlotLock(val index: Int, val channel: FileChannel, val lock: FileLock)

  private val slotLock: SlotLock by lazy {
    UiEnv.runDir.mkdirs()
    (0 until SLOTS).firstNotNullOfOrNull { k ->
      val ch = RandomAccessFile(File(UiEnv.runDir, "slot-$k.lock"), "rw").channel
      val lock = ch.tryLock()
      if (lock != null) SlotLock(k, ch, lock) else { ch.close(); null }
    } ?: error("no free port slot in ${UiEnv.runDir}")
  }
  val slot: Int get() = slotLock.index
  val portBase: Int get() = 20000 + 100 * slot

  private var proc: Process? = null
  private val responses = LinkedBlockingQueue<String>()
  lateinit var smp1: String
    private set
  val xftp1: String get() = "xftp://$KEY_HASH@localhost:${portBase + 2}"

  fun start() {
    if (proc != null) return
    val dir = File(UiEnv.jvmDir, "servers").apply { mkdirs() }
    val p = ProcessBuilder("setpriv", "--pdeathsig", "KILL", UiEnv.servers, dir.absolutePath, portBase.toString())
      .directory(UiEnv.repoRoot).redirectErrorStream(true).start()
    proc = p
    val log = File(dir, "servers.log")
    Thread({
      p.inputStream.bufferedReader().forEachLine { line ->
        log.appendText("$line\n")
        if (line.startsWith("ready ") || line.startsWith("error ")) responses.add(line)
      }
    }, "uitest-servers").apply { isDaemon = true; start() }
    val r = responses.poll(30, TimeUnit.SECONDS) ?: error("uitest-servers did not start; see $log")
    check(r.startsWith("ready smp1 ")) { "uitest-servers: $r" }
    smp1 = r.removePrefix("ready smp1 ")
  }

  fun configure() = configureServers(smp1, xftp1)

  fun stop() {
    val p = proc ?: return
    proc = null
    runCatching { p.outputStream.close() }
    if (!p.waitFor(10, TimeUnit.SECONDS)) p.destroyForcibly()
  }
}
