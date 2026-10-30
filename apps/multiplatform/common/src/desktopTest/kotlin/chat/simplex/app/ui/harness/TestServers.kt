package chat.simplex.app.ui.harness

import java.io.File
import java.io.RandomAccessFile
import java.io.Writer
import java.nio.channels.FileChannel
import java.nio.channels.FileLock
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit

object TestServers {
  private const val SLOTS = 120
  const val SOCKS_PORT_OFFSET = 20
  private val BASELINE = setOf("smp1", "xftp1")
  private const val REPLY_TIMEOUT_S = 30L

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
  val remoteCtrlPort: Int get() = portBase + 90

  @Volatile private var proc: Process? = null
  private var commands: Writer? = null
  private var log: File? = null
  private val responses = LinkedBlockingQueue<String>()
  private val addresses = HashMap<String, String>()
  private val running = HashSet<String>()

  val smp1: String get() = address("smp1")
  val xftp1: String get() = address("xftp1")

  @Synchronized fun launch() {
    if (proc != null) return
    val dir = File(UiEnv.jvmDir, "servers").apply { mkdirs() }
    val p = ProcessBuilder("setpriv", "--pdeathsig", "KILL", UiEnv.servers, dir.absolutePath, portBase.toString())
      .directory(UiEnv.repoRoot).redirectErrorStream(true).start()
    proc = p
    commands = p.outputStream.bufferedWriter(Charsets.UTF_8)
    val logFile = File(dir, "servers.log").also { log = it }
    Thread({
      p.inputStream.bufferedReader().forEachLine { line ->
        logFile.appendText("$line\n")
        if (line.startsWith("ready ") || line.startsWith("stopped ") || line.startsWith("error ")) responses.add(line)
      }
    }, "uitest-servers").apply { isDaemon = true; start() }
    for (name in BASELINE) ready(name, reply(name))
  }

  @Synchronized fun start(name: String): String = ready(name, command("start", name))

  @Synchronized fun stop(name: String) {
    val r = command("stop", name)
    check(r == "stopped $name") { "uitest-servers answered `$r` to `stop $name`; see $log" }
    running -= name
  }

  @Synchronized fun reset(name: String): String = ready(name, command("reset", name))

  @Synchronized fun address(name: String): String =
    addresses[name]?.takeIf { name in running } ?: error("server $name is not running; TestServers.start(\"$name\") starts it")

  @Synchronized fun restoreBaseline() {
    launch()
    (running - BASELINE).forEach { stop(it) }
    (BASELINE - running).forEach { start(it) }
  }

  val socksPort: Int get() = portBase + SOCKS_PORT_OFFSET

  fun configure() = configureServers(smp1, xftp1)

  fun shutdown() {
    val p = proc ?: return
    proc = null
    runCatching { p.outputStream.close() }
    if (!p.waitFor(10, TimeUnit.SECONDS)) p.destroyForcibly()
  }

  private fun command(cmd: String, name: String): String {
    launch()
    val w = commands ?: error("uitest-servers is not running")
    responses.clear()
    w.write("$cmd $name\n")
    w.flush()
    return reply(name)
  }

  private fun reply(name: String): String {
    val r = responses.poll(REPLY_TIMEOUT_S, TimeUnit.SECONDS) ?: error("uitest-servers did not answer for $name in $REPLY_TIMEOUT_S s; see $log")
    check(!r.startsWith("error ")) { "uitest-servers: $r; see $log" }
    return r
  }

  private fun ready(name: String, reply: String): String {
    check(reply.startsWith("ready $name ")) { "uitest-servers answered `$reply` for $name; see $log" }
    return reply.removePrefix("ready $name ").also { addresses[name] = it; running += name }
  }
}
