package chat.simplex.app.ui.harness

import com.pty4j.PtyProcess
import com.pty4j.PtyProcessBuilder
import java.io.File
import java.io.InputStreamReader
import java.util.concurrent.TimeUnit

class CliPeer(name: String, private val dir: File, private val flags: PeerFlags, waiter: Waiter) : PeerLines(name, waiter) {
  private val transcript = File(dir, "transcript.log")
  private lateinit var screen: VtScreen
  private var proc: PtyProcess? = null
  private var reader: Thread? = null

  fun createFile(fileName: String, content: String): String =
    File(dir, "files").apply { mkdirs() }.resolve(fileName).apply { writeText(content) }.absolutePath

  fun start() {
    File(dir, "tmp").mkdirs()
    if (!File(dir, "simplex_v1_chat.db").exists()) {
      File(UiEnv.runDir, "peer-template").listFiles { f -> f.name.startsWith("simplex_v1_") }?.forEach { it.copyTo(File(dir, it.name)) }
    }
    val args = mutableListOf(UiEnv.cli, "-d", File(dir, "simplex_v1").absolutePath, "--user-display-name", name, "-s", TestServers.address(flags.smpServer), "--xftp-server", TestServers.address(flags.xftpServer))
    if (flags.reactions) args += "--reactions"
    if (flags.markRead) args += "--mark-read"
    if (flags.fullLinks) args += "--show-full-links"
    if (flags.relay) args += "--relay"
    val cmd = arrayOf("setpriv", "--pdeathsig", "KILL", "sh", "-c", "stty -echo -icanon; exec \"\$0\" \"\$@\"") + args
    val env = System.getenv().toMutableMap().apply {
      put("HOME", dir.absolutePath); put("TMPDIR", File(dir, "tmp").absolutePath)
      put("LC_ALL", "C.UTF-8"); put("TERM", "xterm-256color")
    }
    synchronized(lines) { lines.clear() }
    screen = VtScreen(COLUMNS, ROWS) { line ->
      transcript.appendText("${System.currentTimeMillis() % 100_000_000} $line\n")
      synchronized(lines) { lines.add(line) }
    }
    val p = PtyProcessBuilder(cmd).setEnvironment(env).setDirectory(UiEnv.repoRoot.absolutePath)
      .setInitialColumns(COLUMNS).setInitialRows(ROWS).setConsole(false).start()
    proc = p
    reader = Thread({
      val r = InputStreamReader(p.inputStream, Charsets.UTF_8)
      val buf = CharArray(8192)
      try {
        while (true) { val n = r.read(buf); if (n < 0) break; screen.feed(String(buf, 0, n)) }
      } catch (_: Exception) {}
    }, "cli-$name").apply { isDaemon = true; start() }
    started = false
    relayPromptAnswered = false
    startDeadline = System.currentTimeMillis() + START_TIMEOUT_MS
  }

  private var started = true
  private var startDeadline = 0L
  private var relayPromptAnswered = false

  var relayLink: String? = null
    private set

  val relayAddress: String
    get() {
      ensureStarted()
      return relayLink ?: error("$name printed no chat relay address; start it with PeerFlags(relay = true) on a new database")
    }

  fun awaitStart() = ensureStarted()

  fun drainLines(): List<String> = synchronized(lines) { lines.toList().also { lines.clear() } }

  private fun ensureStarted() {
    if (started) return
    waiter.waitUntil((startDeadline - System.currentTimeMillis()).coerceAtLeast(0), "$name to start") {
      answerRelayAddressPrompt()
      synchronized(lines) { lines.any { it.startsWith("Current user: ") } } && promptShown()
    }
    synchronized(lines) {
      val i = lines.indexOfFirst { it.endsWith(RELAY_ADDRESS_CREATED) }
      if (i >= 0) relayLink = lines.getOrNull(i + 1)
      lines.clear()
    }
    started = true
  }

  fun stop() {
    val p = proc ?: return
    proc = null
    p.destroy()
    if (!p.waitFor(5, TimeUnit.SECONDS)) { p.destroyForcibly(); p.waitFor(5, TimeUnit.SECONDS) }
    runCatching { p.inputStream.close() }; runCatching { p.outputStream.close() }; runCatching { p.errorStream.close() }
    reader?.join(2000)
  }

  override fun fill() = ensureStarted()

  override fun type(text: String) {
    ensureStarted()
    typeVerified(text)
    enter(text)
  }

  private fun inputLine(): String = screen.cursorRowText().let { if (it.startsWith(PROMPT)) it.removePrefix(PROMPT) else it.removePrefix(">") }

  private fun promptShown(): Boolean = screen.cursorRowText().startsWith(">")

  private fun answerRelayAddressPrompt() {
    if (flags.relay && !relayPromptAnswered && screen.cursorRowText().startsWith(RELAY_ADDRESS_PROMPT)) {
      write("y\n")
      relayPromptAnswered = true
    }
  }

  private fun write(s: String) {
    val out = proc!!.outputStream
    out.write(s.toByteArray(Charsets.UTF_8)); out.flush()
  }

  private fun typeVerified(text: String) {
    var seen = ""
    repeat(TYPE_ATTEMPTS) {
      write(text)
      if (settles { inputLine() == text }) return
      seen = inputLine()
      write("\u007f".repeat(seen.length + 2))
      settles { inputLine().isEmpty() }
    }
    throw AssertionError("$name: could not type `$text`; the input line held `$seen`")
  }

  private fun enter(text: String) {
    write("\r")
    val retryAt = System.currentTimeMillis() + ENTER_RETRY_MS
    while (System.currentTimeMillis() < retryAt) {
      if (inputLine() != text) return
      Thread.sleep(10)
    }
    write("\r")
  }

  private fun settles(done: () -> Boolean): Boolean {
    var last = inputLine()
    var changedAt = System.currentTimeMillis()
    while (true) {
      if (done()) return true
      Thread.sleep(10)
      val now = inputLine()
      if (now != last) { last = now; changedAt = System.currentTimeMillis() }
      else if (System.currentTimeMillis() - changedAt > UNCHANGED_MS) return done()
    }
  }

  companion object {
    const val COLUMNS = 4000
    const val ROWS = 50
    private const val PROMPT = "> "
    private const val RELAY_ADDRESS_PROMPT = "Create relay address (Yn):"
    private const val RELAY_ADDRESS_CREATED = "Chat relay address is created:"
    private const val START_TIMEOUT_MS = 60_000L
    private const val TYPE_ATTEMPTS = 5
    private const val UNCHANGED_MS = 1_500L
    private const val ENTER_RETRY_MS = 500L
  }
}
