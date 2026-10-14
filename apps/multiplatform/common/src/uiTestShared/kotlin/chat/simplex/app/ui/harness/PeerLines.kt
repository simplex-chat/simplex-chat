package chat.simplex.app.ui.harness

fun interface Waiter {
  fun waitUntil(timeoutMs: Long, what: String, cond: () -> Boolean)
}

data class PeerFlags(val reactions: Boolean = false, val markRead: Boolean = false, val fullLinks: Boolean = false)

abstract class PeerLines(val name: String, protected val waiter: Waiter) {
  protected val lines = ArrayList<String>()

  protected abstract fun fill()

  protected abstract fun type(text: String)

  infix fun sends(cmd: String) {
    type(cmd)
    await(LINE_TIMEOUT_MS, "$name: echo of `$cmd`") { takeFirst { it == cmd || stripTime(it) == cmd } != null }
  }

  infix fun types(text: String) = type(text)

  infix fun expects(line: String) {
    val got = nextLine()
    if (got != line) throw AssertionError("$name: expected `$line`, got `$got`; next: ${pending()}")
  }

  infix fun expectsMessage(line: String) {
    val got = nextLine()
    if (stripTime(got) != line) throw AssertionError("$name: expected message `$line`, got `$got`; next: ${pending()}")
  }

  fun eventually(line: String, timeoutMs: Long = LINE_TIMEOUT_MS) {
    await(timeoutMs, "$name: `$line`") { synchronized(lines) { lines.contains(line) } }
    synchronized(lines) { val i = lines.indexOf(line); repeat(i + 1) { lines.removeAt(0) } }
  }

  fun eventuallyMessage(line: String, timeoutMs: Long = LINE_TIMEOUT_MS) {
    await(timeoutMs, "$name: message `$line`") { synchronized(lines) { lines.any { stripTime(it) == line } } }
    synchronized(lines) { val i = lines.indexOfFirst { stripTime(it) == line }; repeat(i + 1) { lines.removeAt(0) } }
  }

  fun eventuallyMatching(regex: Regex, timeoutMs: Long = LINE_TIMEOUT_MS): MatchResult {
    await(timeoutMs, "$name: a line matching `$regex`") { synchronized(lines) { lines.any { regex.matches(it) } } }
    return synchronized(lines) {
      val i = lines.indexOfFirst { regex.matches(it) }
      regex.matchEntire(lines[i])!!.also { repeat(i + 1) { lines.removeAt(0) } }
    }
  }

  fun nextLine(timeoutMs: Long = LINE_TIMEOUT_MS): String {
    await(timeoutMs, "$name: next output line") { synchronized(lines) { lines.isNotEmpty() } }
    return synchronized(lines) { lines.removeAt(0) }
  }

  fun contactId(contact: String): Long {
    sends("/i @$contact")
    var id: Long? = null
    await(LINE_TIMEOUT_MS, "$name: contact ID of $contact") {
      synchronized(lines) {
        val i = lines.indexOfFirst { it.startsWith("contact ID: ") }
        if (i >= 0) {
          id = lines[i].removePrefix("contact ID: ").trim().toLong()
          val end = (i until lines.size).firstOrNull { lines[it].startsWith("peer chat protocol version range") } ?: (lines.size - 1)
          repeat(end - i + 1) { lines.removeAt(i) }
          true
        } else false
      }
    }
    return id!!
  }

  fun pending(): List<String> = synchronized(lines) { lines.take(10) }

  protected fun await(timeoutMs: Long, what: String, cond: () -> Boolean) =
    try {
      waiter.waitUntil(timeoutMs, what) { fill(); cond() }
    } catch (e: AssertionError) {
      throw AssertionError("${e.message}; $name output: ${pending()}", e)
    }

  protected fun stripTime(line: String): String = TIME_PREFIX.replaceFirst(line, "")

  private fun takeFirst(pred: (String) -> Boolean): String? = synchronized(lines) {
    val i = lines.indexOfFirst(pred)
    if (i >= 0) lines.removeAt(i) else null
  }

  companion object {
    const val LINE_TIMEOUT_MS = 15_000L
    private val TIME_PREFIX = Regex("^\\d{2}:\\d{2} ")
  }
}
