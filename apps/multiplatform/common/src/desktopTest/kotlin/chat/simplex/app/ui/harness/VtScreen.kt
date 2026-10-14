package chat.simplex.app.ui.harness

class VtScreen(private val width: Int, private val height: Int, private val onLine: (String) -> Unit) {
  private val rows = Array(height) { IntArray(width) { ' '.code } }
  private val continued = BooleanArray(height)
  private var row = 0
  private var col = 0
  private var wrapPending = false

  private enum class State { TEXT, ESC, CSI, OSC, OSC_ESC }
  private var state = State.TEXT
  private val csi = StringBuilder()
  private var highSurrogate: Char? = null

  @Synchronized fun feed(text: String) {
    for (ch in text) {
      when (state) {
        State.TEXT -> text(ch)
        State.ESC -> state = when (ch) {
          '[' -> { csi.setLength(0); State.CSI }
          ']' -> State.OSC
          else -> State.TEXT
        }
        State.CSI -> if (ch.code in 0x40..0x7e) { csi(csi.toString(), ch); state = State.TEXT } else csi.append(ch)
        State.OSC -> state = when (ch) { '\u0007' -> State.TEXT; '\u001b' -> State.OSC_ESC; else -> State.OSC }
        State.OSC_ESC -> state = State.TEXT
      }
    }
  }

  @Synchronized fun cursorRowText(): String = rowText(row)

  private fun text(ch: Char) {
    when {
      ch == '\u001b' -> state = State.ESC
      ch == '\r' -> { col = 0; wrapPending = false }
      ch == '\n' -> { onLine(logicalLine(row)); lineFeed() }
      ch == '\b' -> { if (col > 0) col--; wrapPending = false }
      ch.isHighSurrogate() -> highSurrogate = ch
      ch.isLowSurrogate() -> { highSurrogate?.let { put(Character.toCodePoint(it, ch)) }; highSurrogate = null }
      ch.code < 0x20 || ch.code == 0x7f -> {}
      else -> put(ch.code)
    }
  }

  private fun put(cp: Int) {
    if (wrapPending) {
      lineFeed()
      continued[row] = true
      col = 0
      wrapPending = false
    }
    rows[row][col] = cp
    if (col == width - 1) wrapPending = true else col++
  }

  private fun lineFeed() {
    wrapPending = false
    if (row == height - 1) {
      for (r in 0 until height - 1) { rows[r] = rows[r + 1]; continued[r] = continued[r + 1] }
      rows[height - 1] = IntArray(width) { ' '.code }
      continued[height - 1] = false
    } else row++
  }

  private fun csi(params: String, final: Char) {
    if (params.startsWith("?")) return
    val p = params.split(';').map { it.toIntOrNull() }
    fun arg(i: Int, def: Int) = p.getOrNull(i)?.takeIf { it != 0 } ?: def
    wrapPending = false
    when (final) {
      'H', 'f' -> { row = (arg(0, 1) - 1).coerceIn(0, height - 1); col = (arg(1, 1) - 1).coerceIn(0, width - 1); continued[row] = false }
      'A' -> row = (row - arg(0, 1)).coerceAtLeast(0)
      'B' -> row = (row + arg(0, 1)).coerceAtMost(height - 1)
      'C' -> col = (col + arg(0, 1)).coerceAtMost(width - 1)
      'D' -> col = (col - arg(0, 1)).coerceAtLeast(0)
      'G' -> col = (arg(0, 1) - 1).coerceIn(0, width - 1)
      'K' -> when (p.getOrNull(0) ?: 0) {
        0 -> rows[row].fill(' '.code, col, width)
        1 -> rows[row].fill(' '.code, 0, col + 1)
        2 -> rows[row].fill(' '.code)
      }
      'J' -> when (p.getOrNull(0) ?: 0) {
        0 -> { rows[row].fill(' '.code, col, width); for (r in row + 1 until height) { rows[r].fill(' '.code); continued[r] = false } }
        1 -> { for (r in 0 until row) { rows[r].fill(' '.code); continued[r] = false }; rows[row].fill(' '.code, 0, col + 1) }
        2, 3 -> for (r in 0 until height) { rows[r].fill(' '.code); continued[r] = false }
      }
    }
  }

  private fun rowText(r: Int): String {
    val cps = rows[r]
    var end = width
    while (end > 0 && cps[end - 1] == ' '.code) end--
    return String(cps, 0, end)
  }

  private fun logicalLine(r: Int): String {
    var start = r
    while (start > 0 && continued[start]) start--
    return (start..r).joinToString("") { i -> if (i < r) String(rows[i], 0, width) else rowText(i) }.trimEnd()
  }
}
