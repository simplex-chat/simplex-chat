package chat.simplex.app

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.size
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.withStyle
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.item.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ChatEventTextTest {
  private val event = ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(1, Clock.System.now(), "", CIStatus.RcvRead()), CIContent.RcvDirectE2EEInfo(E2EEInfo(pqEnabled = false)), reactions = listOf())

  private fun separatorAndTime(terminal: Boolean): Pair<AnnotatedString, AnnotatedString> {
    var result: Pair<AnnotatedString, AnnotatedString>? = null
    renderScene {
      SimpleXTheme {
        CompositionLocalProvider(LocalTerminalLayout provides terminal) {
          ChatEventWithMeta(event) { separator, ts ->
            result = separator to ts
            Box(Modifier.size(1.dp))
          }
        }
      }
    }
    return result!!
  }

  @Test
  fun classicEventTextUnchanged() {
    val (separator, ts) = separatorAndTime(terminal = false)
    assertEquals(AnnotatedString("  "), separator)
    assertEquals(AnnotatedString(event.timestampText), ts)
    val base = buildAnnotatedString { withStyle(chatEventStyle) { append("alice joined the group  ${event.timestampText}") } }
    assertEquals(base, chatEventText("alice joined the group", separator + ts))
  }

  @Test
  fun terminalSeparatorJoinsCopyByNoBreakSpaces() {
    val (separator, ts) = separatorAndTime(terminal = true)
    assertEquals("\u00A0\u00A0\u00A0", separator.text)
    assertEquals(listOf(chatEventStyle, reserveTimestampStyle), separator.spanStyles.map { it.item })
    assertTrue(ts.text.startsWith("\u2060\u200E") && ts.text.endsWith(event.timestampText.replace(' ', '\u00A0')), ts.text)
  }
}
