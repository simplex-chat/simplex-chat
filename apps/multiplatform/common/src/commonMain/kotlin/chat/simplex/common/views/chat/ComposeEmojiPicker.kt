package chat.simplex.common.views.chat

import androidx.compose.runtime.*
import androidx.compose.ui.text.TextRange
import kotlin.math.min

@Composable
expect fun ComposeEmojiButton(pickerShown: MutableState<Boolean>, enabled: Boolean, onEmoji: (String) -> Unit)

fun ComposeMessage.insertingEmoji(emoji: String): ComposeMessage {
  val start = min(selection.min, text.length)
  val end = min(selection.max, text.length)
  return ComposeMessage(text.replaceRange(start, end, emoji), TextRange(start + emoji.length))
}
