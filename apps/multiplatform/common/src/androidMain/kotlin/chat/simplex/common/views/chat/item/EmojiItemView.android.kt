package chat.simplex.common.views.chat.item

import androidx.compose.material.Text
import androidx.compose.runtime.Composable

@Composable
actual fun EmojiText(text: String) {
  val s = text.trim()
  Text(s, style = if (s.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont)
}
