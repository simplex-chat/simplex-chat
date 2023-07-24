package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatItem

val largeEmojiFont: TextStyle = TextStyle(fontSize = 48.sp)
val mediumEmojiFont: TextStyle = TextStyle(fontSize = 36.sp)

@Composable
fun EmojiItemView(chatItem: ChatItem, timedMessagesTTL: Int?) {
  Column(
    Modifier.padding(vertical = 8.dp, horizontal = 12.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    EmojiText(chatItem.content.text)
    CIMetaView(chatItem, timedMessagesTTL)
  }
}

@Composable
fun EmojiText(text: String) {
  val s = text.trim()
  Text(s, style = if (s.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont)
}

// https://stackoverflow.com/a/46279500
private const val emojiStr = "^(" +
    "(?:[\\u2700-\\u27bf]|" +
    "(?:[\\ud83c\\udde6-\\ud83c\\uddff]){2}|" +
    "[\\ud800\\udc00-\\uDBFF\\uDFFF]|[\\u2600-\\u26FF])[\\ufe0e\\ufe0f]?(?:[\\u0300-\\u036f\\ufe20-\\ufe23\\u20d0-\\u20f0]|[\\ud83c\\udffb-\\ud83c\\udfff])?" +
    "(?:\\u200d(?:[^\\ud800-\\udfff]|" +
    "(?:[\\ud83c\\udde6-\\ud83c\\uddff]){2}|" +
    "[\\ud800\\udc00-\\uDBFF\\uDFFF]|[\\u2600-\\u26FF])[\\ufe0e\\ufe0f]?(?:[\\u0300-\\u036f\\ufe20-\\ufe23\\u20d0-\\u20f0]|[\\ud83c\\udffb-\\ud83c\\udfff])?)*|" +
    "[\\u0023-\\u0039]\\ufe0f?\\u20e3|\\u3299|\\u3297|\\u303d|\\u3030|\\u24c2|[\\ud83c\\udd70-\\ud83c\\udd71]|[\\ud83c\\udd7e-\\ud83c\\udd7f]|\\ud83c\\udd8e|[\\ud83c\\udd91-\\ud83c\\udd9a]|[\\ud83c\\udde6-\\ud83c\\uddff]|[\\ud83c\\ude01-\\ud83c\\ude02]|\\ud83c\\ude1a|\\ud83c\\ude2f|[\\ud83c\\ude32-\\ud83c\\ude3a]|[\\ud83c\\ude50-\\ud83c\\ude51]|\\u203c|\\u2049|[\\u25aa-\\u25ab]|\\u25b6|\\u25c0|[\\u25fb-\\u25fe]|\\u00a9|\\u00ae|\\u2122|\\u2139|\\ud83c\\udc04|[\\u2600-\\u26FF]|\\u2b05|\\u2b06|\\u2b07|\\u2b1b|\\u2b1c|\\u2b50|\\u2b55|\\u231a|\\u231b|\\u2328|\\u23cf|[\\u23e9-\\u23f3]|[\\u23f8-\\u23fa]|\\ud83c\\udccf|\\u2934|\\u2935|[\\u2190-\\u21ff]" +
    ")+$" // Multiple matches with emojis where one follows another without interruptions from other characters
private val emojiRegex = Regex(emojiStr)

fun isShortEmoji(str: String): Boolean {
  val s = str.trim()
  return s.codePoints().count() in 1..5 && emojiRegex.matches(str)
}
