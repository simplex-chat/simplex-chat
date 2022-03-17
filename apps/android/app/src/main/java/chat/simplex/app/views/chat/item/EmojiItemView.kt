package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.ChatItem

val largeEmojiFont: TextStyle = TextStyle(fontSize = 48.sp)
val mediumEmojiFont: TextStyle = TextStyle(fontSize = 36.sp)

@Composable
fun EmojiItemView(chatItem: ChatItem) {
  Column(
    Modifier.padding(vertical = 8.dp, horizontal = 12.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    EmojiText(chatItem.content.text)
    CIMetaView(chatItem)
  }
}

@Composable
fun EmojiText(text: String) {
  val s = text.trim()
  Text(s, style = if (s.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont)
}

private fun isSimpleEmoji(c: Int): Boolean = c > 0x238C
//  guard let firstScalar = c.unicodeScalars.first else { return false }
//  return firstScalar.properties.isEmoji && firstScalar.value > 0x238C
//}

//private fun isCombinedIntoEmoji(c: Char): Boolean =
//  c.unicodeScalars.count > 1 && c.unicodeScalars.first?.properties.isEmoji ?? false

fun isEmoji(c: Int): Boolean = isSimpleEmoji(c) // || isCombinedIntoEmoji(c)

// TODO count perceived emojis, possibly using icu4j
fun isShortEmoji(str: String): Boolean {
  val s = str.trim()
  return s.codePoints().count() in 1..5 && s.codePoints().allMatch(::isEmoji)
}
