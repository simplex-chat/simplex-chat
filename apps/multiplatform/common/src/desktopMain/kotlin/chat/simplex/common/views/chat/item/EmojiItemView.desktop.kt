package chat.simplex.common.views.chat.item

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.MREmojiChar
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
actual fun EmojiText(text: String) {
  val s = text.trim()
  if (desktopPlatform.isMac() && isHeartEmoji(s)) {
    Image(painterResource(MR.images.ic_heart), null, Modifier.height(62.dp).width(54.dp).padding(vertical = 8.dp))
  } else {
    Text(s, style = if (s.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont)
  }
}

/** [MREmojiChar.Heart.value] */
fun isHeartEmoji(s: String): Boolean =
  (s.codePoints().count() == 2L && s.codePointAt(0) == 10084 && s.codePointAt(1) == 65039) ||
      s.codePoints().count() == 1L && s.codePointAt(0) == 10084
