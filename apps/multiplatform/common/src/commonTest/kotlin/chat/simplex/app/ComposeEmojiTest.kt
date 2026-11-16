package chat.simplex.app

import androidx.compose.ui.text.TextRange
import chat.simplex.common.views.chat.ComposeMessage
import chat.simplex.common.views.chat.insertingEmoji
import kotlin.test.Test
import kotlin.test.assertEquals

// use this command for testing:
// ./gradlew desktopTest
class ComposeEmojiTest {
  @Test
  fun testInsertIntoEmptyMessage() {
    assertEquals(ComposeMessage("😀", TextRange(2)), ComposeMessage().insertingEmoji("😀"))
  }

  @Test
  fun testInsertAtCursor() {
    assertEquals(ComposeMessage("hi😀 there", TextRange(4)), ComposeMessage("hi there", TextRange(2)).insertingEmoji("😀"))
    assertEquals(ComposeMessage("😀hi", TextRange(2)), ComposeMessage("hi", TextRange(0)).insertingEmoji("😀"))
    assertEquals(ComposeMessage("hi👍", TextRange(4)), ComposeMessage("hi").insertingEmoji("👍"))
  }

  @Test
  fun testReplaceSelection() {
    assertEquals(ComposeMessage("say ❤️!", TextRange(6)), ComposeMessage("say love!", TextRange(4, 8)).insertingEmoji("❤️"))
    // selection made right to left has start > end
    assertEquals(ComposeMessage("say ❤️!", TextRange(6)), ComposeMessage("say love!", TextRange(8, 4)).insertingEmoji("❤️"))
  }

  @Test
  fun testInsertAfterEmoji() {
    val flag = "🏴󠁧󠁢󠁷󠁬󠁳󠁿"
    val withFlag = ComposeMessage(flag)
    assertEquals(ComposeMessage(flag + "👩‍💻", TextRange(flag.length + "👩‍💻".length)), withFlag.insertingEmoji("👩‍💻"))
  }

  @Test
  fun testSelectionBeyondTextIsClamped() {
    assertEquals(ComposeMessage("ab😀", TextRange(4)), ComposeMessage("ab", TextRange(5)).insertingEmoji("😀"))
  }
}
