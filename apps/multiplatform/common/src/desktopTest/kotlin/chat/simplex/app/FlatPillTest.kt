package chat.simplex.app

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.size
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.DeletedItemView
import chat.simplex.common.views.chat.item.IntegrityErrorItemView
import chat.simplex.common.views.chat.item.LocalTerminalLayout
import chat.simplex.common.views.chat.item.MarkedDeletedItemView
import chat.simplex.common.views.chat.item.TerminalChatItemRow
import org.jetbrains.skia.Bitmap
import kotlin.test.Test
import kotlin.test.assertEquals

class FlatPillTest {
  private fun pillStart(gutter: Dp, item: @Composable () -> Unit): Int {
    val bitmap = Bitmap.makeFromImage(renderScene(width = 402, height = 100) {
      CompositionLocalProvider(LocalTerminalLayout provides true) {
        Box(Modifier.size(402.dp, 100.dp).background(Color.Black)) {
          TerminalChatItemRow(sender = null, gutter = gutter, topGap = 0.dp) { item() }
        }
      }
    })
    return (0 until 402).first { x -> (0 until 100).any { y -> Color(bitmap.getColor(x, y)).red > 0.5f } }
  }

  private fun assertPillsStartAt(expected: Int, gutter: Dp) {
    val deleted = ChatItem.getDeletedContentSampleData()
    assertEquals(expected, pillStart(gutter) { DeletedItemView(deleted, null, showViaProxy = false, showTimestamp = true) }, "deleted")
    val marked = ChatItem.getSampleData(dir = CIDirection.DirectRcv(), itemDeleted = CIDeleted.Deleted(null))
    val chatsCtx = ChatModel.ChatsContext(secondaryContextFilter = null)
    assertEquals(expected, pillStart(gutter) { MarkedDeletedItemView(chatsCtx, marked, Chat.sampleData.chatInfo, null, mutableStateOf(true), showViaProxy = false, showTimestamp = true) }, "marked deleted")
    assertEquals(expected, pillStart(gutter) { IntegrityErrorItemView(MsgErrorType.MsgSkipped(1, 2), deleted, showTimestamp = true, timedMessagesTTL = null) }, "integrity error")
  }

  @Test
  fun pillsStartAtTextEdgeWithoutAvatars() {
    assertPillsStartAt(12, 0.dp)
  }

  @Test
  fun pillTextInMessageColumn() {
    assertPillsStartAt(35, 35.dp)
  }
}
