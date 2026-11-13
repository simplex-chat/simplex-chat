package chat.simplex.app

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.size
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.DeletedItemView
import chat.simplex.common.views.chat.item.IntegrityErrorItemView
import chat.simplex.common.views.chat.item.LocalTerminalLayout
import chat.simplex.common.views.chat.item.MarkedDeletedItemView
import org.jetbrains.skia.Bitmap
import kotlin.test.Test
import kotlin.test.assertEquals

class FlatPillTest {
  private fun pillStart(item: @Composable () -> Unit): Int {
    val bitmap = Bitmap.makeFromImage(renderScene(width = 402, height = 100) {
      CompositionLocalProvider(LocalTerminalLayout provides true) {
        Box(Modifier.size(402.dp, 100.dp).background(Color.White)) { item() }
      }
    })
    return (0 until 402).first { x -> (0 until 100).any { y -> bitmap.getColor(x, y) != Color.White.toArgb() } }
  }

  @Test
  fun pillsStartAtTextEdge() {
    val deleted = ChatItem.getDeletedContentSampleData()
    assertEquals(12, pillStart { DeletedItemView(deleted, null, showViaProxy = false, showTimestamp = true) }, "deleted")
    val marked = ChatItem.getSampleData(dir = CIDirection.DirectRcv(), itemDeleted = CIDeleted.Deleted(null))
    val chatsCtx = ChatModel.ChatsContext(secondaryContextFilter = null)
    assertEquals(12, pillStart { MarkedDeletedItemView(chatsCtx, marked, Chat.sampleData.chatInfo, null, mutableStateOf(true), showViaProxy = false, showTimestamp = true) }, "marked deleted")
    assertEquals(12, pillStart { IntegrityErrorItemView(MsgErrorType.MsgSkipped(1, 2), deleted, showTimestamp = true, timedMessagesTTL = null) }, "integrity error")
  }
}
