package chat.simplex.common.views.chat

import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.focusable
import androidx.compose.foundation.gestures.detectHorizontalDragGestures
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import androidx.compose.material.icons.filled.KeyboardArrowRight
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.key.*
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.base64ToBitmap
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlin.math.abs

@Composable
fun ComposeAttachmentsView(
  attachments: List<PendingAttachment>,
  enabled: Boolean,
  remove: (String) -> Unit,
  move: (Int, Int) -> Unit,
  clear: () -> Unit,
) {
  Row(
    Modifier
      .fillMaxWidth()
      .padding(top = 6.dp)
      .background(MaterialTheme.colors.surface.copy(alpha = 0.96f)),
    verticalAlignment = Alignment.CenterVertically
  ) {
    LazyRow(
      Modifier.weight(1f).padding(start = DEFAULT_PADDING_HALF, top = 6.dp, bottom = 6.dp),
      horizontalArrangement = Arrangement.spacedBy(8.dp)
    ) {
      itemsIndexed(attachments, key = { _, item -> item.id }) { index, attachment ->
        var dragDistance by remember(attachment.id) { mutableFloatStateOf(0f) }
        Surface(
          modifier = Modifier
            .width(154.dp)
            .clip(RoundedCornerShape(10.dp))
            .focusable(enabled)
            .onPreviewKeyEvent { event ->
              if (enabled && event.type == KeyEventType.KeyDown && (event.key == Key.Delete || event.key == Key.Backspace)) {
                remove(attachment.id)
                true
              } else false
            }
            .pointerInput(enabled, index, attachments.size) {
              if (!enabled) return@pointerInput
              detectHorizontalDragGestures(
                onDragStart = { dragDistance = 0f },
                onHorizontalDrag = { change, amount ->
                  change.consume()
                  dragDistance += amount
                  if (abs(dragDistance) >= 44f) {
                    val target = if (dragDistance < 0) index - 1 else index + 1
                    if (target in attachments.indices) move(index, target)
                    dragDistance = 0f
                  }
                }
              )
            },
          shape = RoundedCornerShape(10.dp),
          elevation = 1.dp
        ) {
          Column(Modifier.padding(6.dp), horizontalAlignment = Alignment.CenterHorizontally) {
            val preview = attachment.previewImage
            if (preview != null) {
              Box(contentAlignment = Alignment.Center) {
                Image(
                  bitmap = base64ToBitmap(preview),
                  contentDescription = "Preview of ${attachment.fileName}",
                  modifier = Modifier.fillMaxWidth().height(62.dp).clip(RoundedCornerShape(7.dp)),
                  contentScale = ContentScale.Crop
                )
                if (attachment.kind == PendingAttachmentKind.Video) {
                  Icon(painterResource(MR.images.ic_videocam_filled), "Video", tint = Color.White)
                }
              }
            } else {
              Box(Modifier.fillMaxWidth().height(62.dp), contentAlignment = Alignment.Center) {
                Icon(painterResource(MR.images.ic_draft), null, Modifier.size(30.dp), tint = MaterialTheme.colors.secondary)
              }
            }
            Text(
              attachment.fileName,
              Modifier.fillMaxWidth().padding(top = 4.dp),
              maxLines = 1,
              overflow = TextOverflow.Ellipsis,
              style = MaterialTheme.typography.caption
            )
            Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
              IconButton(onClick = { move(index, index - 1) }, enabled = enabled && index > 0, modifier = Modifier.size(30.dp)) {
                Icon(Icons.Default.KeyboardArrowLeft, "Move ${attachment.fileName} left")
              }
              IconButton(onClick = { remove(attachment.id) }, enabled = enabled, modifier = Modifier.size(30.dp)) {
                Icon(painterResource(MR.images.ic_close), "Remove ${attachment.fileName}")
              }
              IconButton(onClick = { move(index, index + 1) }, enabled = enabled && index < attachments.lastIndex, modifier = Modifier.size(30.dp)) {
                Icon(Icons.Default.KeyboardArrowRight, "Move ${attachment.fileName} right")
              }
            }
          }
        }
      }
    }
    IconButton(onClick = clear, enabled = enabled) {
      Icon(painterResource(MR.images.ic_close), "Remove all attachments", tint = MaterialTheme.colors.primary)
    }
  }
}
