package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.res.painterResource
import chat.simplex.app.R
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.CurrentColors
import kotlinx.datetime.Clock

@Composable
fun CIMetaView(chatItem: ChatItem, timedMessagesTTL: Int?, metaColor: Color = MaterialTheme.colors.secondary) {
  Row(Modifier.padding(start = 3.dp), verticalAlignment = Alignment.CenterVertically) {
    if (chatItem.isDeletedContent) {
      Text(
        chatItem.timestampText,
        color = metaColor,
        fontSize = 12.sp,
        modifier = Modifier.padding(start = 3.dp)
      )
    } else {
      CIMetaText(chatItem.meta, timedMessagesTTL, metaColor)
    }
  }
}

@Composable
// changing this function requires updating reserveSpaceForMeta
private fun CIMetaText(meta: CIMeta, chatTTL: Int?, color: Color) {
  if (meta.itemEdited) {
    StatusIconText(painterResource(R.drawable.ic_edit), color)
    Spacer(Modifier.width(3.dp))
  }
  if (meta.disappearing) {
    StatusIconText(painterResource(R.drawable.ic_timer), color)
    val ttl = meta.itemTimed?.ttl
    if (ttl != chatTTL) {
      Text(shortTimeText(ttl), color = color, fontSize = 12.sp)
    }
    Spacer(Modifier.width(4.dp))
  }
  val statusIcon = meta.statusIcon(MaterialTheme.colors.primary, color)
  if (statusIcon != null) {
    val (icon, statusColor) = statusIcon
    StatusIconText(painterResource(icon), statusColor)
    Spacer(Modifier.width(4.dp))
  } else if (!meta.disappearing) {
    StatusIconText(painterResource(R.drawable.ic_circle_filled), Color.Transparent)
    Spacer(Modifier.width(4.dp))
  }
  Text(meta.timestampText, color = color, fontSize = 12.sp, maxLines = 1, overflow = TextOverflow.Ellipsis)
}

// the conditions in this function should match CIMetaText
fun reserveSpaceForMeta(meta: CIMeta, chatTTL: Int?): String {
  val iconSpace = "    "
  var res = ""
  if (meta.itemEdited) res += iconSpace
  if (meta.itemTimed != null) {
    res += iconSpace
    val ttl = meta.itemTimed.ttl
    if (ttl != chatTTL) {
      res += shortTimeText(ttl)
    }
  }
  if (meta.statusIcon(CurrentColors.value.colors.secondary) != null || !meta.disappearing) {
    res += iconSpace
  }
  return res + meta.timestampText
}

@Composable
private fun StatusIconText(icon: Painter, color: Color) {
  Icon(icon, null, Modifier.height(12.dp), tint = color)
}

@Preview
@Composable
fun PreviewCIMetaView() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
    ),
    null
  )
}

@Preview
@Composable
fun PreviewCIMetaViewUnread() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello",
      status = CIStatus.RcvNew()
    ),
    null
  )
}

@Preview
@Composable
fun PreviewCIMetaViewSendFailed() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello",
      status = CIStatus.SndError("CMD SYNTAX")
    ),
    null
  )
}

@Preview
@Composable
fun PreviewCIMetaViewSendNoAuth() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello", status = CIStatus.SndErrorAuth()
    ),
    null
  )
}

@Preview
@Composable
fun PreviewCIMetaViewSendSent() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello", status = CIStatus.SndSent()
    ),
    null
  )
}

@Preview
@Composable
fun PreviewCIMetaViewEdited() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello",
      itemEdited = true
    ),
    null
  )
}

@Preview
@Composable
fun PreviewCIMetaViewEditedUnread() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectRcv(), Clock.System.now(), "hello",
      itemEdited = true,
      status=CIStatus.RcvNew()
    ),
    null
  )
}

@Preview
@Composable
fun PreviewCIMetaViewEditedSent() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello",
      itemEdited = true,
      status=CIStatus.SndSent()
    ),
    null
  )
}

@Preview
@Composable
fun PreviewCIMetaViewDeletedContent() {
  CIMetaView(
    chatItem = ChatItem.getDeletedContentSampleData(),
    null
  )
}
