package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.Edit
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.WarningYellow
import kotlinx.datetime.Clock

@Composable
fun CIMetaView(chatItem: ChatItem, timedMessagesTTL: Int?, metaColor: Color = HighOrLowlight) {
  Row(Modifier.padding(start = 3.dp), verticalAlignment = Alignment.CenterVertically) {
    if (chatItem.isDeletedContent) {
      Text(
        chatItem.timestampText,
        color = metaColor,
        fontSize = 14.sp,
        modifier = Modifier.padding(start = 3.dp)
      )
    } else {
      CIMetaText(chatItem.meta, timedMessagesTTL, metaColor)
    }
  }
}

@Composable
private fun CIMetaText(meta: CIMeta, chatTTL: Int?, color: Color) {
  if (meta.itemEdited) {
    StatusIconText(Icons.Outlined.Edit, color)
    Spacer(Modifier.width(3.dp))
  }
  if (meta.disappearing) {
    StatusIconText(Icons.Filled.Timer, color)
    Spacer(Modifier.width(3.dp))
    val ttl = meta.itemTimed?.ttl
    if (ttl != chatTTL) {
      Text(TimedMessagesPreference.shortTtlText(ttl), color = color)
      Spacer(Modifier.width(3.dp))
    }
  }
  val statusIcon = meta.statusIcon(MaterialTheme.colors.primary, color)
  if (statusIcon != null) {
    val (icon, statusColor) = statusIcon
    StatusIconText(icon, statusColor)
    Spacer(Modifier.width(3.dp))
  } else if (!meta.disappearing) {
    /*StatusIconText(Icons.Filled.MarkChatUnread, Color.Unspecified)
    Spacer(Modifier.width(3.dp))*/
  }
  Text(meta.timestampText, color = color, fontSize = 14.sp)
}

@Composable
private fun StatusIconText(icon: ImageVector, color: Color) {
  Icon(icon, null, Modifier.height(12.dp), tint = color)
}


@Composable
fun CIStatusView(status: CIStatus, metaColor: Color = HighOrLowlight) {
  when (status) {
    is CIStatus.SndSent -> {
      Icon(Icons.Filled.Check, stringResource(R.string.icon_descr_sent_msg_status_sent), Modifier.height(12.dp), tint = metaColor)
    }
    is CIStatus.SndErrorAuth -> {
      Icon(Icons.Filled.Close,  stringResource(R.string.icon_descr_sent_msg_status_unauthorized_send), Modifier.height(12.dp), tint = Color.Red)
    }
    is CIStatus.SndError -> {
      Icon(Icons.Filled.WarningAmber, stringResource(R.string.icon_descr_sent_msg_status_send_failed), Modifier.height(12.dp), tint = WarningYellow)
    }
    is CIStatus.RcvNew -> {
      Icon(Icons.Filled.Circle, stringResource(R.string.icon_descr_received_msg_status_unread), Modifier.height(12.dp), tint = MaterialTheme.colors.primary)
    }
    else -> {}
  }
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
