package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.isInDarkTheme
import chat.simplex.res.MR
import kotlinx.datetime.Clock

@Composable
fun CIMetaView(
  chatItem: ChatItem,
  timedMessagesTTL: Int?,
  metaColor: Color = MaterialTheme.colors.secondary,
  paleMetaColor: Color = if (isInDarkTheme()) {
    metaColor.copy(
      red = metaColor.red * 0.67F,
      green = metaColor.green * 0.67F,
      blue = metaColor.red * 0.67F)
  } else {
    metaColor.copy(
      red = minOf(metaColor.red * 1.33F, 1F),
      green = minOf(metaColor.green * 1.33F, 1F),
      blue = minOf(metaColor.red * 1.33F, 1F))
  },
  showStatus: Boolean = true,
  showEdited: Boolean = true,
  showTimestamp: Boolean,
  showViaProxy: Boolean,
) {
  Row(Modifier.padding(start = 3.dp), verticalAlignment = Alignment.CenterVertically) {
    if (chatItem.isDeletedContent) {
      Text(
        chatItem.timestampText,
        color = metaColor,
        fontSize = 12.sp,
        modifier = Modifier.padding(start = 3.dp)
      )
    } else {
      CIMetaText(
        chatItem.meta,
        timedMessagesTTL,
        encrypted = chatItem.encryptedFile,
        metaColor,
        paleMetaColor,
        showStatus = showStatus,
        showEdited = showEdited,
        showViaProxy = showViaProxy,
        showTimestamp = showTimestamp
      )
    }
  }
}

@Composable
// changing this function requires updating reserveSpaceForMeta
private fun CIMetaText(
  meta: CIMeta,
  chatTTL: Int?,
  encrypted: Boolean?,
  color: Color,
  paleColor: Color,
  showStatus: Boolean = true,
  showEdited: Boolean = true,
  showTimestamp: Boolean,
  showViaProxy: Boolean,
) {
  if (showEdited && meta.itemEdited) {
    StatusIconText(painterResource(MR.images.ic_edit), color)
  }
  if (meta.disappearing) {
    StatusIconText(painterResource(MR.images.ic_timer), color)
    val ttl = meta.itemTimed?.ttl
    if (ttl != chatTTL) {
      Text(shortTimeText(ttl), color = color, fontSize = 12.sp)
    }
  }
  if (showViaProxy && meta.sentViaProxy == true) {
    Spacer(Modifier.width(4.dp))
    Icon(painterResource(MR.images.ic_arrow_forward), null, Modifier.height(17.dp), tint = MaterialTheme.colors.secondary)
  }
  if (showStatus) {
    Spacer(Modifier.width(4.dp))
    val statusIcon = meta.statusIcon(MaterialTheme.colors.primary, color, paleColor)
    if (statusIcon != null) {
      val (icon, statusColor) = statusIcon
      if (meta.itemStatus is CIStatus.SndSent || meta.itemStatus is CIStatus.SndRcvd) {
        Icon(painterResource(icon), null, Modifier.height(17.dp), tint = statusColor)
      } else {
        StatusIconText(painterResource(icon), statusColor)
      }
    } else if (!meta.disappearing) {
      StatusIconText(painterResource(MR.images.ic_circle_filled), Color.Transparent)
    }
  }
  if (encrypted != null) {
    Spacer(Modifier.width(4.dp))
    StatusIconText(painterResource(if (encrypted) MR.images.ic_lock else MR.images.ic_lock_open_right), color)
  }

  if (showTimestamp) {
    Spacer(Modifier.width(4.dp))
    Text(meta.timestampText, color = color, fontSize = 12.sp, maxLines = 1, overflow = TextOverflow.Ellipsis)
  }
}

// the conditions in this function should match CIMetaText
fun reserveSpaceForMeta(
  meta: CIMeta,
  chatTTL: Int?,
  encrypted: Boolean?,
  secondaryColor: Color,
  showStatus: Boolean = true,
  showEdited: Boolean = true,
  showViaProxy: Boolean = false,
  showTimestamp: Boolean
): String {
  val iconSpace = " \u00A0\u00A0\u00A0"
  val whiteSpace = "\u00A0"
  var res = if (showTimestamp) "" else iconSpace
  var space: String? = null

  fun appendSpace() {
    if (space != null) {
      res += space
      space = null
    }
  }

  if (showEdited && meta.itemEdited) {
    res += iconSpace
  }
  if (meta.itemTimed != null) {
    res += iconSpace
    val ttl = meta.itemTimed.ttl
    if (ttl != chatTTL) {
      res += shortTimeText(ttl)
    }
    space = whiteSpace
  }
  if (showViaProxy && meta.sentViaProxy == true) {
    appendSpace()
    res += iconSpace
  }
  if (showStatus) {
    appendSpace()
    if (meta.statusIcon(secondaryColor) != null) {
      res += iconSpace
    } else if (!meta.disappearing) {
      res += iconSpace
    }
    space = whiteSpace
  }

  if (encrypted != null) {
    appendSpace()
    res += iconSpace
    space = whiteSpace
  }
  if (showTimestamp) {
    appendSpace()
    res += meta.timestampText
  }
  return res
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
    null,
    showViaProxy = false,
    showTimestamp = true
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
    null,
    showViaProxy = false,
    showTimestamp = true
  )
}

@Preview
@Composable
fun PreviewCIMetaViewSendFailed() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello",
      status = CIStatus.CISSndError(SndError.Other("CMD SYNTAX"))
    ),
    null,
    showViaProxy = false,
    showTimestamp = true
  )
}

@Preview
@Composable
fun PreviewCIMetaViewSendNoAuth() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello", status = CIStatus.SndErrorAuth()
    ),
    null,
    showViaProxy = false,
    showTimestamp = true
  )
}

@Preview
@Composable
fun PreviewCIMetaViewSendSent() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello", status = CIStatus.SndSent(SndCIStatusProgress.Complete)
    ),
    null,
    showViaProxy = false,
    showTimestamp = true
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
    null,
    showViaProxy = false,
    showTimestamp = true
  )
}

@Preview
@Composable
fun PreviewCIMetaViewEditedUnread() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectRcv(), Clock.System.now(), "hello",
      itemEdited = true,
      status= CIStatus.RcvNew()
    ),
    null,
    showViaProxy = false,
    showTimestamp = true
  )
}

@Preview
@Composable
fun PreviewCIMetaViewEditedSent() {
  CIMetaView(
    chatItem = ChatItem.getSampleData(
      1, CIDirection.DirectSnd(), Clock.System.now(), "hello",
      itemEdited = true,
      status= CIStatus.SndSent(SndCIStatusProgress.Complete)
    ),
    null,
    showViaProxy = false,
    showTimestamp = true
  )
}

@Preview
@Composable
fun PreviewCIMetaViewDeletedContent() {
  CIMetaView(
    chatItem = ChatItem.getDeletedContentSampleData(),
    null,
    showViaProxy = false,
    showTimestamp = true
  )
}
