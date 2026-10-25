package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalLayoutDirection
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.text.withStyle
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.appPreferences
import chat.simplex.common.ui.theme.isInDarkTheme
import chat.simplex.res.*
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
  lineHeightColor: Color = metaColor,
) {
  val timeStyle = LocalTextStyle.current.merge(terminalTextStyle(12.sp, lineHeightColor))
  Row(Modifier.padding(start = 3.dp), verticalAlignment = Alignment.CenterVertically) {
    if (chatItem.isDeletedContent) {
      Text(
        chatItem.timestampText,
        color = metaColor,
        fontSize = 12.sp,
        modifier = Modifier.padding(start = 3.dp),
        style = timeStyle
      )
    } else {
      CIMetaText(
        chatItem.meta,
        timedMessagesTTL,
        encrypted = chatItem.encryptedFile,
        metaColor,
        paleMetaColor,
        timeStyle,
        showStatus = showStatus,
        showEdited = showEdited,
        showViaProxy = showViaProxy,
        showTimestamp = showTimestamp,
        signedFileVerified = chatItem.file?.loaded
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
  timeStyle: TextStyle,
  showStatus: Boolean = true,
  showEdited: Boolean = true,
  showTimestamp: Boolean,
  showViaProxy: Boolean,
  signedFileVerified: Boolean?,
) {
  val showSignature = appPreferences.privacyShowSignature.state.value
  val showEncryption = appPreferences.privacyShowEncryption.state.value
  if (showEdited && meta.itemEdited) {
    StatusIconText(painterResource(MR.images.ic_edit), color)
  }
  if (meta.disappearing) {
    StatusIconText(painterResource(MR.images.ic_timer), color)
    val ttl = meta.itemTimed?.ttl
    if (ttl != chatTTL) {
      Text(shortTimeText(ttl), color = color, fontSize = 12.sp, style = timeStyle)
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
      if (meta.tallStatusIcon) {
        Icon(painterResource(icon), null, Modifier.height(17.dp), tint = statusColor)
      } else {
        StatusIconText(painterResource(icon), statusColor)
      }
    } else if (!meta.disappearing) {
      StatusIconText(painterResource(MR.images.ic_circle_filled), Color.Transparent)
    }
  }
  if (encrypted != null && showEncryption) {
    Spacer(Modifier.width(4.dp))
    StatusIconText(painterResource(if (encrypted) MR.images.ic_lock else MR.images.ic_lock_open_right), color)
  }
  if (showSignature && meta.msgVerified?.verified == true && signedFileVerified != false) {
    Spacer(Modifier.width(4.dp))
    StatusIconText(painterResource(MR.images.ic_verified), color)
  } else if (meta.msgVerified is MsgVerified.SigMissing) {
    Spacer(Modifier.width(4.dp))
    StatusIconText(painterResource(MR.images.ic_verified_missing), Color.Red)
  }

  if (showTimestamp) {
    Spacer(Modifier.width(4.dp))
    Text(meta.timestampText, color = color, fontSize = 12.sp, maxLines = 1, overflow = TextOverflow.Ellipsis, style = timeStyle)
  }
}

fun reserveSpaceForMeta(
  meta: CIMeta,
  chatTTL: Int?,
  encrypted: Boolean?,
  secondaryColor: Color,
  showStatus: Boolean = true,
  showEdited: Boolean = true,
  showViaProxy: Boolean = false,
  showTimestamp: Boolean,
  signedFileVerified: Boolean? = null
): String = buildString {
  reserveSpaceForMeta(meta, chatTTL, encrypted, secondaryColor, showStatus, showEdited, showViaProxy, showTimestamp, signedFileVerified, tallIconSpace = "", spaces = { append(it) }, text = { append(it) })
}

// the conditions in this function should match CIMetaText
private fun reserveSpaceForMeta(
  meta: CIMeta,
  chatTTL: Int?,
  encrypted: Boolean?,
  secondaryColor: Color,
  showStatus: Boolean,
  showEdited: Boolean,
  showViaProxy: Boolean,
  showTimestamp: Boolean,
  signedFileVerified: Boolean?,
  tallIconSpace: String,
  spaces: (String) -> Unit,
  text: (String) -> Unit
) {
  val showSignature = appPreferences.privacyShowSignature.state.value
  val showEncryption = appPreferences.privacyShowEncryption.state.value
  val iconSpace = " \u00A0\u00A0\u00A0"
  val whiteSpace = "\u00A0"
  if (!showTimestamp) spaces(iconSpace)
  var space: String? = null

  fun appendSpace() {
    space?.let(spaces)
    space = null
  }

  if (showEdited && meta.itemEdited) {
    spaces(iconSpace)
  }
  if (meta.itemTimed != null) {
    spaces(iconSpace)
    val ttl = meta.itemTimed.ttl
    if (ttl != chatTTL) {
      text(shortTimeText(ttl))
    }
    space = whiteSpace
  }
  if (showViaProxy && meta.sentViaProxy == true) {
    appendSpace()
    spaces(iconSpace + tallIconSpace)
  }
  if (showStatus) {
    appendSpace()
    if (meta.statusIcon(secondaryColor) != null) {
      spaces(if (meta.tallStatusIcon) iconSpace + tallIconSpace else iconSpace)
    } else if (!meta.disappearing) {
      spaces(iconSpace)
    }
    space = whiteSpace
  }

  if (encrypted != null && showEncryption) {
    appendSpace()
    spaces(iconSpace)
    space = whiteSpace
  }
  if ((showSignature && meta.msgVerified?.verified == true && signedFileVerified != false) || meta.msgVerified is MsgVerified.SigMissing) {
    appendSpace()
    spaces(iconSpace)
    space = whiteSpace
  }
  if (showTimestamp) {
    appendSpace()
    text(meta.timestampText)
  }
}

@Composable
fun terminalReserveSpaceForMeta(
  meta: CIMeta,
  chatTTL: Int?,
  encrypted: Boolean?,
  signedFileVerified: Boolean?,
  secondaryColor: Color,
  showStatus: Boolean = true,
  showEdited: Boolean = true,
  showViaProxy: Boolean,
  showTimestamp: Boolean
): AnnotatedString =
  terminalReserveSpaceForMeta(
    meta, chatTTL, encrypted, signedFileVerified, secondaryColor, showStatus, showEdited, showViaProxy, showTimestamp,
    density = LocalDensity.current,
    bodyFontSize = MaterialTheme.typography.body1.fontSize,
    direction = LocalLayoutDirection.current
  )

fun terminalReserveSpaceForMeta(
  meta: CIMeta,
  chatTTL: Int?,
  encrypted: Boolean?,
  signedFileVerified: Boolean?,
  secondaryColor: Color,
  showStatus: Boolean,
  showEdited: Boolean,
  showViaProxy: Boolean,
  showTimestamp: Boolean,
  density: Density,
  bodyFontSize: TextUnit,
  direction: LayoutDirection
): AnnotatedString {
  val spaceStyle = SpanStyle(fontSize = with(density) { bodyFontSize.value.dp.toSp() }, fontWeight = FontWeight.Normal)
  val textStyle = SpanStyle(fontSize = 12.sp, fontWeight = FontWeight.Normal)
  return buildAnnotatedString {
    append(if (direction == LayoutDirection.Ltr) "\u2060\u200E" else "\u2060\u200F")
    reserveSpaceForMeta(
      meta, chatTTL, encrypted, secondaryColor, showStatus, showEdited, showViaProxy, showTimestamp, signedFileVerified, tallIconSpace = "\u00A0",
      spaces = { withStyle(spaceStyle) { append(it.replace(' ', '\u00A0')) } },
      text = { withStyle(textStyle) { append(it.replace(' ', '\u00A0')) } }
    )
  }
}

private val CIMeta.tallStatusIcon: Boolean get() =
  itemStatus is CIStatus.SndSent || itemStatus is CIStatus.SndRcvd

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
