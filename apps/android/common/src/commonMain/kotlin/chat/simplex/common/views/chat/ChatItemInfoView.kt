package chat.simplex.common.views.chat

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.AnnotatedString
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chat.item.MarkdownText
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.shareText

@Composable
fun ChatItemInfoView(ci: ChatItem, ciInfo: ChatItemInfo, devTools: Boolean) {
  val sent = ci.chatDir.sent
  val appColors = CurrentColors.collectAsState().value.appColors
  val itemColor = if (sent) appColors.sentMessage else appColors.receivedMessage
  val uriHandler = LocalUriHandler.current

  @Composable
  fun ItemVersionView(ciVersion: ChatItemVersion, current: Boolean) {
    val showMenu = remember { mutableStateOf(false) }
    val text = ciVersion.msgContent.text

    @Composable
    fun VersionText() {
      if (text != "") {
        MarkdownText(
          text, if (text.isEmpty()) emptyList() else ciVersion.formattedText,
          linkMode = SimplexLinkMode.DESCRIPTION, uriHandler = uriHandler,
          onLinkLongClick = { showMenu.value = true }
        )
      } else {
        Text(
          generalGetString(MR.strings.item_info_no_text),
          style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.secondary, lineHeight = 22.sp, fontStyle = FontStyle.Italic)
        )
      }
    }

    Column {
      Box(
        Modifier.clip(RoundedCornerShape(18.dp)).background(itemColor).padding(bottom = 3.dp)
          .combinedClickable(onLongClick = { showMenu.value = true }, onClick = {})
      ) {
        Box(Modifier.padding(vertical = 6.dp, horizontal = 12.dp)) {
          VersionText()
        }
      }
      Row(Modifier.padding(start = 12.dp, top = 3.dp, bottom = 16.dp)) {
        Text(
          localTimestamp(ciVersion.itemVersionTs),
          fontSize = 12.sp,
          color = MaterialTheme.colors.secondary,
          modifier = Modifier.padding(end = 6.dp)
        )
        if (current && ci.meta.itemDeleted == null) {
          Text(
            stringResource(MR.strings.item_info_current),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      if (text != "") {
        val clipboard = LocalClipboardManager.current
        DefaultDropdownMenu(showMenu) {
          ItemAction(stringResource(MR.strings.share_verb), painterResource(MR.images.ic_share), onClick = {
            clipboard.shareText(text)
            showMenu.value = false
          })
          val clipboard = LocalClipboardManager.current
          ItemAction(stringResource(MR.strings.copy_verb), painterResource(MR.images.ic_content_copy), onClick = {
            clipboard.setText(AnnotatedString(text))
            showMenu.value = false
          })
        }
      }
    }
  }

  Column(Modifier.fillMaxWidth().verticalScroll(rememberScrollState())) {
    AppBarTitle(stringResource(if (sent) MR.strings.sent_message else MR.strings.received_message))
    SectionView {
      InfoRow(stringResource(MR.strings.info_row_sent_at), localTimestamp(ci.meta.itemTs))
      if (!sent) {
        InfoRow(stringResource(MR.strings.info_row_received_at), localTimestamp(ci.meta.createdAt))
      }
      when (val itemDeleted = ci.meta.itemDeleted) {
        is CIDeleted.Deleted ->
          if (itemDeleted.deletedTs != null) {
            InfoRow(stringResource(MR.strings.info_row_deleted_at), localTimestamp(itemDeleted.deletedTs))
          }
        is CIDeleted.Moderated ->
          if (itemDeleted.deletedTs != null) {
            InfoRow(stringResource(MR.strings.info_row_moderated_at), localTimestamp(itemDeleted.deletedTs))
          }
        else -> {}
      }
      val deleteAt = ci.meta.itemTimed?.deleteAt
      if (deleteAt != null) {
        InfoRow(stringResource(MR.strings.info_row_disappears_at), localTimestamp(deleteAt))
      }
      if (devTools) {
        InfoRow(stringResource(MR.strings.info_row_database_id), ci.meta.itemId.toString())
        InfoRow(stringResource(MR.strings.info_row_updated_at), localTimestamp(ci.meta.updatedAt))
      }
    }
    val versions = ciInfo.itemVersions
    if (versions.isNotEmpty()) {
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
      SectionView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
        Text(stringResource(MR.strings.edit_history), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
        versions.forEachIndexed { i, ciVersion ->
          ItemVersionView(ciVersion, current = i == 0)
        }
      }
    }
    SectionBottomSpacer()
  }
}

fun itemInfoShareText(ci: ChatItem, chatItemInfo: ChatItemInfo, devTools: Boolean): String {
  val meta = ci.meta
  val sent = ci.chatDir.sent
  val shareText = mutableListOf<String>(generalGetString(if (sent) MR.strings.sent_message else MR.strings.received_message), "")

  shareText.add(String.format(generalGetString(MR.strings.share_text_sent_at), localTimestamp(meta.itemTs)))
  if (!ci.chatDir.sent) {
    shareText.add(String.format(generalGetString(MR.strings.share_text_received_at), localTimestamp(meta.createdAt)))
  }
  when (val itemDeleted = ci.meta.itemDeleted) {
    is CIDeleted.Deleted ->
      if (itemDeleted.deletedTs != null) {
        shareText.add(String.format(generalGetString(MR.strings.share_text_deleted_at), localTimestamp(itemDeleted.deletedTs)))
      }
    is CIDeleted.Moderated ->
      if (itemDeleted.deletedTs != null) {
        shareText.add(String.format(generalGetString(MR.strings.share_text_moderated_at), localTimestamp(itemDeleted.deletedTs)))
      }
    else -> {}
  }
  val deleteAt = ci.meta.itemTimed?.deleteAt
  if (deleteAt != null) {
    shareText.add(String.format(generalGetString(MR.strings.share_text_disappears_at), localTimestamp(deleteAt)))
  }
  if (devTools) {
    shareText.add(String.format(generalGetString(MR.strings.share_text_database_id), meta.itemId))
    shareText.add(String.format(generalGetString(MR.strings.share_text_updated_at), meta.updatedAt))
  }
  val versions = chatItemInfo.itemVersions
  if (versions.isNotEmpty()) {
    shareText.add("")
    shareText.add(generalGetString(MR.strings.edit_history))
    versions.forEachIndexed { index, itemVersion ->
      val ts = localTimestamp(itemVersion.itemVersionTs)
      shareText.add("")
      shareText.add(
        if (index == 0 && ci.meta.itemDeleted == null) {
          String.format(generalGetString(MR.strings.current_version_timestamp), ts)
        } else {
          localTimestamp(itemVersion.itemVersionTs)
        }
      )
      val t = itemVersion.msgContent.text
      shareText.add(if (t != "") t else generalGetString(MR.strings.item_info_no_text))
    }
  }
  return shareText.joinToString(separator = "\n")
}
