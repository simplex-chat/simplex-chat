package chat.simplex.app.views.chat

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
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.CurrentColors
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.views.chat.item.ItemAction
import chat.simplex.app.views.chat.item.MarkdownText
import chat.simplex.app.views.helpers.*

@Composable
fun ChatItemInfoView(ci: ChatItem, ciInfo: ChatItemInfo, devTools: Boolean) {
  val sent = ci.chatDir.sent
  val appColors = CurrentColors.collectAsState().value.appColors
  val itemColor = if (sent) appColors.sentMessage else appColors.receivedMessage
  val context = LocalContext.current
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
          generalGetString(R.string.item_info_no_text),
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
            stringResource(R.string.item_info_current),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      if (text != "") {
        DefaultDropdownMenu(showMenu) {
          ItemAction(stringResource(R.string.share_verb), painterResource(R.drawable.ic_share), onClick = {
            shareText(context, text)
            showMenu.value = false
          })
          ItemAction(stringResource(R.string.copy_verb), painterResource(R.drawable.ic_content_copy), onClick = {
            copyText(context, text)
            showMenu.value = false
          })
        }
      }
    }
  }

  Column(Modifier.fillMaxWidth().verticalScroll(rememberScrollState())) {
    AppBarTitle(stringResource(if (sent) R.string.sent_message else R.string.received_message))
    SectionView {
      InfoRow(stringResource(R.string.info_row_sent_at), localTimestamp(ci.meta.itemTs))
      if (!sent) {
        InfoRow(stringResource(R.string.info_row_received_at), localTimestamp(ci.meta.createdAt))
      }
      when (val itemDeleted = ci.meta.itemDeleted) {
        is CIDeleted.Deleted ->
          if (itemDeleted.deletedTs != null) {
            InfoRow(stringResource(R.string.info_row_deleted_at), localTimestamp(itemDeleted.deletedTs))
          }
        is CIDeleted.Moderated ->
          if (itemDeleted.deletedTs != null) {
            InfoRow(stringResource(R.string.info_row_moderated_at), localTimestamp(itemDeleted.deletedTs))
          }
        else -> {}
      }
      val deleteAt = ci.meta.itemTimed?.deleteAt
      if (deleteAt != null) {
        InfoRow(stringResource(R.string.info_row_disappears_at), localTimestamp(deleteAt))
      }
      if (devTools) {
        InfoRow(stringResource(R.string.info_row_database_id), ci.meta.itemId.toString())
        InfoRow(stringResource(R.string.info_row_updated_at), localTimestamp(ci.meta.updatedAt))
      }
    }
    val versions = ciInfo.itemVersions
    if (versions.isNotEmpty()) {
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
      SectionView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
        Text(stringResource(R.string.edit_history), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
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
  val shareText = mutableListOf<String>(generalGetString(if (sent) R.string.sent_message else R.string.received_message), "")

  shareText.add(String.format(generalGetString(R.string.share_text_sent_at), localTimestamp(meta.itemTs)))
  if (!ci.chatDir.sent) {
    shareText.add(String.format(generalGetString(R.string.share_text_received_at), localTimestamp(meta.createdAt)))
  }
  when (val itemDeleted = ci.meta.itemDeleted) {
    is CIDeleted.Deleted ->
      if (itemDeleted.deletedTs != null) {
        shareText.add(String.format(generalGetString(R.string.share_text_deleted_at), localTimestamp(itemDeleted.deletedTs)))
      }
    is CIDeleted.Moderated ->
      if (itemDeleted.deletedTs != null) {
        shareText.add(String.format(generalGetString(R.string.share_text_moderated_at), localTimestamp(itemDeleted.deletedTs)))
      }
    else -> {}
  }
  val deleteAt = ci.meta.itemTimed?.deleteAt
  if (deleteAt != null) {
    shareText.add(String.format(generalGetString(R.string.share_text_disappears_at), localTimestamp(deleteAt)))
  }
  if (devTools) {
    shareText.add(String.format(generalGetString(R.string.share_text_database_id), meta.itemId))
    shareText.add(String.format(generalGetString(R.string.share_text_updated_at), meta.updatedAt))
  }
  val versions = chatItemInfo.itemVersions
  if (versions.isNotEmpty()) {
    shareText.add("")
    shareText.add(generalGetString(R.string.edit_history))
    versions.forEachIndexed { index, itemVersion ->
      val ts = localTimestamp(itemVersion.itemVersionTs)
      shareText.add("")
      shareText.add(
        if (index == 0 && ci.meta.itemDeleted == null) {
          String.format(generalGetString(R.string.current_version_timestamp), ts)
        } else {
          localTimestamp(itemVersion.itemVersionTs)
        }
      )
      val t = itemVersion.msgContent.text
      shareText.add(if (t != "") t else generalGetString(R.string.item_info_no_text))
    }
  }
  return shareText.joinToString(separator = "\n")
}
