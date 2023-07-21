package chat.simplex.app.views.chat

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.CurrentColors
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.views.chat.item.ItemAction
import chat.simplex.app.views.chat.item.MarkdownText
import chat.simplex.app.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource

enum class CIInfoTab {
  History, Quote
}

@Composable
fun ChatItemInfoView(ci: ChatItem, ciInfo: ChatItemInfo, devTools: Boolean) {
  val sent = ci.chatDir.sent
  val appColors = CurrentColors.collectAsState().value.appColors
  val uriHandler = LocalUriHandler.current
  val selection = remember { mutableStateOf(CIInfoTab.History) }

  @Composable
  fun TextBubble(text: String, formattedText: List<FormattedText>?, sender: String?, showMenu: MutableState<Boolean>) {
    if (text != "") {
      MarkdownText(
        text, if (text.isEmpty()) emptyList() else formattedText,
        sender = sender,
        senderBold = true,
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

  @Composable
  fun ItemVersionView(ciVersion: ChatItemVersion, current: Boolean) {
    val showMenu = remember { mutableStateOf(false) }
    val text = ciVersion.msgContent.text
    val itemColor = if (sent) appColors.sentMessage else appColors.receivedMessage

    Column {
      Box(
        Modifier.clip(RoundedCornerShape(18.dp)).background(itemColor).padding(bottom = 3.dp)
          .combinedClickable(onLongClick = { showMenu.value = true }, onClick = {})
      ) {
        Box(Modifier.padding(vertical = 6.dp, horizontal = 12.dp)) {
          TextBubble(text, ciVersion.formattedText, sender = null, showMenu)
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
        DefaultDropdownMenu(showMenu) {
          ItemAction(stringResource(MR.strings.share_verb), painterResource(MR.images.ic_share), onClick = {
            shareText(text)
            showMenu.value = false
          })
          ItemAction(stringResource(MR.strings.copy_verb), painterResource(MR.images.ic_content_copy), onClick = {
            copyText(text)
            showMenu.value = false
          })
        }
      }
    }
  }

  @Composable
  fun QuotedMsgView(qi: CIQuote) {
    val showMenu = remember { mutableStateOf(false) }
    val text = qi.text
    val quoteColor = if (qi.chatDir?.sent == true) appColors.sentMessage else appColors.receivedMessage

    Column {
      Box(
        Modifier.clip(RoundedCornerShape(18.dp)).background(quoteColor).padding(bottom = 3.dp)
          .combinedClickable(onLongClick = { showMenu.value = true }, onClick = {})
      ) {
        Box(Modifier.padding(vertical = 6.dp, horizontal = 12.dp)) {
          TextBubble(text, qi.formattedText, sender = qi.sender(null), showMenu)
        }
      }
      Row(Modifier.padding(start = 12.dp, top = 3.dp, bottom = 16.dp)) {
        Text(
          localTimestamp(qi.sentAt),
          fontSize = 12.sp,
          color = MaterialTheme.colors.secondary,
          modifier = Modifier.padding(end = 6.dp)
        )
      }
      if (text != "") {
        DefaultDropdownMenu(showMenu) {
          ItemAction(stringResource(MR.strings.share_verb), painterResource(MR.images.ic_share), onClick = {
            shareText(text)
            showMenu.value = false
          })
          ItemAction(stringResource(MR.strings.copy_verb), painterResource(MR.images.ic_content_copy), onClick = {
            copyText(text)
            showMenu.value = false
          })
        }
      }
    }
  }

  @Composable
  fun Details() {
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
  }

  @Composable
  fun HistoryTab() {
    Column(Modifier.fillMaxWidth().verticalScroll(rememberScrollState())) {
      Details()
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
      val versions = ciInfo.itemVersions
      if (versions.isNotEmpty()) {
        SectionView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          Text(stringResource(MR.strings.edit_history), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
          versions.forEachIndexed { i, ciVersion ->
            ItemVersionView(ciVersion, current = i == 0)
          }
        }
      } else {
        SectionView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            Text(stringResource(MR.strings.no_history), color = MaterialTheme.colors.secondary)
          }
        }
      }
      SectionBottomSpacer()
    }
  }

  @Composable
  fun QuoteTab(qi: CIQuote) {
    Column(Modifier.fillMaxWidth().verticalScroll(rememberScrollState())) {
      Details()
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
      SectionView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
        Text(stringResource(MR.strings.in_reply_to), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
        QuotedMsgView(qi)
      }
      SectionBottomSpacer()
    }
  }

  @Composable
  fun tabTitle(tab: CIInfoTab): String {
    return when (tab) {
      CIInfoTab.History -> stringResource(MR.strings.edit_history)
      CIInfoTab.Quote -> stringResource(MR.strings.in_reply_to)
    }
  }

  fun tabIcon(tab: CIInfoTab): ImageResource {
    return when (tab) {
      CIInfoTab.History -> MR.images.ic_history
      CIInfoTab.Quote -> MR.images.ic_reply
    }
  }

  Column {
    if (ci.quotedItem != null) {
      Column(
        Modifier
          .fillMaxHeight(),
        verticalArrangement = Arrangement.SpaceBetween
      ) {
        Column(Modifier.weight(1f)) {
          when (selection.value) {
            CIInfoTab.History -> {
              HistoryTab()
            }

            CIInfoTab.Quote -> {
              QuoteTab(ci.quotedItem)
            }
          }
        }
        TabRow(
          selectedTabIndex = selection.value.ordinal,
          backgroundColor = Color.Transparent,
          contentColor = MaterialTheme.colors.primary,
        ) {
          CIInfoTab.values().forEachIndexed { index, it ->
            Tab(
              selected = selection.value.ordinal == index,
              onClick = {
                selection.value = CIInfoTab.values()[index]
              },
              text = { Text(tabTitle(it), fontSize = 13.sp) },
              icon = {
                Icon(
                  painterResource(tabIcon(it)),
                  tabTitle(it)
                )
              },
              selectedContentColor = MaterialTheme.colors.primary,
              unselectedContentColor = MaterialTheme.colors.secondary,
            )
          }
        }
      }
    } else {
      HistoryTab()
    }
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
  val qi = ci.quotedItem
  if (qi != null) {
    shareText.add("")
    shareText.add(generalGetString(MR.strings.in_reply_to))
    shareText.add("")
    val ts = localTimestamp(qi.sentAt)
    val sender = qi.sender(null)
    if (sender != null) {
      shareText.add(String.format(generalGetString(MR.strings.sender_at_ts), sender, ts))
    } else {
      shareText.add(ts)
    }
    val t = qi.text
    shareText.add(if (t != "") t else generalGetString(MR.strings.item_info_no_text))
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
          ts
        }
      )
      val t = itemVersion.msgContent.text
      shareText.add(if (t != "") t else generalGetString(MR.strings.item_info_no_text))
    }
  }
  return shareText.joinToString(separator = "\n")
}
