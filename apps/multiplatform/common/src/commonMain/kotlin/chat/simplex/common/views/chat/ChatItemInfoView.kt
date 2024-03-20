package chat.simplex.common.views.chat

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.AnnotatedString
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chat.item.MarkdownText
import chat.simplex.common.views.helpers.*
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource

sealed class CIInfoTab {
  class Delivery(val memberDeliveryStatuses: List<MemberDeliveryStatus>): CIInfoTab()
  object History: CIInfoTab()
  class Quote(val quotedItem: CIQuote): CIInfoTab()
}

@Composable
fun ChatItemInfoView(chatModel: ChatModel, ci: ChatItem, ciInfo: ChatItemInfo, devTools: Boolean) {
  val sent = ci.chatDir.sent
  val appColors = CurrentColors.collectAsState().value.appColors
  val uriHandler = LocalUriHandler.current
  val selection = remember { mutableStateOf<CIInfoTab>(CIInfoTab.History) }

  @Composable
  fun TextBubble(text: String, formattedText: List<FormattedText>?, sender: String?, showMenu: MutableState<Boolean>) {
    if (text != "") {
      MarkdownText(
        text, if (text.isEmpty()) emptyList() else formattedText,
        sender = sender,
        senderBold = true,
        toggleSecrets = true,
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
          .onRightClick { showMenu.value = true }
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
        val clipboard = LocalClipboardManager.current
        DefaultDropdownMenu(showMenu) {
          ItemAction(stringResource(MR.strings.share_verb), painterResource(MR.images.ic_share), onClick = {
            clipboard.shareText(text)
            showMenu.value = false
          })
          ItemAction(stringResource(MR.strings.copy_verb), painterResource(MR.images.ic_content_copy), onClick = {
            clipboard.setText(AnnotatedString(text))
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
          .onRightClick { showMenu.value = true }
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
        val clipboard = LocalClipboardManager.current
        DefaultDropdownMenu(showMenu) {
          ItemAction(stringResource(MR.strings.share_verb), painterResource(MR.images.ic_share), onClick = {
            clipboard.shareText(text)
            showMenu.value = false
          })
          ItemAction(stringResource(MR.strings.copy_verb), painterResource(MR.images.ic_content_copy), onClick = {
            clipboard.setText(AnnotatedString(text))
            showMenu.value = false
          })
        }
      }
    }
  }

  @Composable
  fun Details() {
    AppBarTitle(stringResource(if (ci.localNote) MR.strings.saved_message_title else if (sent) MR.strings.sent_message else MR.strings.received_message))
    SectionView {
      InfoRow(stringResource(if (!ci.localNote) MR.strings.info_row_sent_at else MR.strings.info_row_created_at), localTimestamp(ci.meta.itemTs))
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
    // LALAL SCROLLBAR DOESN'T WORK
    ColumnWithScrollBar(Modifier.fillMaxWidth()) {
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
    // LALAL SCROLLBAR DOESN'T WORK
    ColumnWithScrollBar(Modifier.fillMaxWidth()) {
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
  fun MemberDeliveryStatusView(member: GroupMember, status: CIStatus) {
    SectionItemView(
      padding = PaddingValues(horizontal = 0.dp)
    ) {
      ProfileImage(size = 36.dp, member.image)
      Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON))
      Text(
        member.chatViewName,
        modifier = Modifier.weight(10f, fill = true),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
      Spacer(Modifier.fillMaxWidth().weight(1f))
      val statusIcon = status.statusIcon(MaterialTheme.colors.primary, CurrentColors.value.colors.secondary)
      var modifier = Modifier.size(36.dp).clip(RoundedCornerShape(20.dp))
      val info = status.statusInto
      if (info != null) {
        modifier = modifier.clickable {
          AlertManager.shared.showAlertMsg(
            title = info.first,
            text = info.second
          )
        }
      }
      Box(modifier, contentAlignment = Alignment.Center) {
        if (statusIcon != null) {
          val (icon, statusColor) = statusIcon
          Icon(
            painterResource(icon),
            contentDescription = null,
            tint = statusColor
          )
        } else {
          Icon(
            painterResource(MR.images.ic_more_horiz),
            contentDescription = null,
            tint = CurrentColors.value.colors.secondary
          )
        }
      }
    }
  }

  @Composable
  fun DeliveryTab(memberDeliveryStatuses: List<MemberDeliveryStatus>) {
    // LALAL SCROLLBAR DOESN'T WORK
    ColumnWithScrollBar(Modifier.fillMaxWidth()) {
      Details()
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
      val mss = membersStatuses(chatModel, memberDeliveryStatuses)
      if (mss.isNotEmpty()) {
        SectionView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          Text(stringResource(MR.strings.delivery), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
          mss.forEach { (member, status) ->
            MemberDeliveryStatusView(member, status)
          }
        }
      } else {
        SectionView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            Text(stringResource(MR.strings.no_info_on_delivery), color = MaterialTheme.colors.secondary)
          }
        }
      }
      SectionBottomSpacer()
    }
  }

  @Composable
  fun tabTitle(tab: CIInfoTab): String {
    return when (tab) {
      is CIInfoTab.Delivery -> stringResource(MR.strings.delivery)
      is CIInfoTab.History -> stringResource(MR.strings.edit_history)
      is CIInfoTab.Quote -> stringResource(MR.strings.in_reply_to)
    }
  }

  fun tabIcon(tab: CIInfoTab): ImageResource {
    return when (tab) {
      is CIInfoTab.Delivery -> MR.images.ic_double_check
      is CIInfoTab.History -> MR.images.ic_history
      is CIInfoTab.Quote -> MR.images.ic_reply
    }
  }

  fun numTabs(): Int {
    var numTabs = 1
    if (ciInfo.memberDeliveryStatuses != null) {
      numTabs += 1
    }
    if (ci.quotedItem != null) {
      numTabs += 1
    }
    return numTabs
  }

  Column {
    if (numTabs() > 1) {
      Column(
        Modifier
          .fillMaxHeight(),
        verticalArrangement = Arrangement.SpaceBetween
      ) {
        LaunchedEffect(ciInfo) {
          if (ciInfo.memberDeliveryStatuses != null) {
            selection.value = CIInfoTab.Delivery(ciInfo.memberDeliveryStatuses)
          }
        }
        Column(Modifier.weight(1f)) {
          when (val sel = selection.value) {
            is CIInfoTab.Delivery -> {
              DeliveryTab(sel.memberDeliveryStatuses)
            }

            is CIInfoTab.History -> {
              HistoryTab()
            }

            is CIInfoTab.Quote -> {
              QuoteTab(sel.quotedItem)
            }
          }
        }
        val availableTabs = mutableListOf<CIInfoTab>()
        if (ciInfo.memberDeliveryStatuses != null) {
          availableTabs.add(CIInfoTab.Delivery(ciInfo.memberDeliveryStatuses))
        }
        availableTabs.add(CIInfoTab.History)
        if (ci.quotedItem != null) {
          availableTabs.add(CIInfoTab.Quote(ci.quotedItem))
        }
        TabRow(
          selectedTabIndex = availableTabs.indexOfFirst { it::class == selection.value::class },
          backgroundColor = Color.Transparent,
          contentColor = MaterialTheme.colors.primary,
        ) {
          availableTabs.forEach { ciInfoTab ->
            Tab(
              selected = selection.value::class == ciInfoTab::class,
              onClick = {
                selection.value = ciInfoTab
              },
              text = { Text(tabTitle(ciInfoTab), fontSize = 13.sp) },
              icon = {
                Icon(
                  painterResource(tabIcon(ciInfoTab)),
                  tabTitle(ciInfoTab)
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

private fun membersStatuses(chatModel: ChatModel, memberDeliveryStatuses: List<MemberDeliveryStatus>): List<Pair<GroupMember, CIStatus>> {
  return memberDeliveryStatuses.mapNotNull { mds ->
    chatModel.getGroupMember(mds.groupMemberId)?.let { mem ->
      mem to mds.memberDeliveryStatus
    }
  }
}

fun itemInfoShareText(chatModel: ChatModel, ci: ChatItem, chatItemInfo: ChatItemInfo, devTools: Boolean): String {
  val meta = ci.meta
  val sent = ci.chatDir.sent
  val shareText = mutableListOf<String>("# " + generalGetString(if (ci.localNote) MR.strings.saved_message_title else if (sent) MR.strings.sent_message else MR.strings.received_message), "")

  shareText.add(String.format(generalGetString(if (ci.localNote) MR.strings.share_text_created_at else MR.strings.share_text_sent_at), localTimestamp(meta.itemTs)))
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
    shareText.add("## " + generalGetString(MR.strings.in_reply_to))
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
    shareText.add("## " + generalGetString(MR.strings.edit_history))
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
