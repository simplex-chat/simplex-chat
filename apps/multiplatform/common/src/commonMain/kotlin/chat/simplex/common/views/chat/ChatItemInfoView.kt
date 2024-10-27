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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.AnnotatedString
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.group.MemberProfileImage
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import kotlinx.serialization.encodeToString

sealed class CIInfoTab {
  class Delivery(val memberDeliveryStatuses: List<MemberDeliveryStatus>): CIInfoTab()
  object History: CIInfoTab()
  class Quote(val quotedItem: CIQuote): CIInfoTab()
  class Forwarded(val forwardedFromChatItem: AChatItem): CIInfoTab()
}

@Composable
fun ChatItemInfoView(chatRh: Long?, ci: ChatItem, ciInfo: ChatItemInfo, devTools: Boolean) {
  val sent = ci.chatDir.sent
  val appColors = MaterialTheme.appColors
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
        Modifier.clipChatItem().background(itemColor).padding(bottom = 3.dp)
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
        Modifier.clipChatItem().background(quoteColor).padding(bottom = 3.dp)
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

  val local = when (ci.chatDir) {
    is CIDirection.LocalSnd -> true
    is CIDirection.LocalRcv -> true
    else -> false
  }

  @Composable
  fun ForwardedFromSender(forwardedFromItem: AChatItem) {
    @Composable
    fun ItemText(text: String, fontStyle: FontStyle = FontStyle.Normal, color: Color = MaterialTheme.colors.onBackground) {
      Text(
        text,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.body1,
        fontStyle = fontStyle,
        color = color,
      )
    }

    Row(verticalAlignment = Alignment.CenterVertically) {
      ChatInfoImage(forwardedFromItem.chatInfo, size = 57.dp)
      Column(
        modifier = Modifier
          .padding(start = 15.dp)
          .weight(1F)
      ) {
        if (forwardedFromItem.chatItem.chatDir.sent) {
          ItemText(text = stringResource(MR.strings.sender_you_pronoun), fontStyle = FontStyle.Italic)
          Spacer(Modifier.height(7.dp))
          ItemText(forwardedFromItem.chatInfo.chatViewName, color = MaterialTheme.colors.secondary)
        } else if (forwardedFromItem.chatItem.chatDir is CIDirection.GroupRcv) {
          ItemText(text = forwardedFromItem.chatItem.chatDir.groupMember.chatViewName)
          Spacer(Modifier.height(7.dp))
          ItemText(forwardedFromItem.chatInfo.chatViewName, color = MaterialTheme.colors.secondary)
        } else {
          ItemText(forwardedFromItem.chatInfo.chatViewName, color = MaterialTheme.colors.onBackground)
        }
      }
    }
  }

  @Composable
  fun ForwardedFromView(forwardedFromItem: AChatItem) {
    Column {
      SectionItemView(
        click = {
          withBGApi {
            openChat(chatRh, forwardedFromItem.chatInfo, chatModel)
            ModalManager.end.closeModals()
          }
        },
        padding = PaddingValues(start = 17.dp, end = DEFAULT_PADDING)
      ) {
        ForwardedFromSender(forwardedFromItem)
      }

      if (!local) {
        Divider(Modifier.padding(start = DEFAULT_PADDING_HALF, top = 41.dp, end = DEFAULT_PADDING_HALF, bottom = DEFAULT_PADDING_HALF))
        Text(stringResource(MR.strings.recipients_can_not_see_who_message_from), Modifier.padding(horizontal = DEFAULT_PADDING), fontSize = 12.sp, color = MaterialTheme.colors.secondary)
      }
    }
  }

  @Composable
  fun ExpandableInfoRow(title: String, value: String) {
    val expanded = remember { mutableStateOf(false) }
    Row(
      Modifier
        .fillMaxWidth()
        .sizeIn(minHeight = DEFAULT_MIN_SECTION_ITEM_HEIGHT)
        .padding(PaddingValues(horizontal = DEFAULT_PADDING))
        .clickable { expanded.value = !expanded.value },
      horizontalArrangement = Arrangement.spacedBy(12.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Text(title, color = MaterialTheme.colors.onBackground)
      if (expanded.value) {
        Text(value, color = MaterialTheme.colors.secondary)
      } else {
        Text(value, color = MaterialTheme.colors.secondary, maxLines = 1)
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
        ExpandableInfoRow(stringResource(MR.strings.info_row_message_status), jsonShort.encodeToString(ci.meta.itemStatus))
        if (ci.file != null) {
          ExpandableInfoRow(stringResource(MR.strings.info_row_file_status), jsonShort.encodeToString(ci.file.fileStatus))
        }
      }
    }
  }

  @Composable
  fun HistoryTab() {
    ColumnWithScrollBar(Modifier.fillMaxWidth()) {
      Details()
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = true)
      val versions = ciInfo.itemVersions
      if (versions.isNotEmpty()) {
        SectionView(contentPadding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          Text(stringResource(MR.strings.edit_history), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
          versions.forEachIndexed { i, ciVersion ->
            ItemVersionView(ciVersion, current = i == 0)
          }
        }
      } else {
        SectionView(contentPadding = PaddingValues(horizontal = DEFAULT_PADDING)) {
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
    ColumnWithScrollBar(Modifier.fillMaxWidth()) {
      Details()
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = true)
      SectionView(contentPadding = PaddingValues(horizontal = DEFAULT_PADDING)) {
        Text(stringResource(MR.strings.in_reply_to), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
        QuotedMsgView(qi)
      }
      SectionBottomSpacer()
    }
  }

  @Composable
  fun ForwardedFromTab(forwardedFromItem: AChatItem) {
    ColumnWithScrollBar(Modifier.fillMaxWidth()) {
      Details()
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = true)
      SectionView {
        Text(stringResource(if (local) MR.strings.saved_from_chat_item_info_title else MR.strings.forwarded_from_chat_item_info_title),
          style = MaterialTheme.typography.h2,
          modifier = Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING))
        ForwardedFromView(forwardedFromItem)
      }
      SectionBottomSpacer()
    }
  }

  @Composable
  fun MemberDeliveryStatusView(member: GroupMember, status: GroupSndStatus, sentViaProxy: Boolean?) {
    SectionItemView(
      padding = PaddingValues(horizontal = 0.dp)
    ) {
      MemberProfileImage(size = 36.dp, member)
      Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON))
      Text(
        member.chatViewName,
        modifier = Modifier.weight(10f, fill = true),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
      Spacer(Modifier.fillMaxWidth().weight(1f))
      if (sentViaProxy == true) {
        Box(
          Modifier.size(36.dp),
          contentAlignment = Alignment.Center
        ) {
          Icon(
            painterResource(MR.images.ic_arrow_forward),
            contentDescription = null,
            tint = CurrentColors.value.colors.secondary
          )
        }
      }
      val (icon, statusColor) = status.statusIcon(MaterialTheme.colors.primary, CurrentColors.value.colors.secondary)
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
        Icon(
          painterResource(icon),
          contentDescription = null,
          tint = statusColor
        )
      }
    }
  }

  @Composable
  fun DeliveryTab(memberDeliveryStatuses: List<MemberDeliveryStatus>) {
    ColumnWithScrollBar(Modifier.fillMaxWidth()) {
      Details()
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = true)
      val mss = membersStatuses(chatModel, memberDeliveryStatuses)
      if (mss.isNotEmpty()) {
        SectionView(contentPadding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          Text(stringResource(MR.strings.delivery), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
          mss.forEach { (member, status, sentViaProxy) ->
            MemberDeliveryStatusView(member, status, sentViaProxy)
          }
        }
      } else {
        SectionView(contentPadding = PaddingValues(horizontal = DEFAULT_PADDING)) {
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
      is CIInfoTab.Forwarded -> stringResource(if (local) MR.strings.saved_chat_item_info_tab else MR.strings.forwarded_chat_item_info_tab)
    }
  }

  fun tabIcon(tab: CIInfoTab): ImageResource {
    return when (tab) {
      is CIInfoTab.Delivery -> MR.images.ic_double_check
      is CIInfoTab.History -> MR.images.ic_history
      is CIInfoTab.Quote -> MR.images.ic_reply
      is CIInfoTab.Forwarded -> MR.images.ic_forward
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
    if (ciInfo.forwardedFromChatItem != null) {
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

            is CIInfoTab.Forwarded -> {
              ForwardedFromTab(sel.forwardedFromChatItem)
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
        if (ciInfo.forwardedFromChatItem != null) {
          availableTabs.add(CIInfoTab.Forwarded(ciInfo.forwardedFromChatItem))
        }
        if (availableTabs.none { it.javaClass == selection.value.javaClass }) {
          selection.value = availableTabs.first()
        }
        LaunchedEffect(ciInfo) {
          if (ciInfo.forwardedFromChatItem != null && selection.value is CIInfoTab.Forwarded) {
            selection.value = CIInfoTab.Forwarded(ciInfo.forwardedFromChatItem)
          } else if (ciInfo.memberDeliveryStatuses != null) {
            selection.value = CIInfoTab.Delivery(ciInfo.memberDeliveryStatuses)
          }
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

private fun membersStatuses(chatModel: ChatModel, memberDeliveryStatuses: List<MemberDeliveryStatus>): List<Triple<GroupMember, GroupSndStatus, Boolean?>> {
  return memberDeliveryStatuses.mapNotNull { mds ->
    chatModel.getGroupMember(mds.groupMemberId)?.let { mem ->
      Triple(mem, mds.memberDeliveryStatus, mds.sentViaProxy)
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
    shareText.add(String.format(generalGetString(MR.strings.share_text_message_status), jsonShort.encodeToString(ci.meta.itemStatus)))
    if (ci.file != null) {
      shareText.add(String.format(generalGetString(MR.strings.share_text_file_status), jsonShort.encodeToString(ci.file.fileStatus)))
    }
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
