package chat.simplex.common.views.chat.group

import SectionDividerSpaced
import SectionItemView
import SectionItemViewLongClickable
import SectionSpacer
import SectionView
import androidx.compose.animation.*
import androidx.compose.animation.AnimatedVisibility
import androidx.compose.animation.core.animateDpAsState
import androidx.compose.foundation.background
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListScope
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.chatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.SelectedListItem
import chat.simplex.common.views.chat.SendReceipts
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chat.item.SelectItemAction
import chat.simplex.common.views.chat.group.showGroupReportsView
import chat.simplex.common.views.chatlist.cantInviteIncognitoAlert
import chat.simplex.common.views.chatlist.openChat
import chat.simplex.common.views.chatlist.UnreadBadge
import chat.simplex.common.views.chatlist.unreadCountStr
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.common.views.usersettings.SettingsActionItemWithContent
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch

enum class GroupInfoTab {
  Members,
  Images,
  Videos,
  Links,
  Files,
  Voices
}

fun LazyListScope.GroupChatInfoTabs(
  groupInfo: GroupInfo,
  activeSortedMembers: List<GroupMember>,
  filteredMembers: State<List<GroupMember>>,
  searchText: MutableState<TextFieldValue>,
  selectedItems: MutableState<Set<Long>?>,
  scrollToItemId: MutableState<Long?>,
  addMembers: () -> Unit,
  showMemberInfo: (GroupMember) -> Unit,
  selectedTab: MutableState<GroupInfoTab>,
  filteredChatItems: State<List<ChatItem>>,
  chat: Chat,
  groupLink: GroupLink?,
  manageGroupLink: () -> Unit,
  openMemberSupport: () -> Unit
) {

  item {
    SectionSpacer()

    val scrollState = rememberScrollState()
    Column {
      Row(
        Modifier
          .fillMaxWidth()
          .horizontalScroll(scrollState),
        horizontalArrangement = Arrangement.Start
      ) {
        GroupInfoTab.values().forEach { tab ->
          Tab(
            selected = selectedTab.value == tab,
            onClick = { selectedTab.value = tab },
            text = { Text(tabTitle(tab), fontSize = 13.sp) },
            selectedContentColor = MaterialTheme.colors.primary,
            unselectedContentColor = MaterialTheme.colors.secondary,
          )
        }
      }
      // Simple indicator line
      Box(
        Modifier
          .fillMaxWidth()
          .height(2.dp)
          .background(MaterialTheme.colors.surface)
      )
    }
    Divider()

    SectionSpacer()
  }

  when (selectedTab.value) {
    GroupInfoTab.Members -> {
      MembersTabContent(
        groupInfo = groupInfo,
        activeSortedMembers = activeSortedMembers,
        filteredMembers = filteredMembers,
        searchText = searchText,
        selectedItems = selectedItems,
        showMemberInfo = showMemberInfo,
        addMembers = addMembers,
        chat = chat,
        groupLink = groupLink,
        manageGroupLink = manageGroupLink,
        openMemberSupport = openMemberSupport,
        scrollToItemId = scrollToItemId
      )
    }
    GroupInfoTab.Images,
    GroupInfoTab.Videos,
    GroupInfoTab.Links,
    GroupInfoTab.Files,
    GroupInfoTab.Voices -> {
      ContentItemsTab(
        filteredChatItems = filteredChatItems,
        scrollToItemId = scrollToItemId
      )
    }
  }
}

private fun LazyListScope.MembersTabContent(
  groupInfo: GroupInfo,
  activeSortedMembers: List<GroupMember>,
  filteredMembers: State<List<GroupMember>>,
  searchText: MutableState<TextFieldValue>,
  selectedItems: MutableState<Set<Long>?>,
  showMemberInfo: (GroupMember) -> Unit,
  addMembers: () -> Unit,
  chat: Chat,
  groupLink: GroupLink?,
  manageGroupLink: () -> Unit,
  openMemberSupport: () -> Unit,
  scrollToItemId: MutableState<Long?>
) {

  item {
    val scope = rememberCoroutineScope()
    var anyTopSectionRowShow = false
    SectionView {
      if (groupInfo.canAddMembers && groupInfo.businessChat == null) {
        anyTopSectionRowShow = true
        if (groupLink == null) {
          CreateGroupLinkButton(manageGroupLink)
        } else {
          GroupLinkButton(manageGroupLink)
        }
      }
      if (groupInfo.businessChat == null && groupInfo.membership.memberRole >= GroupMemberRole.Moderator) {
        anyTopSectionRowShow = true
        MemberSupportButton(chat, openMemberSupport)
      }
      if (groupInfo.canModerate) {
        anyTopSectionRowShow = true
        GroupReportsButton(chat) {
          scope.launch {
            showGroupReportsView(chatModel.chatId, scrollToItemId, chat.chatInfo)
          }
        }
      }
      if (
        groupInfo.membership.memberActive &&
        (groupInfo.membership.memberRole < GroupMemberRole.Moderator || groupInfo.membership.supportChat != null)
      ) {
        anyTopSectionRowShow = true
        UserSupportChatButton(chat, groupInfo, scrollToItemId)
      }
    }
    if (anyTopSectionRowShow) {
      SectionDividerSpaced(maxBottomPadding = false)
    }
  }

  if (!groupInfo.nextConnectPrepared) {
    item {
      SectionView(title = String.format(generalGetString(MR.strings.group_info_section_title_num_members), activeSortedMembers.count() + 1)) {
        if (groupInfo.canAddMembers) {
          val onAddMembersClick = if (chat.chatInfo.incognito) ::cantInviteIncognitoAlert else addMembers
          val tint = if (chat.chatInfo.incognito) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
          val addMembersTitleId = when (groupInfo.businessChat?.chatType) {
            BusinessChatType.Customer -> MR.strings.button_add_team_members
            BusinessChatType.Business -> MR.strings.button_add_friends
            null -> MR.strings.button_add_members
          }
          AddMembersButton(addMembersTitleId, tint, onAddMembersClick)
        }
        if (activeSortedMembers.size > 8) {
          SectionItemView(padding = PaddingValues(start = 14.dp, end = DEFAULT_PADDING_HALF)) {
            MemberListSearchRowView(searchText)
          }
        }
        SectionItemView(minHeight = 54.dp, padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          MemberRow(groupInfo.membership, user = true)
        }
      }
    }
  }
  if (!groupInfo.nextConnectPrepared) {
    items(filteredMembers.value, key = { it.groupMemberId }) { member ->
      Divider()
      val showMenu = remember { mutableStateOf(false) }
      val canBeSelected = groupInfo.membership.memberRole >= member.memberRole && member.memberRole < GroupMemberRole.Moderator
      SectionItemViewLongClickable(
        click = {
          if (selectedItems.value != null) {
            if (canBeSelected) {
              toggleItemSelection(member.groupMemberId, selectedItems)
            }
          } else {
            showMemberInfo(member)
          }
        },
        longClick = { showMenu.value = true },
        minHeight = 54.dp,
        padding = PaddingValues(horizontal = DEFAULT_PADDING)
      ) {
        Box(contentAlignment = Alignment.CenterStart) {
          this@SectionItemViewLongClickable.AnimatedVisibility(selectedItems.value != null, enter = fadeIn(), exit = fadeOut()) {
            SelectedListItem(Modifier.alpha(if (canBeSelected) 1f else 0f).padding(start = 2.dp), member.groupMemberId, selectedItems)
          }
          val selectionOffset by animateDpAsState(if (selectedItems.value != null) 20.dp + 22.dp * fontSizeMultiplier else 0.dp)
          DropDownMenuForMember(chat.remoteHostId, member, groupInfo, selectedItems, showMenu)
          Box(Modifier.padding(start = selectionOffset)) {
            MemberRow(member)
          }
        }
      }
    }
  }
}

private fun LazyListScope.ContentItemsTab(
  filteredChatItems: State<List<ChatItem>>,
  scrollToItemId: MutableState<Long?>
) {
  if (filteredChatItems.value.isEmpty()) {
    item {
      Box(
        Modifier
          .fillMaxWidth()
          .padding(vertical = DEFAULT_PADDING * 2),
        contentAlignment = Alignment.Center
      ) {
      //TODO: this is just a temporary UI, if no item, tab will not be shown
        Text(
          "No items found",
          color = MaterialTheme.colors.secondary
        )
      }
    }
  } else {
    items(filteredChatItems.value, key = { it.id }) { chatItem ->
      Divider()
      SectionItemView(
        click = { scrollToItemId.value = chatItem.id },
        minHeight = 54.dp,
        padding = PaddingValues(horizontal = DEFAULT_PADDING)
      ) {
        GroupChatItemRow(chatItem)
      }
    }
  }
}


@Composable
private fun tabTitle(tab: GroupInfoTab): String {
  return when (tab) {
    GroupInfoTab.Members -> stringResource(MR.strings.group_info_tab_members)
    GroupInfoTab.Images -> stringResource(MR.strings.group_info_tab_images)
    GroupInfoTab.Videos -> stringResource(MR.strings.group_info_tab_videos)
    GroupInfoTab.Links -> stringResource(MR.strings.group_info_tab_links)
    GroupInfoTab.Files -> stringResource(MR.strings.group_info_tab_files)
    GroupInfoTab.Voices -> stringResource(MR.strings.group_info_tab_voices)
  }
}


@Composable
private fun GroupChatItemRow(chatItem: ChatItem) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    when (val content = chatItem.content.msgContent) {
      is MsgContent.MCImage -> {
        Icon(painterResource(MR.images.ic_image), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
        //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            content.text.ifEmpty { "Image" },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      is MsgContent.MCVideo -> {
        Icon(painterResource(MR.images.ic_videocam), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
          //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            content.text.ifEmpty { "Video" },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      is MsgContent.MCLink -> {
        Icon(painterResource(MR.images.ic_link), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
          //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            content.preview.uri.ifEmpty { content.text },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      is MsgContent.MCFile -> {
        Icon(painterResource(MR.images.ic_draft_filled), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
          //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            chatItem.file?.fileName ?: content.text.ifEmpty { "File" },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      is MsgContent.MCVoice -> {
        Icon(painterResource(MR.images.ic_mic_filled), null, Modifier.size(24.dp), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING))
        Column(Modifier.weight(1f)) {
          //TODO, This is just a temporary UI, needs to be adjusted
          Text(
            content.text.ifEmpty { generalGetString(MR.strings.voice_message) },
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            getTimestampText(chatItem.meta.itemTs),
            fontSize = 12.sp,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      else -> {
        Text(
          generalGetString(MR.strings.unknown_message_format),
          color = MaterialTheme.colors.secondary
        )
      }
    }
  }
}


@Composable
private fun AddMembersButton(titleId: StringResource, tint: Color = MaterialTheme.colors.primary, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_add),
    stringResource(titleId),
    onClick,
    iconColor = tint,
    textColor = tint
  )
}


@Composable
private fun DropDownMenuForMember(rhId: Long?, member: GroupMember, groupInfo: GroupInfo, selectedItems: MutableState<Set<Long>?>, showMenu: MutableState<Boolean>) {
  if (groupInfo.membership.memberRole >= GroupMemberRole.Moderator) {
    val canBlockForAll = member.canBlockForAll(groupInfo)
    val canRemove = member.canBeRemoved(groupInfo)
    if (canBlockForAll || canRemove) {
      DefaultDropdownMenu(showMenu) {
        if (canBlockForAll) {
          if (member.blockedByAdmin) {
            ItemAction(stringResource(MR.strings.unblock_for_all), painterResource(MR.images.ic_do_not_touch), onClick = {
              unblockForAllAlert(rhId, groupInfo, member)
              showMenu.value = false
            })
          } else {
            ItemAction(stringResource(MR.strings.block_for_all), painterResource(MR.images.ic_back_hand), color = MaterialTheme.colors.error, onClick = {
              blockForAllAlert(rhId, groupInfo, member)
              showMenu.value = false
            })
          }
        }
        if (canRemove) {
          ItemAction(stringResource(MR.strings.remove_member_button), painterResource(MR.images.ic_delete), color = MaterialTheme.colors.error, onClick = {
            removeMemberAlert(rhId, groupInfo, member)
            showMenu.value = false
          })
        }
        if (selectedItems.value == null && member.memberRole < GroupMemberRole.Moderator) {
          Divider()
          SelectItemAction(showMenu) { toggleItemSelection(member.groupMemberId, selectedItems) }
        }
      }
    }
  } else if (!member.blockedByAdmin) {
    DefaultDropdownMenu(showMenu) {
      if (member.memberSettings.showMessages) {
        ItemAction(stringResource(MR.strings.block_member_button), painterResource(MR.images.ic_back_hand), color = MaterialTheme.colors.error, onClick = {
          blockMemberAlert(rhId, groupInfo, member)
          showMenu.value = false
        })
      } else {
        ItemAction(stringResource(MR.strings.unblock_member_button), painterResource(MR.images.ic_do_not_touch), onClick = {
          unblockMemberAlert(rhId, groupInfo, member)
          showMenu.value = false
        })
      }
    }
  }
}


private fun removeMemberAlert(rhId: Long?, groupInfo: GroupInfo, mem: GroupMember) {
  val messageId = if (groupInfo.businessChat == null)
    MR.strings.member_will_be_removed_from_group_cannot_be_undone
  else
    MR.strings.member_will_be_removed_from_chat_cannot_be_undone
  AlertManager.shared.showAlertDialogButtonsColumn(
    generalGetString(MR.strings.button_remove_member_question),
    generalGetString(messageId),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          removeMembers(rhId, groupInfo, listOf(mem.groupMemberId), withMessages = false)
        }) {
          Text(generalGetString(MR.strings.remove_member_confirmation), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          removeMembers(rhId, groupInfo, listOf(mem.groupMemberId), withMessages = true)
        }) {
          Text(generalGetString(MR.strings.remove_member_delete_messages_confirmation), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(generalGetString(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    })
}

@Composable
private fun CreateGroupLinkButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_add_link),
    stringResource(MR.strings.create_group_link),
    onClick,
    iconColor = MaterialTheme.colors.secondary
  )
}

@Composable
private fun GroupLinkButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_link),
    stringResource(MR.strings.group_link),
    onClick,
    iconColor = MaterialTheme.colors.secondary
  )
}

@Composable
private fun GroupPreferencesButton(titleId: StringResource, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_toggle_on),
    stringResource(titleId),
    click = onClick
  )
}

@Composable
private fun GroupReportsButton(chat: Chat, onClick: () -> Unit) {
  SettingsActionItemWithContent(
    painterResource(if (chat.chatStats.reportsCount > 0) MR.images.ic_flag_filled else MR.images.ic_flag),
    stringResource(MR.strings.group_reports_member_reports),
    click = onClick,
    iconColor = (if (chat.chatStats.reportsCount > 0) Color.Red else MaterialTheme.colors.secondary)
  ) {
    if (chat.chatStats.reportsCount > 0) {
      UnreadBadge(
        text = unreadCountStr(chat.chatStats.reportsCount),
        backgroundColor = Color.Red
      )
    }
  }
}

@Composable
private fun SendReceiptsOption(currentUser: User, state: State<SendReceipts>, onSelected: (SendReceipts) -> Unit) {
  val values = remember {
    mutableListOf(SendReceipts.Yes, SendReceipts.No, SendReceipts.UserDefault(currentUser.sendRcptsSmallGroups)).map { it to it.text }
  }
  ExposedDropDownSettingRow(
    generalGetString(MR.strings.send_receipts),
    values,
    state,
    icon = painterResource(MR.images.ic_double_check),
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}

@Composable
private fun MemberSupportButton(chat: Chat, onClick: () -> Unit) {
  SettingsActionItemWithContent(
    painterResource(if (chat.supportUnreadCount > 0) MR.images.ic_flag_filled else MR.images.ic_flag),
    stringResource(MR.strings.member_support),
    click = onClick,
    iconColor = (if (chat.supportUnreadCount > 0) MaterialTheme.colors.primary else MaterialTheme.colors.secondary)
  ) {
    if (chat.supportUnreadCount > 0) {
      UnreadBadge(
        text = unreadCountStr(chat.supportUnreadCount),
        backgroundColor = MaterialTheme.colors.primary
      )
    }
  }
}
