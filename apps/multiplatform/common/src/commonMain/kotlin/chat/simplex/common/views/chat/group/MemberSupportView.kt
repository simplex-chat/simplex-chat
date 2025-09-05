package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionItemView
import SectionItemViewLongClickable
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chatlist.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.*

@Composable
fun ModalData.MemberSupportView(
  rhId: Long?,
  chat: Chat,
  groupInfo: GroupInfo,
  scrollToItemId: MutableState<Long?>,
  close: () -> Unit
) {
  KeyChangeEffect(chatModel.chatId.value) {
    ModalManager.end.closeModals()
  }
  LaunchedEffect(Unit) {
    setGroupMembers(rhId, groupInfo, chatModel)
  }
  ModalView(
    close = close,
    endButtons = { RefreshMembersButton(rhId, groupInfo) }
  ) {
    MemberSupportViewLayout(
      chat,
      groupInfo,
      scrollToItemId
    )
  }
}

@Composable
fun RefreshMembersButton(
  rhId: Long?,
  groupInfo: GroupInfo
) {
  IconButton(
    onClick = {
      withBGApi {
        setGroupMembers(rhId, groupInfo, chatModel)
      }
    }
  ) {
    Icon(
      painterResource(MR.images.ic_refresh),
      contentDescription = null,
      tint = MaterialTheme.colors.primary
    )
  }
}

@Composable
private fun ModalData.MemberSupportViewLayout(
  chat: Chat,
  groupInfo: GroupInfo,
  scrollToItemId: MutableState<Long?>
) {
  val oneHandUI = remember { ChatController.appPrefs.oneHandUI.state }
  val scope = rememberCoroutineScope()

  val membersWithChats = remember { chatModel.groupMembers }.value
    .filter { it.supportChat != null && it.memberStatus != GroupMemberStatus.MemLeft && it.memberStatus != GroupMemberStatus.MemRemoved }
    .sortedWith(
      compareByDescending<GroupMember> { it.memberPending }
        .thenByDescending { (it.supportChat?.mentions ?: 0) > 0 }
        .thenByDescending { (it.supportChat?.memberAttention ?: 0) > 0 }
        .thenByDescending { (it.supportChat?.unread ?: 0) > 0 }
        .thenByDescending { it.supportChat?.chatTs }
    )

  val searchText = remember { stateGetOrPut("searchText") { TextFieldValue() } }
  val filteredmembersWithChats = remember(membersWithChats) {
    derivedStateOf {
      val s = searchText.value.text.trim().lowercase()
      if (s.isEmpty()) membersWithChats else membersWithChats.filter { m -> m.anyNameContains(s) }
    }
  }

  LazyColumnWithScrollBar(
    contentPadding =
    PaddingValues(
      top = if (oneHandUI.value) WindowInsets.statusBars.asPaddingValues().calculateTopPadding() + DEFAULT_PADDING + 5.dp else topPaddingToContent(false)
    )
  ) {
    item {
      AppBarTitle(stringResource(MR.strings.member_support))
    }

    if (membersWithChats.isEmpty()) {
      item {
        Box(Modifier.fillMaxSize().padding(horizontal = DEFAULT_PADDING), contentAlignment = Alignment.Center) {
          Text(generalGetString(MR.strings.no_support_chats), color = MaterialTheme.colors.secondary, textAlign = TextAlign.Center)
        }
      }
    } else {
      item {
        SectionItemView(padding = PaddingValues(start = 14.dp, end = DEFAULT_PADDING_HALF)) {
          MemberListSearchRowView(searchText)
        }
      }
      items(filteredmembersWithChats.value, key = { it.groupMemberId }) { member ->
        Divider()
        val showMenu = remember { mutableStateOf(false) }
        SectionItemViewLongClickable(
          click = {
            val scopeInfo = GroupChatScopeInfo.MemberSupport(groupMember_ = member)
            val supportChatInfo = ChatInfo.Group(groupInfo, groupChatScope = scopeInfo)
            scope.launch {
              showMemberSupportChatView(
                chatModel.chatId,
                scrollToItemId = scrollToItemId,
                supportChatInfo,
                scopeInfo
              )
            }
          },
          longClick = { showMenu.value = true },
          minHeight = 54.dp,
          padding = PaddingValues(horizontal = DEFAULT_PADDING)
        ) {
          Box(contentAlignment = Alignment.CenterStart) {
            DropDownMenuForSupportChat(chat.remoteHostId, member, groupInfo, showMenu)
            SupportChatRow(member)
          }
        }
      }
      item {
        Divider()
        SectionBottomSpacer()
      }
    }
  }
}

@Composable
fun SupportChatRow(member: GroupMember) {
  fun memberStatus(): String {
    return if (member.activeConn?.connDisabled == true) {
      generalGetString(MR.strings.member_info_member_disabled)
    } else if (member.activeConn?.connInactive == true) {
      generalGetString(MR.strings.member_info_member_inactive)
    } else if (member.memberPending) {
      member.memberStatus.text
    } else {
      member.memberRole.text
    }
  }

  @Composable
  fun SupportChatUnreadIndicator(supportChat: GroupSupportChat) {
    Box(Modifier.widthIn(min = 34.sp.toDp()), contentAlignment = Alignment.TopEnd) {
      Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.sp.toDp())) {
        if (supportChat.unread > 0 || supportChat.mentions > 0 || supportChat.memberAttention > 0) {
          val unreadBadgeColor = when {
            supportChat.mentions > 0 || supportChat.memberAttention > 0 -> MaterialTheme.colors.primaryVariant
            else -> MaterialTheme.colors.secondary
          }
          if (supportChat.mentions == 1 && supportChat.unread == 1) {
            Box(modifier = Modifier.offset(y = 2.sp.toDp()).size(15.sp.toDp()).background(unreadBadgeColor, shape = CircleShape), contentAlignment = Alignment.Center) {
              Icon(
                painterResource(MR.images.ic_alternate_email),
                contentDescription = generalGetString(MR.strings.notifications),
                tint = Color.White,
                modifier = Modifier.size(9.sp.toDp())
              )
            }
          } else {
            if (supportChat.mentions > 0 && supportChat.unread > 1) {
              Icon(
                painterResource(MR.images.ic_alternate_email),
                contentDescription = generalGetString(MR.strings.notifications),
                tint = unreadBadgeColor,
                modifier = Modifier.size(12.sp.toDp()).offset(y = 2.sp.toDp())
              )
            }

            UnreadBadge(
              text = unreadCountStr(supportChat.unread),
              backgroundColor = unreadBadgeColor,
              yOffset = 2.dp
            )
          }
        }
      }
    }
  }

  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      Modifier.weight(1f).padding(top = MEMBER_ROW_VERTICAL_PADDING, end = DEFAULT_PADDING, bottom = MEMBER_ROW_VERTICAL_PADDING),
      horizontalArrangement = Arrangement.spacedBy(4.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      MemberProfileImage(size = MEMBER_ROW_AVATAR_SIZE, member)
      Spacer(Modifier.width(DEFAULT_PADDING_HALF))
      Column {
        Row(verticalAlignment = Alignment.CenterVertically) {
          if (member.verified) {
            MemberVerifiedShield()
          }
          Text(
            member.chatViewName, maxLines = 1, overflow = TextOverflow.Ellipsis,
            color = if (member.memberIncognito) Indigo else Color.Unspecified
          )
        }

        Text(
          memberStatus(),
          color = MaterialTheme.colors.secondary,
          fontSize = 12.sp,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis
        )
      }
    }

    Row {
      if (member.memberPending) {
        Icon(
          painterResource(MR.images.ic_flag_filled),
          contentDescription = null,
          Modifier.padding(end = 3.dp).size(16.dp),
          tint = MaterialTheme.colors.primaryVariant
        )
      }
      if (member.supportChat != null) {
        SupportChatUnreadIndicator(member.supportChat)
      }
    }
  }
}

@Composable
private fun DropDownMenuForSupportChat(rhId: Long?, member: GroupMember, groupInfo: GroupInfo, showMenu: MutableState<Boolean>) {
  DefaultDropdownMenu(showMenu) {
    if (member.memberPending) {
      ItemAction(stringResource(MR.strings.accept_pending_member_button), painterResource(MR.images.ic_check), color = MaterialTheme.colors.primary, onClick = {
        acceptMemberDialog(rhId, groupInfo, member)
        showMenu.value = false
      })
    } else {
      if (member.supportChatNotRead) {
        ItemAction(stringResource(MR.strings.mark_read), painterResource(MR.images.ic_check), color = MaterialTheme.colors.primary, onClick = {
          markSupportChatRead(rhId, groupInfo, member)
          showMenu.value = false
        })
      }
      ItemAction(stringResource(MR.strings.delete_member_support_chat_button), painterResource(MR.images.ic_delete), color = MaterialTheme.colors.error, onClick = {
        deleteMemberSupportChatDialog(rhId, groupInfo, member)
        showMenu.value = false
      })
    }
  }
}

fun deleteMemberSupportChatDialog(rhId: Long?, groupInfo: GroupInfo, member: GroupMember) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_member_support_chat_alert_title),
    confirmText = generalGetString(MR.strings.delete_member_support_chat_button),
    onConfirm = {
      deleteMemberSupportChat(rhId, groupInfo, member)
    },
    destructive = true,
  )
}

private fun deleteMemberSupportChat(rhId: Long?, groupInfo: GroupInfo, member: GroupMember) {
  withBGApi {
    val r = chatModel.controller.apiDeleteMemberSupportChat(rhId, groupInfo.groupId, member.groupMemberId)
    if (r != null) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.upsertGroupMember(rhId, r.first, r.second)
        chatModel.chatsContext.updateGroup(rhId, r.first)
      }
    }
  }
}

private fun markSupportChatRead(rhId: Long?, groupInfo: GroupInfo, member: GroupMember) {
  withBGApi {
    if (member.supportChatNotRead) {
      val r = chatModel.controller.apiSupportChatRead(
        rh = rhId,
        type = ChatType.Group,
        id = groupInfo.apiId,
        scope = GroupChatScope.MemberSupport(member.groupMemberId)
      )
      if (r != null) {
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.upsertGroupMember(rhId, r.first, r.second)
          chatModel.chatsContext.updateGroup(rhId, r.first)
        }
      }
    }
  }
}
