package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionItemView
import SectionItemViewLongClickable
import androidx.compose.animation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.LazyColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
fun ModalData.MemberSupportView(
  chat: Chat,
  groupInfo: GroupInfo,
  activeSortedMembers: List<GroupMember>,
  close: () -> Unit
) {
  KeyChangeEffect(chat.id) {
    close()
  }
  ModalView(close = close) {
    MemberSupportViewLayout(
      chat,
      groupInfo,
      activeSortedMembers,
    )
  }
}

@Composable
private fun ModalData.MemberSupportViewLayout(
  chat: Chat,
  groupInfo: GroupInfo,
  activeSortedMembers: List<GroupMember>
) {
  val membersSupportChats = activeSortedMembers.filter { it.supportChat != null }
  val searchText = remember { stateGetOrPut("searchText") { TextFieldValue() } }
  val filteredMembers = remember(membersSupportChats) {
    derivedStateOf {
      val s = searchText.value.text.trim().lowercase()
      if (s.isEmpty()) membersSupportChats else membersSupportChats.filter { m -> m.anyNameContains(s) }
    }
  }

  LazyColumnWithScrollBar(
    contentPadding =
      PaddingValues(
        top = topPaddingToContent(false)
      )
  ) {
    item {
      AppBarTitle(stringResource(MR.strings.member_support))
    }

    if (membersSupportChats.isEmpty()) {
      item {
        Box(Modifier.fillMaxSize().padding(horizontal = DEFAULT_PADDING), contentAlignment = Alignment.Center) {
          Text(generalGetString(MR.strings.no_support_chats), color = MaterialTheme.colors.secondary, textAlign = TextAlign.Center)
        }
      }
    } else {
      if (membersSupportChats.size > 8) {
        item {
          SectionItemView(padding = PaddingValues(start = 14.dp, end = DEFAULT_PADDING_HALF)) {
            MemberListSearchRowView(searchText)
          }
        }
      }
      items(filteredMembers.value, key = { it.groupMemberId }) { member ->
        Divider()
        val showMenu = remember { mutableStateOf(false) }
        SectionItemViewLongClickable(
          click = {
            // TODO [knocking] open chat
          },
          longClick = { showMenu.value = true },
          minHeight = 54.dp,
          padding = PaddingValues(horizontal = DEFAULT_PADDING)
        ) {
          Box(contentAlignment = Alignment.CenterStart) {
            DropDownMenuForSupportChat(chat.remoteHostId, member, groupInfo, showMenu)
            MemberRow(member)
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
private fun DropDownMenuForSupportChat(rhId: Long?, member: GroupMember, groupInfo: GroupInfo, showMenu: MutableState<Boolean>) {
  DefaultDropdownMenu(showMenu) {
    ItemAction(stringResource(MR.strings.mark_unread), painterResource(MR.images.ic_mark_chat_unread), onClick = {
      // TODO [knocking] mark support chat unread
      showMenu.value = false
    })
  }
}
