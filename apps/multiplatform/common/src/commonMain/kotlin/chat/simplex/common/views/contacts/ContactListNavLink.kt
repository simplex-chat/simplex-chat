package chat.simplex.common.views.contacts

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.foundation.combinedClickable
import androidx.compose.ui.Alignment
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay

@Composable
fun ContactListNavLinkView(chat: Chat, nextChatSelected: State<Boolean>) {
  val showMenu = remember { mutableStateOf(false) }
  val disabled = chatModel.chatRunning.value == false || chatModel.deletedChats.value.contains(chat.remoteHostId to chat.chatInfo.id)
  LaunchedEffect(chat.id) {
    showMenu.value = false
    delay(500L)
  }
  val selectedChat = remember(chat.id) { derivedStateOf { chat.id == chatModel.chatId.value } }

  when (chat.chatInfo) {
    is ChatInfo.Direct -> {
      ContactListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ContactListNavLink", error = { ErrorChatListItem() }) {
            ContactPreviewView(chat, disabled)
          }
        },
        click = { directChatAction(chat.remoteHostId, chat.chatInfo.contact, chatModel) },
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ContactListNavLinkDropdown", error = {}) {
            ContactMenuItems(chat, chat.chatInfo.contact, chatModel, showMenu)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
      )
    }
    else -> {}
  }
}

fun directChatAction(rhId: Long?, contact: Contact, chatModel: ChatModel) {
  withBGApi { openChat(rhId, ChatInfo.Direct(contact), chatModel) }
}

@Composable
fun ContactMenuItems(chat: Chat, contact: Contact, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  if (contact.activeConn != null) {
    ToggleFavoritesChatAction(chat, chatModel, chat.chatInfo.chatSettings?.favorite == true, showMenu)
  }
  DeleteContactAction(chat, chatModel, showMenu)
}

@Composable
fun ToggleFavoritesChatAction(chat: Chat, chatModel: ChatModel, favorite: Boolean, showMenu: MutableState<Boolean>) {
  ItemAction(
    if (favorite) stringResource(MR.strings.unfavorite_chat) else stringResource(MR.strings.favorite_chat),
    if (favorite) painterResource(MR.images.ic_star_off) else painterResource(MR.images.ic_star),
    onClick = {
      toggleChatFavorite(chat, !favorite, chatModel)
      showMenu.value = false
    }
  )
}

@Composable
fun DeleteContactAction(chat: Chat, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.delete_contact_menu_action),
    painterResource(MR.images.ic_delete),
    onClick = {
      deleteContactDialog(chat, chatModel)
      showMenu.value = false
    },
    color = Color.Red
  )
}

// TODO differentiate android and desktop (dividers)
@Composable
fun ContactListNavLinkLayout(
  chatLinkPreview: @Composable () -> Unit,
  click: () -> Unit,
  dropdownMenuItems: (@Composable () -> Unit)?,
  showMenu: MutableState<Boolean>,
  disabled: Boolean,
  selectedChat: State<Boolean>,
  nextChatSelected: State<Boolean>,
) {
  var modifier = Modifier.fillMaxWidth()
  if (!disabled) modifier = modifier
    .combinedClickable(onClick = click, onLongClick = { showMenu.value = true })
    .onRightClick { showMenu.value = true }
  Box(modifier) {
    Row(
      modifier = Modifier
        .fillMaxWidth()
        .padding(start = 8.dp, top = 8.dp, end = 12.dp, bottom = 8.dp),
      verticalAlignment = Alignment.Top
    ) {
      chatLinkPreview()
    }
    if (dropdownMenuItems != null) {
      DefaultDropdownMenu(showMenu, dropdownMenuItems = dropdownMenuItems)
    }
  }
  Divider(Modifier.padding(horizontal = 8.dp))
}
