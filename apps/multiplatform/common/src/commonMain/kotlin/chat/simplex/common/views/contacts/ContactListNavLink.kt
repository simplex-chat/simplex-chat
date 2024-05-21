package chat.simplex.common.views.contacts

import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
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
  val view = LocalMultiplatformView()

  when (chat.chatInfo) {
    is ChatInfo.Direct -> {
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ContactListNavLink", error = { ErrorChatListItem() }) {
            ContactPreviewView(chat, disabled)
          }
        },
        click = {
          val chatRh = chat.remoteHostId
          if (ModalManager.end.hasModalsOpen()) {
            ModalManager.end.closeModals()
            // return@ChatLayout -- TODO what's this for?
          }
          hideKeyboard(view)
          withBGApi {
            var preloadedContactInfo: Pair<ConnectionStats?, Profile?>? = chatModel.controller.apiContactInfo(chatRh, chat.chatInfo.apiId)
            var preloadedCode: String? = chatModel.controller.apiGetContactCode(chatRh, chat.chatInfo.apiId)?.second
            ModalManager.end.showModalCloseable(true) { close ->
              var contactInfo: Pair<ConnectionStats?, Profile?>? by remember { mutableStateOf(preloadedContactInfo) }
              var code: String? by remember { mutableStateOf(preloadedCode) }
              KeyChangeEffect(chat.id, ChatModel.networkStatuses.toMap()) {
                contactInfo = chatModel.controller.apiContactInfo(chatRh, chat.chatInfo.apiId)
                preloadedContactInfo = contactInfo
                code = chatModel.controller.apiGetContactCode(chatRh, chat.chatInfo.apiId)?.second
                preloadedCode = code
              }
              ChatInfoView(chatModel, chat.chatInfo.contact, contactInfo?.first, contactInfo?.second, chat.chatInfo.localAlias, code, close)
            }
          }
        },
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
