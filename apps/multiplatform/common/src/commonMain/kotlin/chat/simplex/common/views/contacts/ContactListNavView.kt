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
fun ContactListNavLinkView(chat: Chat, nextChatSelected: State<Boolean>, oneHandUI: State<Boolean>) {
    val showMenu = remember { mutableStateOf(false) }
    val disabled = chatModel.chatRunning.value == false || chatModel.deletedChats.value.contains(chat.remoteHostId to chat.chatInfo.id)
    LaunchedEffect(chat.id) {
        showMenu.value = false
        delay(500L)
    }
    val selectedChat = remember(chat.id) { derivedStateOf { chat.id == chatModel.chatId.value } }

    when (chat.chatInfo) {
        is ChatInfo.Direct -> {
            ChatListNavLinkLayout(
                chatLinkPreview = {
                    tryOrShowError("${chat.id}ContactListNavLink", error = { ErrorChatListItem() }) {
                        ContactPreviewView(chat, disabled)
                    }
                },
                click = {
                    directChatAction(chat.remoteHostId, chat.chatInfo.contact, chatModel)
                    ModalManager.start.closeModals()
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
                oneHandUI
            )
        }
        is ChatInfo.ContactRequest -> {
            ChatListNavLinkLayout(
                chatLinkPreview = {
                    tryOrShowError("${chat.id}ContactListNavLink", error = { ErrorChatListItem() }) {
                        ContactPreviewView(chat, disabled)
                    }
                },
                click = {
                    contactRequestAlertDialog(
                        chat.remoteHostId,
                        chat.chatInfo,
                        chatModel,
                        onSuccess = {
                            ModalManager.start.closeModals()
                        }
                    )
                },
                dropdownMenuItems = {
                    tryOrShowError("${chat.id}ContactListNavLinkDropdown", error = {}) {
                        ContactRequestMenuItems(chat.remoteHostId, chat.chatInfo, chatModel, showMenu)
                    }
                },
                showMenu,
                disabled,
                selectedChat,
                nextChatSelected,
                oneHandUI
            )
        }
        is ChatInfo.ContactConnection -> {
            ChatListNavLinkLayout(
                chatLinkPreview = {
                    tryOrShowError("${chat.id}ContactListNavLink", error = { ErrorChatListItem() }) {
                        ContactPreviewView(chat, disabled)
                    }
                },
                click = {
                    contactRequestAlertDialog(
                        rhId = chat.remoteHostId,
                        connection = chat.chatInfo.contactConnection
                    )
                },
                dropdownMenuItems = {},
                showMenu,
                disabled,
                selectedChat,
                nextChatSelected,
                oneHandUI
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

fun contactRequestAlertDialog(rhId: Long?, connection: PendingContactConnection) {
    AlertManager.shared.showAlertMsg(
        title = "Connection not yet accepted",
        text = "${connection.displayName} didn't accept your connection request yet.",
        hostDevice = hostDevice(rhId),
    )
}