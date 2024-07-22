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
    val loadedChat = remember { mutableStateOf<Chat?>(null) }
    val rhId = chat.remoteHostId
    val disabled = chatModel.chatRunning.value == false || chatModel.deletedChats.value.contains(rhId to chat.chatInfo.id)
    val contactType = getContactType(chat)

    LaunchedEffect(chat.id) {
        showMenu.value = false
        delay(500L)
    }

    LaunchedEffect(contactType) {
        if (contactType == ContactType.RECENT) {
            withBGApi {
                loadedChat.value = chatModel.controller.apiGetChat(rhId, chat.chatInfo.chatType, chat.chatInfo.apiId)
            }
        }
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
                    when (contactType) {
                        ContactType.RECENT -> {
                            loadedChat.value?.let {
                                openLoadedChat(it, chatModel)
                            } ?: run {
                                withApi {
                                    openChat(rhId, chat.chatInfo, chatModel)
                                }
                            }

                            ModalManager.start.closeModals()
                        }
                        ContactType.REMOVED -> {
                            openLoadedChat(chat, chatModel)
                            ModalManager.start.closeModals()
                        }
                        ContactType.CARD -> {
                            directChatAction(
                                chatModel = chatModel,
                                rhId = rhId,
                                contact = chat.chatInfo.contact
                            )
                            ModalManager.start.closeModals()
                        }
                        else -> {}
                    }
                },
                dropdownMenuItems = {
                    tryOrShowError("${chat.id}ContactListNavLinkDropdown", error = {}) {
                        DeleteContactAction(chat, chatModel, showMenu)
                    }
                },
                showMenu,
                disabled,
                selectedChat,
                nextChatSelected,
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
                        rhId,
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
            )
        }
        else -> {}
    }
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