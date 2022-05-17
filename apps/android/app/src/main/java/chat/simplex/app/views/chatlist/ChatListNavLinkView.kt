package chat.simplex.app.views.chatlist

import android.content.res.Configuration
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Delete
import androidx.compose.material.icons.outlined.Restore
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.ItemAction
import chat.simplex.app.views.helpers.*
import kotlinx.datetime.Clock

@Composable
fun ChatListNavLinkView(chat: Chat, chatModel: ChatModel) {
  ChatListNavLinkLayout(
    chat = chat,
    click = {
      when (chat.chatInfo) {
        is ChatInfo.ContactRequest -> contactRequestAlertDialog(chat.chatInfo, chatModel)
        is ChatInfo.ContactConnection -> contactConnectionAlertDialog(chat.chatInfo.contactConnection, chatModel)
        else ->
          if (chat.chatInfo.ready) {
            withApi { openChat(chatModel, chat.chatInfo) }
          } else {
            pendingContactAlertDialog(chat.chatInfo, chatModel)
          }
      }
    },
    deleteContact = {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(R.string.delete_contact__question),
        text = generalGetString(R.string.delete_contact_all_messages_deleted_cannot_undo_warning),
        confirmText = generalGetString(R.string.delete_verb),
        onConfirm = {
          val cInfo = chat.chatInfo
          withApi {
            val r = chatModel.controller.apiDeleteChat(cInfo.chatType, cInfo.apiId)
            if (r) {
              chatModel.removeChat(cInfo.id)
              chatModel.chatId.value = null
            }
          }
        }
      )
    },
    clearChat = {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(R.string.clear_chat_question),
        text = generalGetString(R.string.clear_chat_warning),
        confirmText = generalGetString(R.string.clear_verb),
        onConfirm = {
          val cInfo = chat.chatInfo
          withApi {
            val r = chatModel.controller.apiClearChat(cInfo.chatType, cInfo.apiId)
            if (r) {
              chatModel.clearChat(cInfo)
            }
          }
        }
      )
    },
  )
}

suspend fun openChat(chatModel: ChatModel, cInfo: ChatInfo) {
  val chat = chatModel.controller.apiGetChat(cInfo.chatType, cInfo.apiId)
  if (chat != null) {
    chatModel.chatItems.clear()
    chatModel.chatItems.addAll(chat.chatItems)
    chatModel.chatId.value = cInfo.id
  }
}

fun contactRequestAlertDialog(contactRequest: ChatInfo.ContactRequest, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.accept_connection_request__question),
    text = generalGetString(R.string.if_you_choose_to_reject_the_sender_will_not_be_notified),
    confirmText = generalGetString(R.string.accept_contact_button),
    onConfirm = {
      withApi {
        val contact = chatModel.controller.apiAcceptContactRequest(contactRequest.apiId)
        if (contact != null) {
          val chat = Chat(ChatInfo.Direct(contact), listOf())
          chatModel.replaceChat(contactRequest.id, chat)
        }
      }
    },
    dismissText = generalGetString(R.string.reject_contact_button),
    onDismiss = {
      withApi {
        chatModel.controller.apiRejectContactRequest(contactRequest.apiId)
        chatModel.removeChat(contactRequest.id)
      }
    }
  )
}

fun contactConnectionAlertDialog(connection: PendingContactConnection, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialogButtons(
    title = generalGetString(
      if (connection.initiated) R.string.you_invited_your_contact
      else R.string.you_accepted_connection
    ),
    text = generalGetString(
      if (connection.viaContactUri) R.string.you_will_be_connected_when_your_connection_request_is_accepted
      else R.string.you_will_be_connected_when_your_contacts_device_is_online
    ),
    buttons = {
      Row(
        Modifier
          .fillMaxWidth()
          .padding(horizontal = 8.dp, vertical = 2.dp),
        horizontalArrangement = Arrangement.End,
      ) {
        Button(onClick = {
          AlertManager.shared.hideAlert()
          deleteContactConnectionAlert(connection, chatModel)
        }) {
          Text(stringResource(R.string.delete_verb))
        }
        Spacer(Modifier.padding(horizontal = 4.dp))
        Button(onClick = { AlertManager.shared.hideAlert() }) {
          Text(stringResource(R.string.ok))
        }
      }
    }
  )
}

fun deleteContactConnectionAlert(connection: PendingContactConnection, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.delete_pending_connection__question),
    text = generalGetString(
      if (connection.initiated) R.string.contact_you_shared_link_with_wont_be_able_to_connect
      else R.string.connection_you_accepted_will_be_cancelled
    ),
    confirmText = generalGetString(R.string.delete_verb),
    onConfirm = {
      withApi {
        AlertManager.shared.hideAlert()
        if (chatModel.controller.apiDeleteChat(ChatType.ContactConnection, connection.apiId)) {
          chatModel.removeChat(connection.id)
        }
      }
    }
  )
}

fun pendingContactAlertDialog(chatInfo: ChatInfo, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.alert_title_contact_connection_pending),
    text = generalGetString(R.string.alert_text_connection_pending_they_need_to_be_online_can_delete_and_retry),
    confirmText = generalGetString(R.string.button_delete_contact),
    onConfirm = {
      withApi {
        val r = chatModel.controller.apiDeleteChat(chatInfo.chatType, chatInfo.apiId)
        if (r) {
          chatModel.removeChat(chatInfo.id)
          chatModel.chatId.value = null
        }
      }
    },
    dismissText = generalGetString(R.string.cancel_verb),
  )
}

@Composable
fun ChatListNavLinkLayout(
  chat: Chat,
  click: () -> Unit,
  deleteContact: () -> Unit,
  clearChat: () -> Unit
) {
  val showMenu = remember { mutableStateOf(false) }
  Surface(
    modifier = Modifier
      .fillMaxWidth()
      .combinedClickable(
        onClick = click,
        onLongClick = {
          if (chat.chatInfo is ChatInfo.Direct || chat.chatInfo is ChatInfo.Group)
            showMenu.value = true
        }
      )
      .height(88.dp)
  ) {
    Row(
      modifier = Modifier
        .fillMaxWidth()
        .padding(vertical = 8.dp)
        .padding(start = 8.dp)
        .padding(end = 12.dp),
      verticalAlignment = Alignment.Top
    ) {
      when (chat.chatInfo) {
        is ChatInfo.Direct -> ChatPreviewView(chat)
        is ChatInfo.Group -> ChatPreviewView(chat)
        is ChatInfo.ContactRequest -> ContactRequestView(chat.chatInfo)
        is ChatInfo.ContactConnection -> ContactConnectionView(chat.chatInfo.contactConnection)
      }
    }
    Box(Modifier.padding(horizontal = 16.dp)) {
      DropdownMenu(
        expanded = showMenu.value,
        onDismissRequest = { showMenu.value = false },
        Modifier.width(220.dp)
      ) {
        if (chat.chatInfo is ChatInfo.Direct || chat.chatInfo is ChatInfo.Group)
          ItemAction(
            stringResource(R.string.clear_verb),
            Icons.Outlined.Restore,
            onClick = {
              clearChat()
              showMenu.value = false
            }
          )
        if (chat.chatInfo is ChatInfo.Direct) {
          ItemAction(
            stringResource(R.string.delete_verb),
            Icons.Outlined.Delete,
            onClick = {
              deleteContact()
              showMenu.value = false
            },
            color = Color.Red
          )
        }
      }
    }
  }
  Divider(Modifier.padding(horizontal = 8.dp))
}

@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatListNavLinkDirect() {
  SimpleXTheme {
    ChatListNavLinkLayout(
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = listOf(
          ChatItem.getSampleData(
            1,
            CIDirection.DirectSnd(),
            Clock.System.now(),
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
          )
        ),
        chatStats = Chat.ChatStats()
      ),
      click = {},
      deleteContact = {},
      clearChat = {}
    )
  }
}

@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatListNavLinkGroup() {
  SimpleXTheme {
    ChatListNavLinkLayout(
      chat = Chat(
        chatInfo = ChatInfo.Group.sampleData,
        chatItems = listOf(
          ChatItem.getSampleData(
            1,
            CIDirection.DirectSnd(),
            Clock.System.now(),
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
          )
        ),
        chatStats = Chat.ChatStats()
      ),
      click = {},
      deleteContact = {},
      clearChat = {}
    )
  }
}

@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatListNavLinkContactRequest() {
  SimpleXTheme {
    ChatListNavLinkLayout(
      chat = Chat(
        chatInfo = ChatInfo.ContactRequest.sampleData,
        chatItems = listOf(),
        chatStats = Chat.ChatStats()
      ),
      click = {},
      deleteContact = {},
      clearChat = {}
    )
  }
}
