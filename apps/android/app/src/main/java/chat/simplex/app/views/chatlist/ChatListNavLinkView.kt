package chat.simplex.app.views.chatlist

import android.content.res.Configuration
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.Divider
import androidx.compose.material.Surface
import androidx.compose.runtime.Composable
import androidx.compose.runtime.toMutableStateList
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import kotlinx.datetime.Clock

@Composable
fun ChatListNavLinkView(chat: Chat, chatModel: ChatModel) {
  ChatListNavLinkLayout(
    chat = chat,
    click = {
      if (chat.chatInfo is ChatInfo.ContactRequest) {
        contactRequestAlertDialog(chat.chatInfo, chatModel)
      } else {
        withApi { openChat(chatModel, chat.chatInfo) }
      }
    }
  )
}

suspend fun openChat(chatModel: ChatModel, cInfo: ChatInfo) {
  val chat = chatModel.controller.apiGetChat(cInfo.chatType, cInfo.apiId)
  if (chat != null) {
    chatModel.chatItems = chat.chatItems.toMutableStateList()
    chatModel.chatId.value = cInfo.id
  }
}

fun contactRequestAlertDialog(contactRequest: ChatInfo.ContactRequest, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.accept_connection_request__question),
    text = generalGetString(R.string.if_you_choose_to_reject_the_sender_will_not_be_notified),
    confirmText = generalGetString(R.string.accept_verb),
    onConfirm = {
      withApi {
        val contact = chatModel.controller.apiAcceptContactRequest(contactRequest.apiId)
        if (contact != null) {
          val chat = Chat(ChatInfo.Direct(contact), listOf())
          chatModel.replaceChat(contactRequest.id, chat)
        }
      }
    },
    dismissText = generalGetString(R.string.reject_verb),
    onDismiss = {
      withApi {
        chatModel.controller.apiRejectContactRequest(contactRequest.apiId)
        chatModel.removeChat(contactRequest.id)
      }
    }
  )
}

@Composable
fun ChatListNavLinkLayout(chat: Chat, click: () -> Unit) {
  Surface(
    modifier = Modifier
      .fillMaxWidth()
      .clickable(onClick = click)
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
      if (chat.chatInfo is ChatInfo.ContactRequest) {
        ContactRequestView(chat)
      } else {
        ChatPreviewView(chat)
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
      click = {}
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
      click = {}
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
      click = {}
    )
  }
}
