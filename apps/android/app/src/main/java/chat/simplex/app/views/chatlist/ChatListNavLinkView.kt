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
import androidx.compose.ui.text.ExperimentalTextApi
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.withApi
import kotlinx.coroutines.DelicateCoroutinesApi
import kotlinx.datetime.Clock

@ExperimentalTextApi
@Composable
fun ChatListNavLinkView(chat: Chat, chatModel: ChatModel, nav: NavController) {
  ChatListNavLink(
    chat = chat,
    action = {
      when (chat.chatInfo) {
        is ChatInfo.Direct -> chatNavLink(chat, chatModel, nav)
        is ChatInfo.Group -> chatNavLink(chat, chatModel, nav)
        is ChatInfo.ContactRequest -> contactRequestNavLink(chat.chatInfo, chatModel, nav)
      }
    }
  )
}

@DelicateCoroutinesApi
fun chatNavLink(chatPreview: Chat, chatModel: ChatModel, navController: NavController) {
  withApi {
    val chatInfo = chatPreview.chatInfo
    val chat = chatModel.controller.apiGetChat(chatInfo.chatType, chatInfo.apiId)
    if (chat != null) {
      chatModel.chatId.value = chatInfo.id
      chatModel.chatItems = chat.chatItems.toMutableStateList()
      navController.navigate(Pages.Chat.route)
    } else {
      // TODO show error? or will apiGetChat show it
    }
  }
}

@DelicateCoroutinesApi
fun contactRequestNavLink(contactRequest: ChatInfo.ContactRequest, chatModel: ChatModel, navController: NavController) {
  chatModel.alertManager.showAlertDialog(
    title = "Accept connection request?",
    text = "If you choose to reject sender will NOT be notified",
    confirmText = "Accept",
    onConfirm = {
      withApi {
        val contact = chatModel.controller.apiAcceptContactRequest(contactRequest.apiId)
        if (contact != null) {
          val chat = Chat(ChatInfo.Direct(contact), listOf())
          chatModel.replaceChat(contactRequest.id, chat)
        }
      }
    },
    dismissText = "Reject",
    onDismiss = {
      withApi {
        chatModel.controller.apiRejectContactRequest(contactRequest.apiId)
        chatModel.removeChat(contactRequest.id)
      }
    }
  )
}

@ExperimentalTextApi
@Composable
fun ChatListNavLink(chat: Chat, action: () -> Unit) {
  ChatListNavLinkLayout(
    content = {
      when (chat.chatInfo) {
        is ChatInfo.Direct -> ChatPreviewView(chat)
        is ChatInfo.Group -> ChatPreviewView(chat)
        is ChatInfo.ContactRequest -> ContactRequestView(chat)
      }
    },
    action = action
  )
}

@Composable
fun ChatListNavLinkLayout(content: (@Composable () -> Unit), action: () -> Unit) {
  Surface(
    modifier = Modifier
      .fillMaxWidth()
      .clickable(onClick = action)
      .height(88.dp)
  ) {
    Row(
      modifier = Modifier
        .fillMaxWidth()
        .padding(vertical = 8.dp)
        .padding(start = 8.dp)
        .padding(end = 12.dp),
      verticalAlignment = Alignment.Top,
//      TODO?
//      verticalAlignment = Alignment.CenterVertically,
//      horizontalArrangement = Arrangement.SpaceEvenly
    ) {
      content.invoke()
    }
  }
  Divider(Modifier.padding(horizontal = 8.dp))
}

@ExperimentalTextApi
@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatListNavLinkDirect() {
  SimpleXTheme {
    ChatListNavLink(
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
      action = {}
    )
  }
}

@ExperimentalTextApi
@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatListNavLinkGroup() {
  SimpleXTheme {
    ChatListNavLink(
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
      action = {}
    )
  }
}

@ExperimentalTextApi
@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatListNavLinkContactRequest() {
  SimpleXTheme {
    ChatListNavLink(
      chat = Chat(
        chatInfo = ChatInfo.ContactRequest.sampleData,
        chatItems = listOf(),
        chatStats = Chat.ChatStats()
      ),
      action = {}
    )
  }
}
