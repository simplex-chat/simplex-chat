package chat.simplex.app.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.Circle
import androidx.compose.material.icons.outlined.Delete
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun ChatInfoView(chatModel: ChatModel, nav: NavController) {
  val chat = chatModel.chats.firstOrNull { it.id == chatModel.chatId.value }
  if (chat != null) {
    ChatInfoLayout(chat,
      close = nav::popBackStack,
      deleteContact = {
        chatModel.alertManager.showAlertMsg(
          title = "Delete contact?",
          text = "Contact and all messages will be deleted - this cannot be undone!",
          confirmText = "Delete",
          onConfirm = {
            val cInfo = chat.chatInfo
            withApi {
              val r = chatModel.controller.apiDeleteChat(cInfo.chatType, cInfo.apiId)
              if (r) {
                chatModel.removeChat(cInfo.id)
                chatModel.chatId.value = null
                nav.popBackStack()
              }
            }
          }
        )
      }
    )
  }
}

@Composable
fun ChatInfoLayout(chat: Chat, close: () -> Unit, deleteContact: () -> Unit) {
  Column(Modifier
    .fillMaxSize()
    .background(MaterialTheme.colors.background)
    .padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    CloseSheetBar(close)
    Spacer(Modifier.size(48.dp))
    ChatInfoImage(chat, size = 192.dp)
    val cInfo = chat.chatInfo
    Text(
      cInfo.displayName, style = MaterialTheme.typography.h1,
      color = MaterialTheme.colors.onBackground,
      modifier = Modifier.padding(top = 32.dp).padding(bottom = 8.dp)
    )
    Text(
      cInfo.fullName, style = MaterialTheme.typography.h2,
      color = MaterialTheme.colors.onBackground,
      modifier = Modifier.padding(bottom = 16.dp)
    )

    if (cInfo is ChatInfo.Direct) {
      Column(horizontalAlignment = Alignment.CenterHorizontally) {
        Row(Modifier.padding(horizontal = 32.dp)) {
          ServerImage(chat)
          Text(
            chat.serverInfo.networkStatus.statusString,
            textAlign = TextAlign.Center,
            color = MaterialTheme.colors.onBackground,
            modifier = Modifier.padding(start = 8.dp)
          )
        }
        Text(
          chat.serverInfo.networkStatus.statusExplanation,
          style = MaterialTheme.typography.body2,
          color = MaterialTheme.colors.onBackground,
          textAlign = TextAlign.Center,
          modifier = Modifier.padding(top = 16.dp).padding(horizontal = 16.dp)
        )
      }

      Spacer(Modifier.weight(1F))

      Box(Modifier.padding(24.dp)) {
        SimpleButton(
          "Delete contact", icon = Icons.Outlined.Delete,
          color = Color.Red,
          click = deleteContact
        )
      }
    }
  }
}

@Composable
fun ServerImage(chat: Chat) {
  val status = chat.serverInfo.networkStatus
  when {
    status is Chat.NetworkStatus.Connected ->
      Icon(Icons.Filled.Circle, "Connected", tint = MaterialTheme.colors.primaryVariant)
    status is Chat.NetworkStatus.Disconnected ->
      Icon(Icons.Filled.Pending, "Disconnected", tint = HighOrLowlight)
    status is Chat.NetworkStatus.Error ->
      Icon(Icons.Filled.Error, "Error", tint = HighOrLowlight)
    else ->
      Icon(Icons.Outlined.Circle, "Pending", tint = HighOrLowlight)
  }
}

@Preview
@Composable
fun PreviewChatInfoLayout() {
  SimpleXTheme {
    ChatInfoLayout(
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = arrayListOf(),
        serverInfo = Chat.ServerInfo(Chat.NetworkStatus.Error("agent BROKER TIMEOUT"))
      ),
      close = {}, deleteContact = {}
    )
  }
}
