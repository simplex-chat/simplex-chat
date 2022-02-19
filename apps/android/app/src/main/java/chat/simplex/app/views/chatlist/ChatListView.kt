package chat.simplex.app.views.chatlist

import androidx.compose.foundation.clickable
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.model.*
import kotlinx.coroutines.launch
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.newchat.NewChatSheet
import com.google.accompanist.permissions.ExperimentalPermissionsApi
import kotlinx.coroutines.*


@ExperimentalMaterialApi
class ScaffoldController(val state: BottomSheetScaffoldState, val scope: CoroutineScope) {
  fun expand() = scope.launch { state.bottomSheetState.expand() }
  fun collapse() = scope.launch { state.bottomSheetState.collapse() }
  fun toggle() = scope.launch {
    val s = state.bottomSheetState
    if (s.isExpanded) s.collapse() else s.expand()
  }
}

@ExperimentalMaterialApi
@Composable
fun scaffoldController(): ScaffoldController {
  return ScaffoldController(
    state = rememberBottomSheetScaffoldState(),
    scope = rememberCoroutineScope()
  )
}

@DelicateCoroutinesApi
@ExperimentalPermissionsApi
@ExperimentalMaterialApi
@Composable
fun ChatListView(chatModel: ChatModel, nav: NavController) {
  val newChatCtrl = scaffoldController()
  BottomSheetScaffold(
    scaffoldState = newChatCtrl.state,
    sheetPeekHeight = 0.dp,
    sheetContent = { NewChatSheet(chatModel, newChatCtrl, nav) },
    sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp),
  ) {
    Box {
      Column(
        modifier = Modifier
          .padding(vertical = 8.dp)
          .fillMaxSize()
          .background(MaterialTheme.colors.background)
      ) {
        ChatListToolbar(newChatCtrl)
        ChatList(chatModel, nav)
        Button(
          onClick = { nav.navigate(Pages.Terminal.route) },
          modifier = Modifier.padding(14.dp)
        ) {
          Text("Terminal")
        }
      }
      if (newChatCtrl.state.bottomSheetState.isExpanded) {
        Surface(Modifier
          .fillMaxSize()
          .clickable { newChatCtrl.collapse() },
          color = Color.Black.copy(alpha = 0.12F)
        ) {}
      }
    }
  }
}

@ExperimentalMaterialApi
@Composable
fun ChatListToolbar(newChatSheetCtrl: ScaffoldController) {
  Row(
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically,
    modifier = Modifier
      .fillMaxWidth()
      .height(40.dp)) {
    Icon(
      Icons.Outlined.Settings,
      "Settings Cog",
      tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(horizontal = 10.dp)
    )
    Text(
      "Your chats",
      color = MaterialTheme.colors.onBackground,
      fontWeight = FontWeight.Bold,
      modifier = Modifier.padding(5.dp)
    )
    Icon(
      Icons.Outlined.PersonAdd,
      "Add Contact",
      tint = MaterialTheme.colors.primary,
      modifier = Modifier
        .padding(horizontal = 10.dp)
        .clickable { newChatSheetCtrl.toggle() }
    )
  }
}


@DelicateCoroutinesApi
fun goToChat(chatPreview: Chat, chatModel: ChatModel, navController: NavController) {
  withApi {
    val cInfo = chatPreview.chatInfo
    val chat = chatModel.controller.apiGetChat(cInfo.chatType, cInfo.apiId)
    if (chat != null ) {
      chatModel.chatId = mutableStateOf(cInfo.id)
      chatModel.chatItems = chat.chatItems.toMutableStateList()
      navController.navigate(Pages.Chat.route)
    } else {
      // TODO show error? or will apiGetChat show it
    }
  }
}

@DelicateCoroutinesApi
@Composable
fun ChatList(chatModel: ChatModel, navController: NavController) {
  LazyColumn(
    modifier = Modifier.fillMaxWidth()
  ) {
    items(chatModel.chats) { chat ->
      ChatPreviewView(chat) {goToChat(chat, chatModel, navController)}
    }
  }
}


//@Preview
//@Composable
//fun PreviewChatListView() {
//  SimpleXTheme {
//    ChatListView(
//      chats = listOf(
//        Chat()
//      ),
//
//    )
//  }
//}
