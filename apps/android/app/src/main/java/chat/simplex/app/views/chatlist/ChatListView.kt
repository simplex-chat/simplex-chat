package chat.simplex.app.views.chatlist

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.PersonAdd
import androidx.compose.material.icons.outlined.Settings
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import androidx.navigation.NavOptions
import chat.simplex.app.Pages
import chat.simplex.app.model.Chat
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.newchat.NewChatSheet
import chat.simplex.app.views.usersettings.SettingsView
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

  fun toggleDrawer() = scope.launch {
    state.drawerState.apply {
      if (isClosed) open() else close()
    }
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
  val scaffoldCtrl = scaffoldController()
  BottomSheetScaffold(
    scaffoldState = scaffoldCtrl.state,
    topBar = {
      ChatListToolbar(
        scaffoldCtrl,
        settings = { scaffoldCtrl.toggleDrawer() }
      )
    },
    drawerContent = {
      SettingsView(chatModel, nav)
    },
    sheetPeekHeight = 0.dp,
    sheetContent = { NewChatSheet(chatModel, scaffoldCtrl, nav) },
    sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp),
  ) {
    Column(
      modifier = Modifier
        .padding(vertical = 8.dp)
        .fillMaxSize()
        .background(MaterialTheme.colors.background)
    ) {
      ChatList(chatModel, nav)
    }
    if (scaffoldCtrl.state.bottomSheetState.isExpanded) {
      Surface(
        Modifier
          .fillMaxSize()
          .clickable { scaffoldCtrl.collapse() },
        color = Color.Black.copy(alpha = 0.12F)
      ) {}
    }
  }
}

@ExperimentalMaterialApi
@Composable
fun ChatListToolbar(newChatSheetCtrl: ScaffoldController, settings: () -> Unit) {
  Row(
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically,
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = 8.dp)
      .height(60.dp)
  ) {
    IconButton(onClick = settings) {
      Icon(
        Icons.Outlined.Settings,
        "Settings",
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
    Text(
      "Your chats",
      color = MaterialTheme.colors.onBackground,
      fontWeight = FontWeight.Bold,
      modifier = Modifier.padding(5.dp)
    )
    IconButton(onClick = { newChatSheetCtrl.toggle() }) {
      Icon(
        Icons.Outlined.PersonAdd,
        "Add Contact",
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
  }
}

@DelicateCoroutinesApi
fun goToChat(chatPreview: Chat, chatModel: ChatModel, navController: NavController) {
  withApi {
    val cInfo = chatPreview.chatInfo
    val chat = chatModel.controller.apiGetChat(cInfo.chatType, cInfo.apiId)
    if (chat != null) {
      chatModel.chatId.value = cInfo.id
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
      ChatPreviewView(chat) { goToChat(chat, chatModel, navController) }
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
