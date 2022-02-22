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
import chat.simplex.app.Pages
import chat.simplex.app.model.Chat
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.chat.ChatHelpView
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.newchat.NewChatSheet
import chat.simplex.app.views.usersettings.SettingsView
import com.google.accompanist.permissions.ExperimentalPermissionsApi
import kotlinx.coroutines.*

@ExperimentalMaterialApi
class ScaffoldController(val state: BottomSheetScaffoldState, val scope: CoroutineScope) {
  fun expand() = scope.launch { state.bottomSheetState.expand() }
  fun collapse() = scope.launch { state.bottomSheetState.collapse() }
  fun toggleSheet() = scope.launch {
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
      ChatListToolbar(scaffoldCtrl)
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
      when (chatModel.chatsLoaded.value) {
        true -> if (chatModel.chats.isNotEmpty()) {
          ChatList(chatModel, nav)
        } else {
          val user = chatModel.currentUser.value
          Help(scaffoldCtrl, displayName = user?.profile?.displayName)
        }
        else -> ChatList(chatModel, nav)
      }
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
fun Help(scaffoldCtrl: ScaffoldController, displayName: String?) {
  Column(
    Modifier
      .fillMaxWidth()
      .padding(8.dp)
  ) {
    Text(
      text = if (displayName != null) "Welcome ${displayName}!" else "Welcome!",
      Modifier.padding(bottom = 24.dp),
      style = MaterialTheme.typography.h1,
      color = MaterialTheme.colors.onBackground
    )
    ChatHelpView({ scaffoldCtrl.toggleSheet() }, true)
    Row(
      Modifier.padding(top = 30.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(8.dp)
    ) {
      Text(
        "This text is available in settings",
        color = MaterialTheme.colors.onBackground
      )
      Icon(
        Icons.Outlined.Settings,
        "Settings",
        tint = MaterialTheme.colors.onBackground,
        modifier = Modifier.clickable(onClick = { scaffoldCtrl.toggleDrawer() })
      )
    }
  }
}

@ExperimentalMaterialApi
@Composable
fun ChatListToolbar(scaffoldCtrl: ScaffoldController) {
  Row(
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically,
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = 8.dp)
      .height(60.dp)
  ) {
    IconButton(onClick = { scaffoldCtrl.toggleDrawer() }) {
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
    IconButton(onClick = { scaffoldCtrl.toggleSheet() }) {
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
