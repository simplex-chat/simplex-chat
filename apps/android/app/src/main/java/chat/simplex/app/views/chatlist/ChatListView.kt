package chat.simplex.app.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.ToolbarDark
import chat.simplex.app.ui.theme.ToolbarLight
import chat.simplex.app.views.helpers.ModalManager
import chat.simplex.app.views.helpers.generalGetString
import chat.simplex.app.views.newchat.NewChatSheet
import chat.simplex.app.views.usersettings.SettingsView
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.launch

class ScaffoldController(val scope: CoroutineScope) {
  lateinit var state: BottomSheetScaffoldState
  val expanded = mutableStateOf(false)

  fun expand() {
    expanded.value = true
    scope.launch { state.bottomSheetState.expand() }
  }

  fun collapse() {
    expanded.value = false
    scope.launch { state.bottomSheetState.collapse() }
  }

  fun toggleSheet() {
    if (state.bottomSheetState.isExpanded) collapse() else expand()
  }

  fun toggleDrawer() = scope.launch {
    state.drawerState.apply { if (isClosed) open() else close() }
  }
}

@Composable
fun scaffoldController(): ScaffoldController {
  val ctrl = ScaffoldController(scope = rememberCoroutineScope())
  val bottomSheetState = rememberBottomSheetState(
    BottomSheetValue.Collapsed,
    confirmStateChange = {
      ctrl.expanded.value = it == BottomSheetValue.Expanded
      true
    }
  )
  ctrl.state = rememberBottomSheetScaffoldState(bottomSheetState = bottomSheetState)
  return ctrl
}

@Composable
fun ChatListView(chatModel: ChatModel) {
  val scaffoldCtrl = scaffoldController()
  if (chatModel.clearOverlays.value) {
    scaffoldCtrl.collapse()
    ModalManager.shared.closeModal()
    chatModel.clearOverlays.value = false
  }
  BottomSheetScaffold(
    scaffoldState = scaffoldCtrl.state,
    drawerContent = { SettingsView(chatModel) },
    sheetPeekHeight = 0.dp,
    sheetContent = { NewChatSheet(chatModel, scaffoldCtrl) },
    sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp),
  ) {
    Box {
      Column(
        modifier = Modifier
          .fillMaxSize()
          .background(MaterialTheme.colors.background)
      ) {
        ChatListToolbar(scaffoldCtrl)
        Divider()
        if (chatModel.chats.isNotEmpty()) {
          ChatList(chatModel)
        } else {
          val user = chatModel.currentUser.value
          Help(scaffoldCtrl, displayName = user?.profile?.displayName)
        }
      }
      if (scaffoldCtrl.expanded.value) {
        Surface(
          Modifier
            .fillMaxSize()
            .clickable { scaffoldCtrl.collapse() },
          color = Color.Black.copy(alpha = 0.12F)
        ) {}
      }
    }
  }
}

@Composable
fun Help(scaffoldCtrl: ScaffoldController, displayName: String?) {
  Column(
    Modifier
      .fillMaxWidth()
      .padding(16.dp)
  ) {
    val welcomeMsg = if (displayName != null) {
      String.format(generalGetString(R.string.personal_welcome), displayName)
    } else generalGetString(R.string.welcome)
    Text(
      text = welcomeMsg,
      Modifier.padding(bottom = 24.dp),
      style = MaterialTheme.typography.h1,
      color = MaterialTheme.colors.onBackground
    )
    ChatHelpView { scaffoldCtrl.toggleSheet() }
    Row(
      Modifier.padding(top = 30.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(8.dp)
    ) {
      Text(
        generalGetString(R.string.this_text_is_available_in_settings),
        color = MaterialTheme.colors.onBackground
      )
      Icon(
        Icons.Outlined.Settings,
        generalGetString(R.string.icon_descr_settings),
        tint = MaterialTheme.colors.onBackground,
        modifier = Modifier.clickable(onClick = { scaffoldCtrl.toggleDrawer() })
      )
    }
  }
}

@Composable
fun ChatListToolbar(scaffoldCtrl: ScaffoldController) {
  Row(
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically,
    modifier = Modifier
      .fillMaxWidth()
      .height(52.dp)
      .background(if (isSystemInDarkTheme()) ToolbarDark else ToolbarLight)
      .padding(horizontal = 8.dp)
  ) {
    IconButton(onClick = { scaffoldCtrl.toggleDrawer() }) {
      Icon(
        Icons.Outlined.Menu,
        generalGetString(R.string.icon_descr_settings),
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
    Text(
      generalGetString(R.string.your_chats),
      color = MaterialTheme.colors.onBackground,
      fontWeight = FontWeight.SemiBold,
      modifier = Modifier.padding(5.dp)
    )
    IconButton(onClick = { scaffoldCtrl.toggleSheet() }) {
      Icon(
        Icons.Outlined.PersonAdd,
        generalGetString(R.string.add_contact),
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
  }
}

@Composable
fun ChatList(chatModel: ChatModel) {
  LazyColumn(
    modifier = Modifier.fillMaxWidth()
  ) {
    items(chatModel.chats) { chat ->
      ChatListNavLinkView(chat, chatModel)
    }
  }
}
