package chat.simplex.common.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.common.SettingsViewState
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.home.UserProfileButton
import chat.simplex.res.MR
import kotlinx.coroutines.flow.MutableStateFlow

@Composable
fun ShareListView(chatModel: ChatModel, settingsState: SettingsViewState, stopped: Boolean) {
  var searchInList by rememberSaveable { mutableStateOf("") }
  val (userPickerState, scaffoldState) = settingsState
  val endPadding = if (appPlatform.isDesktop) 56.dp else 0.dp
  Scaffold(
    Modifier.padding(end = endPadding),
    contentColor = LocalContentColor.current,
    drawerContentColor = LocalContentColor.current,
    scaffoldState = scaffoldState,
    topBar = { Column { ShareListToolbar(chatModel, userPickerState, stopped) { searchInList = it.trim() } } },
  ) {
    Box(Modifier.padding(it)) {
      Column(
        modifier = Modifier
          .fillMaxSize()
      ) {
        if (chatModel.chats.isNotEmpty()) {
          ShareList(chatModel, search = searchInList)
        } else {
          EmptyList()
        }
      }
    }
  }
  if (appPlatform.isAndroid) {
    tryOrShowError("UserPicker", error = {}) {
      UserPicker(chatModel, userPickerState, showSettings = false, showCancel = true, cancelClicked = {
        chatModel.sharedContent.value = null
        userPickerState.value = AnimatedViewState.GONE
      })
    }
  }
}

@Composable
private fun EmptyList() {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    Text(stringResource(MR.strings.you_have_no_chats), color = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun ShareListToolbar(chatModel: ChatModel, userPickerState: MutableStateFlow<AnimatedViewState>, stopped: Boolean, onSearchValueChanged: (String) -> Unit) {
  var showSearch by rememberSaveable { mutableStateOf(false) }
  val hideSearchOnBack = { onSearchValueChanged(""); showSearch = false }
  if (showSearch) {
    BackHandler(onBack = hideSearchOnBack)
  }
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  val users by remember { derivedStateOf { chatModel.users.filter { u -> u.user.activeUser || !u.user.hidden } } }
  val navButton: @Composable RowScope.() -> Unit = {
    when {
      showSearch -> NavigationButtonBack(hideSearchOnBack)
      (users.size > 1 || chatModel.remoteHosts.isNotEmpty()) && remember { chatModel.sharedContent }.value !is SharedContent.Forward -> {
        val allRead = users
          .filter { u -> !u.user.activeUser && !u.user.hidden }
          .all { u -> u.unreadCount == 0 }
        UserProfileButton(chatModel.currentUser.value?.profile?.image, allRead) {
          userPickerState.value = AnimatedViewState.VISIBLE
        }
      }
      else -> NavigationButtonBack(onButtonClicked = {
        val sharedContent = chatModel.sharedContent.value
        // Drop shared content
        chatModel.sharedContent.value = null
        if (sharedContent is SharedContent.Forward) {
          chatModel.chatId.value = sharedContent.fromChatInfo.id
        }
      })
    }
  }
  if (chatModel.chats.size >= 8) {
    barButtons.add {
      IconButton({ showSearch = true }) {
        Icon(painterResource(MR.images.ic_search_500), stringResource(MR.strings.search_verb), tint = MaterialTheme.colors.primary)
      }
    }
  }
  if (stopped) {
    barButtons.add {
      IconButton(onClick = {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.chat_is_stopped_indication),
          generalGetString(MR.strings.you_can_start_chat_via_setting_or_by_restarting_the_app)
        )
      }) {
        Icon(
          painterResource(MR.images.ic_report_filled),
          generalGetString(MR.strings.chat_is_stopped_indication),
          tint = Color.Red,
        )
      }
    }
  }

  DefaultTopAppBar(
    navigationButton = navButton,
    title = {
      Row(verticalAlignment = Alignment.CenterVertically) {
        Text(
          when (chatModel.sharedContent.value) {
            is SharedContent.Text -> stringResource(MR.strings.share_message)
            is SharedContent.Media -> stringResource(MR.strings.share_image)
            is SharedContent.File -> stringResource(MR.strings.share_file)
            is SharedContent.Forward -> stringResource(MR.strings.forward_message)
            null -> stringResource(MR.strings.share_message)
          },
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
      }
    },
    onTitleClick = null,
    showSearch = showSearch,
    onSearchValueChanged = onSearchValueChanged,
    buttons = barButtons
  )
  Divider()
}

@Composable
private fun ShareList(chatModel: ChatModel, search: String) {
  val chats by remember(search) {
    derivedStateOf {
      val sorted = chatModel.chats.toList().sortedByDescending { it.chatInfo is ChatInfo.Local }
      if (search.isEmpty()) {
        sorted.filter { it.chatInfo.ready }
      } else {
        sorted.filter { it.chatInfo.ready && it.chatInfo.chatViewName.lowercase().contains(search.lowercase()) }
      }
    }
  }
  LazyColumnWithScrollBar(
    modifier = Modifier.fillMaxWidth()
  ) {
    items(chats) { chat ->
      ShareListNavLinkView(chat, chatModel)
    }
  }
}
