package chat.simplex.app.views.chatlist

import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.flow.MutableStateFlow

@Composable
fun ShareListView(chatModel: ChatModel, stopped: Boolean) {
  var searchInList by rememberSaveable { mutableStateOf("") }
  val userPickerState by rememberSaveable(stateSaver = AnimatedViewState.saver()) { mutableStateOf(MutableStateFlow(AnimatedViewState.GONE)) }
  val switchingUsers = rememberSaveable { mutableStateOf(false) }
  Scaffold(
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
  UserPicker(chatModel, userPickerState, switchingUsers, showSettings = false, showCancel = true, cancelClicked = {
    chatModel.sharedContent.value = null
  })
}

@Composable
private fun EmptyList() {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    Text(stringResource(R.string.you_have_no_chats), color = MaterialTheme.colors.secondary)
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
      users.size > 1 -> {
        val allRead = users
          .filter { u -> !u.user.activeUser && !u.user.hidden }
          .all { u -> u.unreadCount == 0 }
        UserProfileButton(chatModel.currentUser.value?.profile?.image, allRead) {
          userPickerState.value = AnimatedViewState.VISIBLE
        }
      }
      else -> NavigationButtonBack { chatModel.sharedContent.value = null }
    }
  }
  if (chatModel.chats.size >= 8) {
    barButtons.add {
      IconButton({ showSearch = true }) {
        Icon(painterResource(R.drawable.ic_search_500), stringResource(android.R.string.search_go).capitalize(Locale.current), tint = MaterialTheme.colors.primary)
      }
    }
  }
  if (stopped) {
    barButtons.add {
      IconButton(onClick = {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.chat_is_stopped_indication),
          generalGetString(R.string.you_can_start_chat_via_setting_or_by_restarting_the_app)
        )
      }) {
        Icon(
          painterResource(R.drawable.ic_report_filled),
          generalGetString(R.string.chat_is_stopped_indication),
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
            is SharedContent.Text -> stringResource(R.string.share_message)
            is SharedContent.Media -> stringResource(R.string.share_image)
            is SharedContent.File -> stringResource(R.string.share_file)
            else -> stringResource(R.string.share_message)
          },
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
        if (chatModel.incognito.value) {
          Icon(
            painterResource(R.drawable.ic_theater_comedy_filled),
            stringResource(R.string.incognito),
            tint = Indigo,
            modifier = Modifier.padding(10.dp).size(26.dp)
          )
        }
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
  val filter: (Chat) -> Boolean = { chat: Chat ->
    chat.chatInfo.chatViewName.lowercase().contains(search.lowercase())
  }
  val chats by remember(search) {
    derivedStateOf {
      if (search.isEmpty()) chatModel.chats.filter { it.chatInfo.ready } else chatModel.chats.filter { it.chatInfo.ready }.filter(filter)
    }
  }
  LazyColumn(
    modifier = Modifier.fillMaxWidth()
  ) {
    items(chats) { chat ->
      ShareListNavLinkView(chat, chatModel)
    }
  }
}
