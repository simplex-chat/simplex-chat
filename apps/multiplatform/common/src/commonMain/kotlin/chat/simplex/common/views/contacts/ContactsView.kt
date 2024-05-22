package chat.simplex.common.views.contacts


import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chatlist.ToggleFilterButton
import chat.simplex.common.views.home.contactChats
import chat.simplex.res.MR
import kotlinx.coroutines.flow.distinctUntilChanged

@Composable
private fun ContactsSearchBar(listState: LazyListState, searchText: MutableState<TextFieldValue>) {
  Row(verticalAlignment = Alignment.CenterVertically) {
    val focusRequester = remember { FocusRequester() }
    var focused by remember { mutableStateOf(false) }
    Icon(painterResource(MR.images.ic_search), null, Modifier.padding(horizontal = DEFAULT_PADDING_HALF), tint = MaterialTheme.colors.secondary)
    SearchTextField(
      Modifier.weight(1f).onFocusChanged { focused = it.hasFocus }.focusRequester(focusRequester),
      placeholder = stringResource(MR.strings.search_verb),
      alwaysVisible = true,
      searchText = searchText,
      trailingContent = null,
    ) {
      searchText.value = searchText.value.copy(it)
    }
    val hasText = remember { derivedStateOf { searchText.value.text.isNotEmpty() } }
    if (hasText.value) {
      val hideSearchOnBack: () -> Unit = { searchText.value = TextFieldValue() }
      BackHandler(onBack = hideSearchOnBack)
      KeyChangeEffect(chatModel.currentRemoteHost.value) {
        hideSearchOnBack()
      }
    } else {
      Row {
        val padding = if (appPlatform.isDesktop) 0.dp else 7.dp
        if (chatModel.chats.size > 0) {
          ToggleFilterButton()
        }
        Spacer(Modifier.width(padding))
      }
    }
    val focusManager = LocalFocusManager.current
    val keyboardState = getKeyboardState()
    LaunchedEffect(keyboardState.value) {
      if (keyboardState.value == KeyboardState.Closed && focused) {
        focusManager.clearFocus()
      }
    }
    LaunchedEffect(Unit) {
      snapshotFlow { searchText.value.text }
        .distinctUntilChanged()
        .collect {
          if (it.isNotEmpty()) {
            focusRequester.requestFocus()
          } else if (listState.layoutInfo.totalItemsCount > 0) {
            listState.scrollToItem(0)
          }
        }
    }
  }
}

private var lazyListState = 0 to 0

@Composable
fun ContactsList(chatModel: ChatModel, searchText: MutableState<TextFieldValue>) {
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  DisposableEffect(Unit) {
    onDispose { lazyListState = listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset }
  }
  val showUnreadAndFavorites = remember { ChatController.appPrefs.showUnreadAndFavorites.state }.value
  val allContactChats = remember(chatModel.chats.toList()) { contactChats(chatModel.chats) }
  // In some not always reproducible situations this code produce IndexOutOfBoundsException on Compose's side
  // which is related to [derivedStateOf]. Using safe alternative instead
  // val chats by remember(search, showUnreadAndFavorites) { derivedStateOf { filteredChats(showUnreadAndFavorites, search, allChats.toList()) } }
  val filteredContactChats = filteredContactChats(showUnreadAndFavorites, searchText.value.text, allContactChats.toList())
  LazyColumnWithScrollBar(
    Modifier.fillMaxWidth(),
    listState
  ) {
    stickyHeader {
      Column(
        Modifier
          .offset {
            val y = if (searchText.value.text.isEmpty()) {
              if (listState.firstVisibleItemIndex == 0) -listState.firstVisibleItemScrollOffset else -1000
            } else {
              0
            }
            IntOffset(0, y)
          }
          .background(MaterialTheme.colors.background)
      ) {
        ContactsSearchBar(listState, searchText)
        Divider()
      }
    }
    itemsIndexed(filteredContactChats) { index, chat ->
      val nextChatSelected = remember(chat.id, filteredContactChats) { derivedStateOf {
        chatModel.chatId.value != null && filteredContactChats.getOrNull(index + 1)?.id == chatModel.chatId.value
      } }
      ContactListNavLinkView(chat, nextChatSelected)
    }
  }
  if (filteredContactChats.isEmpty() && allContactChats.isNotEmpty()) {
    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
      Text(generalGetString(MR.strings.no_filtered_contacts), color = MaterialTheme.colors.secondary)
    }
  }
}

private fun filteredContactChats(
  showUnreadAndFavorites: Boolean,
  searchText: String,
  contactChats: List<Chat>
): List<Chat> {
  val s = searchText.trim().lowercase()
  return (
      if (s.isEmpty() && !showUnreadAndFavorites)
        contactChats.filter { chat ->
          when (val cInfo = chat.chatInfo) {
            is ChatInfo.Direct -> cInfo.contact.contactStatus != ContactStatus.DeletedByUser
            else -> false
          }
        }
      else {
        contactChats.filter { chat ->
          when (val cInfo = chat.chatInfo) {
            is ChatInfo.Direct -> cInfo.contact.contactStatus != ContactStatus.DeletedByUser && (
                if (s.isEmpty()) {
                  chat.id == chatModel.chatId.value ||
                      (cInfo.chatSettings?.favorite ?: false)
                } else {
                  (viewNameContains(cInfo, s) ||
                      cInfo.contact.profile.displayName.lowercase().contains(s) ||
                      cInfo.contact.fullName.lowercase().contains(s))
                })

            else -> false
          }
        }
      }
      ).sortedBy { it.chatInfo.displayName.lowercase() }
}

private fun viewNameContains(cInfo: ChatInfo, s: String): Boolean =
  cInfo.chatViewName.lowercase().contains(s.lowercase())
