package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.TextRange
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR
import kotlinx.coroutines.flow.distinctUntilChanged
import java.net.URI

@Composable
fun ToggleFilterButton() {
  val pref = remember { ChatController.appPrefs.showUnreadAndFavorites }
  IconButton(onClick = { pref.set(!pref.get()) }) {
    Icon(
      painterResource(MR.images.ic_filter_list),
      null,
      tint = if (pref.state.value) MaterialTheme.colors.background else MaterialTheme.colors.secondary,
      modifier = Modifier
        .padding(3.dp)
        .background(color = if (pref.state.value) MaterialTheme.colors.primary else Color.Unspecified, shape = RoundedCornerShape(50))
        .border(width = 1.dp, color = if (pref.state.value) MaterialTheme.colors.primary else Color.Unspecified, shape = RoundedCornerShape(50))
        .padding(3.dp)
        .size(16.dp)
    )
  }
}

@Composable
private fun ChatListSearchBar(listState: LazyListState, searchText: MutableState<TextFieldValue>, searchShowingSimplexLink: MutableState<Boolean>, searchChatFilteredBySimplexLink: MutableState<String?>) {
  Row(verticalAlignment = Alignment.CenterVertically) {
    val focusRequester = remember { FocusRequester() }
    var focused by remember { mutableStateOf(false) }
    Icon(painterResource(MR.images.ic_search), null, Modifier.padding(horizontal = DEFAULT_PADDING_HALF), tint = MaterialTheme.colors.secondary)
    SearchTextField(
      Modifier.weight(1f).onFocusChanged { focused = it.hasFocus }.focusRequester(focusRequester),
      placeholder = stringResource(MR.strings.search_or_paste_simplex_link),
      alwaysVisible = true,
      searchText = searchText,
      enabled = !remember { searchShowingSimplexLink }.value,
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
    val view = LocalMultiplatformView()
    LaunchedEffect(Unit) {
      snapshotFlow { searchText.value.text }
        .distinctUntilChanged()
        .collect {
          val link = strHasSingleSimplexLink(it.trim())
          if (link != null) {
            // if SimpleX link is pasted, show connection dialogue
            hideKeyboard(view)
            if (link.format is Format.SimplexLink) {
              val linkText = link.simplexLinkText(link.format.linkType, link.format.smpHosts)
              searchText.value = searchText.value.copy(linkText, selection = TextRange.Zero)
            }
            searchShowingSimplexLink.value = true
            searchChatFilteredBySimplexLink.value = null
            connect(link.text, searchChatFilteredBySimplexLink) { searchText.value = TextFieldValue() }
          } else if (!searchShowingSimplexLink.value || it.isEmpty()) {
            if (it.isNotEmpty()) {
              // if some other text is pasted, enter search mode
              focusRequester.requestFocus()
            } else if (listState.layoutInfo.totalItemsCount > 0) {
              listState.scrollToItem(0)
            }
            searchShowingSimplexLink.value = false
            searchChatFilteredBySimplexLink.value = null
          }
        }
    }
  }
}

private fun connect(link: String, searchChatFilteredBySimplexLink: MutableState<String?>, cleanup: (() -> Unit)?) {
  withBGApi {
    planAndConnect(
      chatModel.remoteHostId(),
      URI.create(link),
      incognito = null,
      filterKnownContact = { searchChatFilteredBySimplexLink.value = it.id },
      filterKnownGroup = { searchChatFilteredBySimplexLink.value = it.id },
      close = null,
      cleanup = cleanup,
    )
  }
}

private var lazyListState = 0 to 0

@Composable
fun ChatList(chatModel: ChatModel, searchText: MutableState<TextFieldValue>) {
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  DisposableEffect(Unit) {
    onDispose { lazyListState = listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset }
  }
  val showUnreadAndFavorites = remember { ChatController.appPrefs.showUnreadAndFavorites.state }.value
  val allChats = remember { chatModel.chats }
  // In some not always reproducible situations this code produce IndexOutOfBoundsException on Compose's side
  // which is related to [derivedStateOf]. Using safe alternative instead
  // val chats by remember(search, showUnreadAndFavorites) { derivedStateOf { filteredChats(showUnreadAndFavorites, search, allChats.toList()) } }
  val searchShowingSimplexLink = remember { mutableStateOf(false) }
  val searchChatFilteredBySimplexLink = remember { mutableStateOf<String?>(null) }
  val chats = filteredChats(showUnreadAndFavorites, searchShowingSimplexLink, searchChatFilteredBySimplexLink, searchText.value.text, allChats.toList())
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
        ChatListSearchBar(listState, searchText, searchShowingSimplexLink, searchChatFilteredBySimplexLink)
        Divider()
      }
    }
    itemsIndexed(chats) { index, chat ->
      val nextChatSelected = remember(chat.id, chats) { derivedStateOf {
        chatModel.chatId.value != null && chats.getOrNull(index + 1)?.id == chatModel.chatId.value
      } }
      ChatListNavLinkView(chat, nextChatSelected)
    }
  }
  if (chats.isEmpty() && chatModel.chats.isNotEmpty()) {
    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
      Text(generalGetString(MR.strings.no_filtered_chats), color = MaterialTheme.colors.secondary)
    }
  }
}

private fun filteredChats(
  showUnreadAndFavorites: Boolean,
  searchShowingSimplexLink: State<Boolean>,
  searchChatFilteredBySimplexLink: State<String?>,
  searchText: String,
  chats: List<Chat>
): List<Chat> {
  val linkChatId = searchChatFilteredBySimplexLink.value
  return if (linkChatId != null) {
    chats.filter { it.id == linkChatId }
  } else {
    val s = if (searchShowingSimplexLink.value) "" else searchText.trim().lowercase()
    if (s.isEmpty() && !showUnreadAndFavorites)
      chats.filter { chat -> !chat.chatInfo.chatDeleted }
    else {
      chats.filter { chat ->
        when (val cInfo = chat.chatInfo) {
          is ChatInfo.Direct -> !chat.chatInfo.chatDeleted && (
              if (s.isEmpty()) {
                chat.id == chatModel.chatId.value || filtered(chat)
              } else {
                (viewNameContains(cInfo, s) ||
                    cInfo.contact.profile.displayName.lowercase().contains(s) ||
                    cInfo.contact.fullName.lowercase().contains(s))
              })
          is ChatInfo.Group -> if (s.isEmpty()) {
            chat.id == chatModel.chatId.value || filtered(chat) || cInfo.groupInfo.membership.memberStatus == GroupMemberStatus.MemInvited
          } else {
            viewNameContains(cInfo, s)
          }
          is ChatInfo.Local -> s.isEmpty() || viewNameContains(cInfo, s)
          is ChatInfo.ContactRequest -> s.isEmpty() || viewNameContains(cInfo, s)
          is ChatInfo.ContactConnection -> (s.isNotEmpty() && cInfo.contactConnection.localAlias.lowercase().contains(s)) || (s.isEmpty() && chat.id == chatModel.chatId.value)
          is ChatInfo.InvalidJSON -> chat.id == chatModel.chatId.value
        }
      }
    }
  }
}

private fun filtered(chat: Chat): Boolean =
  (chat.chatInfo.chatSettings?.favorite ?: false) ||
      chat.chatStats.unreadChat ||
      (chat.chatInfo.ntfsEnabled && chat.chatStats.unreadCount > 0)

private fun viewNameContains(cInfo: ChatInfo, s: String): Boolean =
  cInfo.chatViewName.lowercase().contains(s.lowercase())
