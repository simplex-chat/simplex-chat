package chat.simplex.common.views.newchat

import SectionDividerSpaced
import SectionItemView
import SectionView
import TextIconSpaced
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.focus.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.text.TextRange
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.contacts.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.cancel
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.flow.filter

@Composable
fun ModalData.NewChatSheet(rh: RemoteHostInfo?, close: () -> Unit) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }

  Box {
    val closeAll = { ModalManager.start.closeModals() }

    Column(modifier = Modifier.fillMaxSize()) {
      NewChatSheetLayout(
        addContact = {
          ModalManager.start.showModalCloseable(endButtons = { AddContactLearnMoreButton() }) { _ -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = closeAll) }
        },
        scanPaste = {
          ModalManager.start.showModalCloseable(endButtons = { AddContactLearnMoreButton() }) { _ -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, showQRCodeScanner = appPlatform.isAndroid, close = closeAll) }
        },
        createGroup = {
          ModalManager.start.showCustomModal { close -> AddGroupView(chatModel, chatModel.currentRemoteHost.value, close, closeAll) }
        },
        rh = rh,
        close = close
      )
    }
    if (oneHandUI.value) {
      Column(Modifier.align(Alignment.BottomCenter)) {
        DefaultAppBar(
          navigationButton = { NavigationButtonBack(onButtonClicked = close) },
          fixedTitleText = generalGetString(MR.strings.new_message),
          onTop = false,
        )
      }
    }
  }
}

enum class ContactType {
  CARD, REQUEST, RECENT, CHAT_DELETED, UNLISTED
}

fun chatContactType(chat: Chat): ContactType {
  return when (val cInfo = chat.chatInfo) {
    is ChatInfo.ContactRequest -> ContactType.REQUEST
    is ChatInfo.Direct -> {
      val contact = cInfo.contact

      when {
        contact.activeConn == null && contact.profile.contactLink != null && contact.active -> ContactType.CARD
        contact.chatDeleted -> ContactType.CHAT_DELETED
        contact.contactStatus == ContactStatus.Active -> ContactType.RECENT
        else -> ContactType.UNLISTED
      }
    }
    else -> ContactType.UNLISTED
  }
}

private fun filterContactTypes(c: List<Chat>, contactTypes: List<ContactType>): List<Chat> {
  return c.filter { chat -> contactTypes.contains(chatContactType(chat)) }
}

@Composable
private fun ModalData.NewChatSheetLayout(
  rh: RemoteHostInfo?,
  addContact: () -> Unit,
  scanPaste: () -> Unit,
  createGroup: () -> Unit,
  close: () -> Unit,
) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val listState = remember { appBarHandler.listState }
  // This is workaround of an issue when position of a list is not restored (when going back to that screen) when a header exists.
  // Upon returning back, this code returns correct index and position if number of items is the same
  LaunchedEffect(Unit) {
    val prevIndex = listState.firstVisibleItemIndex
    val prevOffset = listState.firstVisibleItemScrollOffset
    val total = listState.layoutInfo.totalItemsCount
    if (prevIndex == 0 && prevOffset == 0) return@LaunchedEffect
    snapshotFlow { listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset }
      .filter { it == 0 to 0  }
      .collect {
        if (total <= listState.layoutInfo.totalItemsCount) {
          listState.scrollToItem(prevIndex, prevOffset)
        }
        cancel()
      }
  }
  val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue("")) }
  val searchShowingSimplexLink = remember { mutableStateOf(false) }
  val searchChatFilteredBySimplexLink = remember { mutableStateOf<String?>(null) }
  val showUnreadAndFavorites = remember { ChatController.appPrefs.showUnreadAndFavorites.state }.value
  val baseContactTypes = remember { listOf(ContactType.CARD, ContactType.RECENT, ContactType.REQUEST) }
  val contactTypes by remember(searchText.value.text.isEmpty()) {
    derivedStateOf { contactTypesSearchTargets(baseContactTypes, searchText.value.text.isEmpty()) }
  }
  val allChats by remember(chatModel.chats.value, contactTypes) {
    derivedStateOf { filterContactTypes(chatModel.chats.value, contactTypes) }
  }
  var scrollDirection by remember { mutableStateOf(ScrollDirection.Idle) }
  var previousIndex by remember { mutableStateOf(0) }
  var previousScrollOffset by remember { mutableStateOf(0) }
  val keyboardState by getKeyboardState()

  LaunchedEffect(listState.firstVisibleItemIndex, listState.firstVisibleItemScrollOffset) {
    val currentIndex = listState.firstVisibleItemIndex
    val currentScrollOffset = listState.firstVisibleItemScrollOffset
    val threshold = 25

    scrollDirection = when {
      currentIndex > previousIndex -> ScrollDirection.Down
      currentIndex < previousIndex -> ScrollDirection.Up
      currentScrollOffset > previousScrollOffset + threshold -> ScrollDirection.Down
      currentScrollOffset < previousScrollOffset - threshold -> ScrollDirection.Up
      currentScrollOffset == previousScrollOffset -> ScrollDirection.Idle
      else -> scrollDirection
    }

    previousIndex = currentIndex
    previousScrollOffset = currentScrollOffset
  }

  val filteredContactChats = filteredContactChats(
    showUnreadAndFavorites = showUnreadAndFavorites,
    searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
    searchShowingSimplexLink = searchShowingSimplexLink,
    searchText = searchText.value.text,
    contactChats = allChats
  )

  val sectionModifier = Modifier.fillMaxWidth()
  val deletedContactTypes = listOf(ContactType.CHAT_DELETED)
  val deletedChats by remember(chatModel.chats.value, deletedContactTypes) {
    derivedStateOf { filterContactTypes(chatModel.chats.value, deletedContactTypes) }
  }

  val actionButtonsOriginal = listOf(
    Triple(
      painterResource(MR.images.ic_add_link),
      stringResource(MR.strings.create_1_time_link),
      addContact,
    ),
    Triple(
      painterResource(MR.images.ic_qr_code),
      if (appPlatform.isAndroid) stringResource(MR.strings.scan_paste_link) else stringResource(MR.strings.paste_link),
      scanPaste,
    ),
    Triple(
      painterResource(MR.images.ic_group),
      stringResource(MR.strings.create_group_button),
      createGroup,
    )
  )

  @Composable
  fun DeletedChatsItem(actionButtons: List<Triple<Painter, String, () -> Unit>>) {
    if (searchText.value.text.isEmpty()) {
      Spacer(Modifier.padding(bottom = 27.dp))
    }

    if (searchText.value.text.isEmpty()) {
      Row {
        SectionView {
          actionButtons.map {
            NewChatButton(
              icon = it.first,
              text = it.second,
              click = it.third,
            )
          }
        }
      }
      if (deletedChats.isNotEmpty()) {
        SectionDividerSpaced(maxBottomPadding = false)
        SectionView {
          SectionItemView(
            click = {
              ModalManager.start.showCustomModal { closeDeletedChats ->
                ModalView(
                  close = closeDeletedChats,
                  showAppBar = !oneHandUI.value,
                ) {
                  if (oneHandUI.value) {
                    BackHandler(onBack = closeDeletedChats)
                  }
                  DeletedContactsView(rh = rh, closeDeletedChats = closeDeletedChats, close = {
                    ModalManager.start.closeModals()
                  })
                }
              }
            }
          ) {
            Icon(
              painterResource(MR.images.ic_inventory_2),
              contentDescription = stringResource(MR.strings.deleted_chats),
              tint = MaterialTheme.colors.secondary,
            )
            TextIconSpaced(false)
            Text(text = stringResource(MR.strings.deleted_chats), color = MaterialTheme.colors.onBackground)
          }
        }
      }
    }
  }

  @Composable
  fun NoFilteredContactsItem() {
    if (filteredContactChats.isEmpty() && allChats.isNotEmpty()) {
      Column(sectionModifier.fillMaxSize().padding(DEFAULT_PADDING)) {
        Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
          Text(
            generalGetString(MR.strings.no_filtered_contacts),
            color = MaterialTheme.colors.secondary
          )
        }
      }
    }
  }

  @Composable
  fun OneHandLazyColumn() {
    val blankSpaceSize = WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + AppBarHeight * fontSizeSqrtMultiplier
    LazyColumnWithScrollBar(
      state = listState,
      reverseLayout = oneHandUI.value
    ) {
      item { Spacer(Modifier.height(blankSpaceSize)) }
      stickyHeader {
        val scrolledSomething by remember { derivedStateOf { listState.firstVisibleItemScrollOffset > 0 || listState.firstVisibleItemIndex > 0 } }
        Column(
          Modifier
            .zIndex(1f)
            .offset {
              val y = if (searchText.value.text.isNotEmpty() || (appPlatform.isAndroid && keyboardState == KeyboardState.Opened)) {
                if (listState.firstVisibleItemIndex == 0) -minOf(listState.firstVisibleItemScrollOffset, blankSpaceSize.roundToPx())
                else -blankSpaceSize.roundToPx()
              } else {
                when (listState.firstVisibleItemIndex) {
                  0 -> 0
                  1 -> listState.firstVisibleItemScrollOffset
                  else -> 1000
                }
              }
              IntOffset(0, y)
            }
            // show background when something is scrolled because otherwise the bar is transparent.
            // not using background always because of gradient in SimpleX theme
            .background(
              if (scrolledSomething && (keyboardState == KeyboardState.Opened || searchText.value.text.isNotEmpty())) {
                MaterialTheme.colors.background
              } else {
                Color.Unspecified
              }
            )
        ) {
          Divider()
          Column(Modifier.consumeWindowInsets(WindowInsets.navigationBars).consumeWindowInsets(PaddingValues(bottom = AppBarHeight * fontSizeSqrtMultiplier))) {
            ContactsSearchBar(
              listState = listState,
              searchText = searchText,
              searchShowingSimplexLink = searchShowingSimplexLink,
              searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
              close = close,
            )
            Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.ime))
          }
        }
      }
      item {
        DeletedChatsItem(actionButtonsOriginal.asReversed())
      }
      item {
        if (filteredContactChats.isNotEmpty() && searchText.value.text.isEmpty()) {
          SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
          SectionView(stringResource(MR.strings.contact_list_header_title).uppercase(), headerBottomPadding = DEFAULT_PADDING_HALF) {}
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))
        }
      }
      item {
        NoFilteredContactsItem()
      }
      itemsIndexed(filteredContactChats) { index, chat ->
        val nextChatSelected = remember(chat.id, filteredContactChats) {
          derivedStateOf {
            chatModel.chatId.value != null && filteredContactChats.getOrNull(index + 1)?.id == chatModel.chatId.value
          }
        }
        ContactListNavLinkView(chat, nextChatSelected, showDeletedChatIcon = true)
      }
      if (appPlatform.isAndroid) {
        item {
          Spacer(Modifier.windowInsetsTopHeight(WindowInsets.statusBars))
        }
      }
    }
  }

  @Composable
  fun NonOneHandLazyColumn() {
    val blankSpaceSize = topPaddingToContent(false)
    LazyColumnWithScrollBar(
      Modifier.imePadding(),
      state = listState,
      reverseLayout = false
    ) {
      item {
        Box(Modifier.padding(top = blankSpaceSize)) {
          AppBarTitle(
            stringResource(MR.strings.new_message),
            hostDevice(rh?.remoteHostId),
            bottomPadding = DEFAULT_PADDING
          )
        }
      }
      stickyHeader {
        val scrolledSomething by remember { derivedStateOf { listState.firstVisibleItemScrollOffset > 0 || listState.firstVisibleItemIndex > 0 } }
        Column(
          Modifier
            .zIndex(1f)
            .offset {
              val y = if (searchText.value.text.isNotEmpty() || (appPlatform.isAndroid && keyboardState == KeyboardState.Opened)) {
                if (listState.firstVisibleItemIndex == 0) (listState.firstVisibleItemScrollOffset - (listState.layoutInfo.visibleItemsInfo[0].size - blankSpaceSize.roundToPx())).coerceAtLeast(0)
                else blankSpaceSize.roundToPx()
              } else {
                when (listState.firstVisibleItemIndex) {
                  0 -> 0
                  1 -> -listState.firstVisibleItemScrollOffset
                  else -> -1000
                }
              }
              IntOffset(0, y)
            }
            // show background when something is scrolled because otherwise the bar is transparent.
            // not using background always because of gradient in SimpleX theme
            .background(
              if (scrolledSomething && (keyboardState == KeyboardState.Opened || searchText.value.text.isNotEmpty())) {
                MaterialTheme.colors.background
              } else {
                Color.Unspecified
              }
            )
        ) {
          Divider()
          ContactsSearchBar(
            listState = listState,
            searchText = searchText,
            searchShowingSimplexLink = searchShowingSimplexLink,
            searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
            close = close,
          )
          Divider()
        }
      }
      item {
        DeletedChatsItem(actionButtonsOriginal)
      }
      item {
        if (filteredContactChats.isNotEmpty() && searchText.value.text.isEmpty()) {
          SectionDividerSpaced()
          SectionView(stringResource(MR.strings.contact_list_header_title).uppercase(), headerBottomPadding = DEFAULT_PADDING_HALF) {}
        }
      }
      item {
        NoFilteredContactsItem()
      }
      itemsIndexed(filteredContactChats) { index, chat ->
        val nextChatSelected = remember(chat.id, filteredContactChats) {
          derivedStateOf {
            chatModel.chatId.value != null && filteredContactChats.getOrNull(index + 1)?.id == chatModel.chatId.value
          }
        }
        ContactListNavLinkView(chat, nextChatSelected, showDeletedChatIcon = true)
      }
      if (appPlatform.isAndroid) {
        item {
          Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.navigationBars))
        }
      }
    }
  }

  Box {
    if (oneHandUI.value) {
      OneHandLazyColumn()
      StatusBarBackground()
    } else {
      NonOneHandLazyColumn()
      NavigationBarBackground(oneHandUI.value, true)
    }
  }
}

@Composable
private fun NewChatButton(
  icon: Painter,
  text: String,
  click: () -> Unit,
  textColor: Color = Color.Unspecified,
  iconColor: Color = MaterialTheme.colors.primary,
  disabled: Boolean = false
) {
  SectionItemView(click, disabled = disabled) {
    Row {
      Icon(icon, text, tint = if (disabled) MaterialTheme.colors.secondary else iconColor)
      TextIconSpaced(false)
      Text(text, color = if (disabled) MaterialTheme.colors.secondary else textColor)
    }
  }
}

@Composable
private fun ContactsSearchBar(
  listState: LazyListState,
  searchText: MutableState<TextFieldValue>,
  searchShowingSimplexLink: MutableState<Boolean>,
  searchChatFilteredBySimplexLink: MutableState<String?>,
  close: () -> Unit,
) {
  var focused by remember { mutableStateOf(false) }

  Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.fillMaxWidth()) {
    val focusRequester = remember { FocusRequester() }
    Icon(
      painterResource(MR.images.ic_search),
      contentDescription = null,
      Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING_HALF).size(22.dp * fontSizeSqrtMultiplier),
      tint = MaterialTheme.colors.secondary
    )
    SearchTextField(
      Modifier.weight(1f).onFocusChanged { focused = it.hasFocus }.focusRequester(focusRequester),
      placeholder = stringResource(MR.strings.search_or_paste_simplex_link),
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
              val linkText =
                link.simplexLinkText(link.format.linkType, link.format.smpHosts)
              searchText.value =
                searchText.value.copy(linkText, selection = TextRange.Zero)
            }
            searchShowingSimplexLink.value = true
            searchChatFilteredBySimplexLink.value = null
            connect(
              link = link.text,
              searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
              close = close,
              cleanup = { searchText.value = TextFieldValue() }
            )
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

@Composable
private fun ToggleFilterButton() {
  val pref = remember { appPrefs.showUnreadAndFavorites }
  IconButton(onClick = { pref.set(!pref.get()) }) {
    val sp16 = with(LocalDensity.current) { 16.sp.toDp() }
    Icon(
      painterResource(MR.images.ic_filter_list),
      null,
      tint = if (pref.state.value) MaterialTheme.colors.background else MaterialTheme.colors.secondary,
      modifier = Modifier
        .padding(3.dp)
        .background(color = if (pref.state.value) MaterialTheme.colors.primary else Color.Unspecified, shape = RoundedCornerShape(50))
        .border(width = 1.dp, color = if (pref.state.value) MaterialTheme.colors.primary else Color.Unspecified, shape = RoundedCornerShape(50))
        .padding(3.dp)
        .size(sp16)
    )
  }
}

private fun connect(link: String, searchChatFilteredBySimplexLink: MutableState<String?>, close: () -> Unit, cleanup: (() -> Unit)?) {
  withBGApi {
    planAndConnect(
      chatModel.remoteHostId(),
      link,
      incognito = null,
      filterKnownContact = { searchChatFilteredBySimplexLink.value = it.id },
      close = close,
      cleanup = cleanup,
    )
  }
}

private fun filteredContactChats(
  showUnreadAndFavorites: Boolean,
  searchShowingSimplexLink: State<Boolean>,
  searchChatFilteredBySimplexLink: State<String?>,
  searchText: String,
  contactChats: List<Chat>
): List<Chat> {
  val linkChatId = searchChatFilteredBySimplexLink.value
  val s = if (searchShowingSimplexLink.value) "" else searchText.trim().lowercase()

  return if (linkChatId != null) {
    contactChats.filter { it.id == linkChatId }
  } else {
    contactChats.filter { chat ->
      filterChat(
        chat = chat,
        searchText = s,
        showUnreadAndFavorites = showUnreadAndFavorites
      )
    }
  }
    .sortedWith(chatsByTypeComparator)
}

private fun filterChat(chat: Chat, searchText: String, showUnreadAndFavorites: Boolean): Boolean {
  var meetsPredicate = true
  val s = searchText.trim().lowercase()
  val cInfo = chat.chatInfo

  if (searchText.isNotEmpty()) {
    meetsPredicate = cInfo.anyNameContains(s)
  }

  if (showUnreadAndFavorites) {
    meetsPredicate = meetsPredicate && (cInfo.chatSettings?.favorite ?: false)
  }

  return meetsPredicate
}

private val chatsByTypeComparator = Comparator<Chat> { chat1, chat2 ->
  val chat1Type = chatContactType(chat1)
  val chat2Type = chatContactType(chat2)

  when {
    chat1Type.ordinal < chat2Type.ordinal -> -1
    chat1Type.ordinal > chat2Type.ordinal -> 1

    else -> chat2.chatInfo.chatTs.compareTo(chat1.chatInfo.chatTs)
  }
}

private fun contactTypesSearchTargets(baseContactTypes: List<ContactType>, searchEmpty: Boolean): List<ContactType> {
  return if (baseContactTypes.contains(ContactType.CHAT_DELETED) || searchEmpty) {
    baseContactTypes
  } else {
    baseContactTypes + ContactType.CHAT_DELETED
  }
}

@Composable
private fun ModalData.DeletedContactsView(rh: RemoteHostInfo?, closeDeletedChats: () -> Unit, close: () -> Unit) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  Box {
    val listState = remember { appBarHandler.listState }
    val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue("")) }
    val searchShowingSimplexLink = remember { mutableStateOf(false) }
    val searchChatFilteredBySimplexLink = remember { mutableStateOf<String?>(null) }
    val showUnreadAndFavorites = remember { appPrefs.showUnreadAndFavorites.state }.value
    val allChats by remember(chatModel.chats.value) {
      derivedStateOf { filterContactTypes(chatModel.chats.value, listOf(ContactType.CHAT_DELETED)) }
    }
    val filteredContactChats = filteredContactChats(
      showUnreadAndFavorites = showUnreadAndFavorites,
      searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
      searchShowingSimplexLink = searchShowingSimplexLink,
      searchText = searchText.value.text,
      contactChats = allChats
    )

    Box {
      val topPaddingToContent = topPaddingToContent(false)
      LazyColumnWithScrollBar(
        if (!oneHandUI.value) Modifier.imePadding() else Modifier,
        contentPadding = PaddingValues(
          top = if (!oneHandUI.value) topPaddingToContent else 0.dp,
          bottom = if (oneHandUI.value) WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + AppBarHeight * fontSizeSqrtMultiplier else 0.dp
        ),
        reverseLayout = oneHandUI.value,
      ) {
        item {
          if (!oneHandUI.value) {
            Box(contentAlignment = Alignment.Center) {
              val bottomPadding = DEFAULT_PADDING
              AppBarTitle(
                stringResource(MR.strings.deleted_chats),
                hostDevice(rh?.remoteHostId),
                bottomPadding = bottomPadding
              )
            }
          }
        }
        item {
          if (!oneHandUI.value) {
            Divider()
            ContactsSearchBar(
              listState = listState,
              searchText = searchText,
              searchShowingSimplexLink = searchShowingSimplexLink,
              searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
              close = close,
            )
          } else {
            Column(Modifier.consumeWindowInsets(WindowInsets.navigationBars).consumeWindowInsets(PaddingValues(bottom = AppBarHeight))) {
              ContactsSearchBar(
                listState = listState,
                searchText = searchText,
                searchShowingSimplexLink = searchShowingSimplexLink,
                searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
                close = close,
              )
              Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.ime))
            }
          }
          Divider()
        }

        item {
          if (filteredContactChats.isEmpty() && allChats.isNotEmpty()) {
            Column(Modifier.fillMaxSize().padding(DEFAULT_PADDING)) {
              Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
                Text(
                  generalGetString(MR.strings.no_filtered_contacts),
                  color = MaterialTheme.colors.secondary,
                )
              }
            }
          }
        }

        itemsIndexed(filteredContactChats) { index, chat ->
          val nextChatSelected = remember(chat.id, filteredContactChats) {
            derivedStateOf {
              chatModel.chatId.value != null && filteredContactChats.getOrNull(index + 1)?.id == chatModel.chatId.value
            }
          }
          ContactListNavLinkView(chat, nextChatSelected, showDeletedChatIcon = false)
        }
        if (appPlatform.isAndroid) {
          item {
            Spacer(if (oneHandUI.value) Modifier.windowInsetsTopHeight(WindowInsets.statusBars) else Modifier.windowInsetsBottomHeight(WindowInsets.navigationBars))
          }
        }
      }
      if (oneHandUI.value) {
        StatusBarBackground()
      } else {
        NavigationBarBackground(oneHandUI.value, true)
      }
    }
    if (oneHandUI.value) {
      Column(Modifier.align(Alignment.BottomCenter)) {
        DefaultAppBar(
          navigationButton = { NavigationButtonBack(onButtonClicked = closeDeletedChats) },
          fixedTitleText = generalGetString(MR.strings.deleted_chats),
          onTop = false,
        )
      }
    }
  }
}

@Composable
fun ActionButton(
  text: String?,
  comment: String?,
  icon: Painter,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(shape = RoundedCornerShape(18.dp), color = Color.Transparent, contentColor = LocalContentColor.current) {
    Column(
      Modifier
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
      if (text != null) {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint,
          modifier = Modifier.padding(bottom = 4.dp)
        )
      }
      if (comment != null) {
        Text(
          comment,
          textAlign = TextAlign.Center,
          style = MaterialTheme.typography.body2
        )
      }
    }
  }
}

@Composable
fun ActionButton(
  modifier: Modifier,
  text: String?,
  comment: String?,
  icon: Painter,
  tint: Color = MaterialTheme.colors.primary,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(modifier, shape = RoundedCornerShape(18.dp), contentColor = LocalContentColor.current) {
    Column(
      Modifier
        .fillMaxWidth()
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) MaterialTheme.colors.secondary else tint
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
      if (text != null) {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint,
          modifier = Modifier.padding(bottom = 4.dp)
        )
      }
      if (comment != null) {
        Text(
          comment,
          textAlign = TextAlign.Center,
          style = MaterialTheme.typography.body2
        )
      }
    }
  }
}

@Preview
@Composable
private fun PreviewNewChatSheet() {
  SimpleXTheme {
    ModalData().NewChatSheet(rh = null, close = {})
  }
}
