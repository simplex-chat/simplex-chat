package chat.simplex.common.views.contacts

import SectionDividerSpaced
import SectionItemView
import SectionView
import TextIconSpaced
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.offset
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.toUpperCase
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.Chat
import chat.simplex.common.model.ChatController
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.ChatType
import chat.simplex.common.model.ContactStatus
import chat.simplex.common.model.Format
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.platform.BackHandler
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.LazyColumnWithScrollBar
import chat.simplex.common.platform.LocalMultiplatformView
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.getKeyboardState
import chat.simplex.common.platform.hideKeyboard
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.DetailedSMPStatsView
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.strHasSingleSimplexLink
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch
import java.util.HashSet


enum class ContactType {
    CARD, REQUEST, RECENT, REMOVED, UNKNOWN
}

private fun contactChats(c: List<Chat>, contactTypes: List<ContactType>): List<Chat> {
    return c.filter { chat -> contactTypes.contains(getContactType(chat)) }
}

private fun getContactType(chat: Chat): ContactType {
    return when (val cInfo = chat.chatInfo) {
        is ChatInfo.ContactRequest -> ContactType.REQUEST
        is ChatInfo.Direct -> {
            val contact = cInfo.contact;

            when {
                contact.activeConn == null && contact.profile.contactLink != null -> ContactType.CARD
                contact.contactStatus == ContactStatus.DeletedByUser || contact.contactStatus == ContactStatus.Deleted -> ContactType.REMOVED
                else -> ContactType.RECENT
            }
        }
        else -> ContactType.UNKNOWN
    }
}

val chatsByTypeComparator = Comparator<Chat> { chat1, chat2 ->
    val chat1Type = getContactType(chat1)
    val chat2Type = getContactType(chat2)

    when {
        chat1Type.ordinal < chat2Type.ordinal -> -1
        chat1Type.ordinal > chat2Type.ordinal -> 1

        else -> chat2.chatInfo.chatTs.compareTo(chat1.chatInfo.chatTs)
    }
}

@Composable
fun ContactActionsSection(contactActions: @Composable () -> Unit) {
    contactActions()
    Spacer(Modifier.height(DEFAULT_PADDING))

    val archived = remember { contactChats(chatModel.chats, listOf(ContactType.REMOVED)) }

    if (archived.isNotEmpty()) {
        SectionView {
            SectionItemView(
                click = {
                    //
                }
            ) {
                Icon(
                    painterResource(MR.images.ic_folder_open),
                    contentDescription = stringResource(MR.strings.contact_type_deleted),
                    tint = MaterialTheme.colors.secondary,
                )
                TextIconSpaced(extraPadding = true)
                Text(text = stringResource(MR.strings.contact_type_deleted), color = MaterialTheme.colors.onBackground)
            }
        }
    }
}

@Composable
fun ContactsView(contactActions: @Composable () -> Unit) {
    val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
    var searchFocused by remember { mutableStateOf(false) }
    val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(
        TextFieldValue("")
    ) }

    SectionView {
        Divider()
        ContactsSearchBar(
            listState = listState,
            searchText = searchText,
            focused = searchFocused,
            onFocusChanged = {
                searchFocused = it
            }
        )
        Divider()
    }

    if (!searchFocused) {
        ContactActionsSection(contactActions)
    }

    Spacer(Modifier.height(DEFAULT_PADDING))

    SectionView(title = stringResource(MR.strings.contact_list_header_title).uppercase(), padding = PaddingValues(DEFAULT_PADDING)) {
        ContactsList(listState = listState, chatModel = chatModel, searchText = searchText, contactType = ContactType.RECENT)
    }

    if (searchFocused) {
        ContactActionsSection(contactActions)
    }
}


@Composable
fun ContactsSearchBar(listState: LazyListState, searchText: MutableState<TextFieldValue>, focused: Boolean, onFocusChanged: (hasFocus: Boolean) -> Unit) {
    Row(verticalAlignment = Alignment.CenterVertically) {
        val focusRequester = remember { FocusRequester() }
        Icon(painterResource(MR.images.ic_search), null, Modifier.padding(horizontal = DEFAULT_PADDING_HALF), tint = MaterialTheme.colors.secondary)
        SearchTextField(
            Modifier.weight(1f).onFocusChanged { onFocusChanged(it.hasFocus) }.focusRequester(focusRequester),
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

@Composable
fun ToggleFilterButton() {
    val pref = remember { ChatController.appPrefs.showUnreadAndFavorites }
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

private var lazyListState = 0 to 0


@Composable
fun ContactsList(listState: LazyListState, chatModel: ChatModel, searchText: MutableState<TextFieldValue>, contactType: ContactType) {
    val oneHandUI = remember { chatModel.controller.appPrefs.oneHandUI }
    DisposableEffect(Unit) {
        onDispose {
            lazyListState =
                listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset
        }
    }
    val showUnreadAndFavorites =
        remember { ChatController.appPrefs.showUnreadAndFavorites.state }.value
    val allChats = remember { contactChats(chatModel.chats, listOf(ContactType.CARD, ContactType.RECENT, ContactType.REQUEST)) }

    val filteredContactChats = filteredContactChats(
        showUnreadAndFavorites = showUnreadAndFavorites,
        searchText = searchText.value.text,
        contactChats = allChats
    )

    if (filteredContactChats.isEmpty() && allChats.isNotEmpty()) {
        Column(Modifier.fillMaxSize().padding(DEFAULT_PADDING)) {
            Box(Modifier.fillMaxWidth() ,contentAlignment = Alignment.Center) {
                Text(
                    generalGetString(MR.strings.no_filtered_contacts),
                    color = MaterialTheme.colors.secondary
                )
            }
        }
    } else {
        LazyColumnWithScrollBar(
            Modifier.fillMaxWidth(),
            listState
        ) {
            itemsIndexed(filteredContactChats) { index, chat ->
                val nextChatSelected = remember(chat.id, filteredContactChats) {
                    derivedStateOf {
                        chatModel.chatId.value != null && filteredContactChats.getOrNull(index + 1)?.id == chatModel.chatId.value
                    }
                }
                ContactListNavLinkView(chat, nextChatSelected, oneHandUI.state)
            }
        }
    }
}

private fun filterChat(chat: Chat, searchText: String, showUnreadAndFavorites: Boolean): Boolean {
    var meetsPredicate = true;
    val s = searchText.trim().lowercase()
    val cInfo = chat.chatInfo

    if (searchText.isNotEmpty()) {
        meetsPredicate = viewNameContains(cInfo, s) ||
                if (cInfo is ChatInfo.Direct) (cInfo.contact.profile.displayName.lowercase().contains(s) ||
                cInfo.contact.fullName.lowercase().contains(s)) else false
    }

    if (showUnreadAndFavorites) {
        meetsPredicate = meetsPredicate && (cInfo.chatSettings?.favorite ?: false)
    }

    return meetsPredicate;
}

private fun filteredContactChats(
    showUnreadAndFavorites: Boolean,
    searchText: String,
    contactChats: List<Chat>
): List<Chat> {
    return contactChats
        .filter { chat -> filterChat(
            chat = chat,
            searchText = searchText,
            showUnreadAndFavorites = showUnreadAndFavorites) }
        .distinctBy { chat ->
            when (val cInfo = chat.chatInfo) {
                is ChatInfo.ContactRequest -> cInfo.contactRequest.contactRequestId
                is ChatInfo.Direct -> cInfo.contact.contactId
                else -> cInfo.id
            }
        }
        .sortedWith(chatsByTypeComparator)
}

private fun viewNameContains(cInfo: ChatInfo, s: String): Boolean =
    cInfo.chatViewName.lowercase().contains(s.lowercase())