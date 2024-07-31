package chat.simplex.common.views.contacts

import SectionView
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Divider
import androidx.compose.material.Icon
import androidx.compose.material.IconButton
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.State
import androidx.compose.runtime.derivedStateOf
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.runtime.snapshotFlow
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.Chat
import chat.simplex.common.model.ChatController
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ContactStatus
import chat.simplex.common.model.Format
import chat.simplex.common.model.RemoteHostInfo
import chat.simplex.common.platform.BackHandler
import chat.simplex.common.platform.LocalMultiplatformView
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.getKeyboardState
import chat.simplex.common.platform.hideKeyboard
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.planAndConnect
import chat.simplex.common.views.newchat.strHasSingleSimplexLink
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.distinctUntilChanged
import java.net.URI

enum class ContactType {
    CARD, REQUEST, RECENT, CHAT_DELETED, UNLISTED
}

fun contactChats(c: List<Chat>, contactTypes: List<ContactType>): List<Chat> {
    return c.filter { chat -> contactTypes.contains(getContactType(chat)) }
}

fun getContactType(chat: Chat): ContactType {
    return when (val cInfo = chat.chatInfo) {
        is ChatInfo.ContactRequest -> ContactType.REQUEST
        is ChatInfo.Direct -> {
            val contact = cInfo.contact;

            when {
                contact.activeConn == null && contact.profile.contactLink != null -> ContactType.CARD
                contact.chatDeleted -> ContactType.CHAT_DELETED
                contact.contactStatus == ContactStatus.Active -> ContactType.RECENT
                else -> ContactType.UNLISTED
            }
        }
        else -> ContactType.UNLISTED
    }
}

private val chatsByTypeComparator = Comparator<Chat> { chat1, chat2 ->
    val chat1Type = getContactType(chat1)
    val chat2Type = getContactType(chat2)

    when {
        chat1Type.ordinal < chat2Type.ordinal -> -1
        chat1Type.ordinal > chat2Type.ordinal -> 1

        else -> chat2.chatInfo.chatTs.compareTo(chat1.chatInfo.chatTs)
    }
}

private var lazyListState = 0 to 0

@Composable
fun DeletedContactsView(rh: RemoteHostInfo?, close: () -> Unit) {
    Column(
        Modifier.fillMaxSize(),
    ) {
        Box(contentAlignment = Alignment.Center) {
            val bottomPadding = DEFAULT_PADDING
            AppBarTitle(
                stringResource(MR.strings.deleted_chats),
                hostDevice(rh?.remoteHostId),
                bottomPadding = bottomPadding
            )
        }
        val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
        val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue("")) }
        val searchShowingSimplexLink = remember { mutableStateOf(false) }
        val searchChatFilteredBySimplexLink = remember { mutableStateOf<String?>(null) }
        val showUnreadAndFavorites = remember { ChatController.appPrefs.showUnreadAndFavorites.state }.value
        val contactTypes = listOf(ContactType.CHAT_DELETED)
        val allChats by remember(chatModel.chats, contactTypes) {
            derivedStateOf { contactChats(chatModel.chats, contactTypes) }
        }
        val filteredContactChats = filteredContactChats(
            showUnreadAndFavorites = showUnreadAndFavorites,
            searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
            searchShowingSimplexLink = searchShowingSimplexLink,
            searchText = searchText.value.text,
            contactChats = allChats
        )

        LazyColumn(
            Modifier.fillMaxWidth(),
            listState
        ) {
            item {
                Divider()
                ContactsSearchBar(
                    listState = listState,
                    searchText = searchText,
                    searchShowingSimplexLink = searchShowingSimplexLink,
                    searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
                    close = close
                )
                Divider()

                Spacer(Modifier.padding(bottom = DEFAULT_PADDING))
            }

            itemsIndexed(filteredContactChats) { index, chat ->
                val nextChatSelected = remember(chat.id, filteredContactChats) {
                    derivedStateOf {
                        chatModel.chatId.value != null && filteredContactChats.getOrNull(index + 1)?.id == chatModel.chatId.value
                    }
                }
                ContactListNavLinkView(chat, nextChatSelected)
            }
        }
        if (filteredContactChats.isEmpty() && allChats.isNotEmpty()) {
            Column(Modifier.fillMaxSize().padding(DEFAULT_PADDING)) {
                Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
                    Text(
                        generalGetString(MR.strings.no_filtered_contacts),
                        color = MaterialTheme.colors.secondary
                    )
                }
            }
        }
    }
}

@Composable
fun ContactsSearchBar(
    listState: LazyListState,
    searchText: MutableState<TextFieldValue>,
    searchShowingSimplexLink: MutableState<Boolean>,
    searchChatFilteredBySimplexLink: MutableState<String?>,
    close: () -> Unit
) {
    var focused by remember { mutableStateOf(false) }

    Row(verticalAlignment = Alignment.CenterVertically) {
        val focusRequester = remember { FocusRequester() }
        Icon(painterResource(MR.images.ic_search), null, Modifier.padding(horizontal = DEFAULT_PADDING_HALF), tint = MaterialTheme.colors.secondary)
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

fun getContactTypes(baseContactTypes: List<ContactType>, searchEmpty: Boolean): List<ContactType> {
    return if (baseContactTypes.contains(ContactType.CHAT_DELETED) || searchEmpty) {
        baseContactTypes
    } else {
        baseContactTypes + ContactType.CHAT_DELETED
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

fun filteredContactChats(
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
    }
    else {
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

private fun viewNameContains(cInfo: ChatInfo, s: String): Boolean =
    cInfo.chatViewName.lowercase().contains(s.lowercase())

private fun connect(link: String, searchChatFilteredBySimplexLink: MutableState<String?>, close: () -> Unit, cleanup: (() -> Unit)?) {
    withBGApi {
        planAndConnect(
            chatModel.remoteHostId(),
            URI.create(link),
            incognito = null,
            filterKnownContact = { searchChatFilteredBySimplexLink.value = it.id },
            close = close,
            cleanup = cleanup,
        )
    }
}
