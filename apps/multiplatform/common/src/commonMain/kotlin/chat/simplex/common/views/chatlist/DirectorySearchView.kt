package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chat.subscriberCountStr
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.PrepareGroupMode
import chat.simplex.common.views.newchat.planAndConnect
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.serialization.json.JsonObject

class OnlineSearchModel {
  var directory by mutableStateOf<List<DirectoryResult>>(emptyList())
    private set
  var moreCount by mutableStateOf<Int?>(null)
    private set
  private val _loading = mutableStateOf(false)
  var loading: Boolean
    get() = _loading.value
    private set(value) { if (value != _loading.value) { _loading.value = value; updateSpinner() } }
  var failed by mutableStateOf(false)
    private set
  var searched by mutableStateOf(false)
    private set

  private var cursor by mutableStateOf<JsonObject?>(null)
  private var searchedText = ""
  private var generation = 0

  val showResults: Boolean get() = loading || searched
  val footer: DirectoryFooter? get() = if (failed) DirectoryFooter.Retry else if (cursor != null) DirectoryFooter.LoadMore else null

  fun reset() {
    generation += 1
    directory = emptyList()
    moreCount = null
    cursor = null
    loading = false
    failed = false
    searched = false
    searchedText = ""
  }

  fun cancelRequest() {
    generation += 1
    loading = false
  }

  fun search(text: String) {
    val text = text.trim()
    if (text.isNotEmpty()) {
      confirmDirectorySearch { withApi { searchDirectory(text) } }
    }
  }

  private suspend fun searchDirectory(text: String) {
    if (text.isEmpty() || (loading && text == searchedText)) return
    reset()
    searchedText = text
    request()
  }

  fun loadMore() {
    confirmDirectorySearch { withApi { requestNextPage() } }
  }

  private suspend fun requestNextPage() {
    if (cursor == null || loading) return
    request()
  }

  fun retry() {
    confirmDirectorySearch { withApi { retryRequest() } }
  }

  private suspend fun retryRequest() {
    if (cursor != null) {
      requestNextPage()
    } else {
      searchDirectory(searchedText)
    }
  }

  fun rows(shownChatIds: Set<ChatId>): OnlineSearchRows {
    val yourChats = mutableListOf<ChatId>()
    val results = mutableListOf<DirectoryResult>()
    for (result in directory) {
      val state = result.state
      when {
        state is DirectoryResultState.YourChat && chatModel.getChat(state.chatId) != null ->
          if (!shownChatIds.contains(state.chatId) && !yourChats.contains(state.chatId)) {
            yourChats.add(state.chatId)
          }
        state is DirectoryResultState.YourChat ->
          results.add(result.copy(state = DirectoryResultState.Open))
        else ->
          results.add(result)
      }
    }
    return OnlineSearchRows(yourChats = yourChats, results = results, footer = footer)
  }

  fun refreshStates() {
    withApi {
      val rh = chatModel.remoteHostId()
      val gen = generation
      val states = mutableMapOf<String, DirectoryResultState>()
      for (result in directory) {
        if (result.state is DirectoryResultState.YourChat) continue
        states[result.id] = resultState(rh, result.entry.link)
      }
      if (gen != generation) return@withApi
      directory = directory.map { r -> r.copy(state = states[r.id] ?: r.state) }
    }
  }

  private suspend fun resultState(rh: Long?, link: String): DirectoryResultState {
    val plan = chatModel.controller.apiConnectPlan(rh, link, resolveMode = PlanResolveMode.PRMNever, inProgress = mutableStateOf(false))
    return directoryResultState(plan?.connectionPlan)
  }

  private suspend fun request() {
    val rh = chatModel.remoteHostId()
    val gen = generation
    loading = true
    failed = false
    val r = chatModel.controller.apiSearchDirectory(rh, searchedText, cursor)
    val page = mutableListOf<DirectoryResult>()
    if (r != null) {
      val seen = directory.map { it.id }.toMutableSet()
      for (entry in r.entries) {
        if (!seen.add(entry.id)) continue
        page.add(DirectoryResult(entry = entry, state = resultState(rh, entry.link)))
      }
    }
    if (gen != generation) return
    loading = false
    searched = true
    if (r == null) {
      failed = true
      return
    }
    cursor = r.cursor
    moreCount = r.moreCount
    directory = directory + page
  }

  private fun updateSpinner() {
    if (loading) {
      ConnectProgressManager.startConnectProgress(
        generalGetString(MR.strings.searching_directory),
        owner = ConnectProgressOwner.DirectorySearch
      ) {
        withApi { cancelRequest() }
      }
    } else {
      ConnectProgressManager.stopConnectProgress(ConnectProgressOwner.DirectorySearch)
    }
  }
}

@Composable
fun SearchInDirectoryRow(onSearch: () -> Unit) {
  val view = LocalMultiplatformView()
  Row(
    Modifier
      .fillMaxWidth()
      .clickable {
        hideKeyboard(view)
        onSearch()
      }
      .padding(vertical = DEFAULT_PADDING_HALF),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(painterResource(MR.images.ic_search), null, Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING_HALF).size(22.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.primary)
    Text(stringResource(MR.strings.search_in_directory), color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun DirectorySearchRow(
  entryType: DirectoryEntryType,
  name: String,
  simplexName: String?,
  shortDescr: String?,
  image: String?,
  imageSize: Dp,
  state: DirectoryResultState,
  modifier: Modifier = Modifier
) {
  val displayName = "#" + name
  val isChannel = entryType.groupType == GroupType.Channel
  val joining = state == DirectoryResultState.Joining
  val summary = entryType.summary
  val count = summary.publicMemberCount ?: summary.currentMembers
  val membersText = if (isChannel) subscriberCountStr(count) else String.format(generalGetString(MR.strings.directory_search_n_members), count)
  Row(
    modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.spacedBy(8.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    ProfileImage(
      size = imageSize,
      image = image,
      icon = if (isChannel) MR.images.ic_bigtop_updates_circle_filled else MR.images.ic_supervised_user_circle_filled
    )
    Column(Modifier.weight(1f), verticalArrangement = Arrangement.spacedBy(2.dp)) {
      Row(horizontalArrangement = Arrangement.spacedBy(4.dp), verticalAlignment = Alignment.CenterVertically) {
        Text(displayName, fontWeight = FontWeight.Bold, maxLines = 1, overflow = TextOverflow.Ellipsis)
        if (simplexName != null) {
          Text(
            simplexName,
            Modifier.weight(1f, fill = false),
            color = MaterialTheme.colors.secondary,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
        }
      }
      if (shortDescr != null && shortDescr.isNotEmpty()) {
        Text(shortDescr, color = MaterialTheme.colors.secondary, maxLines = 2, overflow = TextOverflow.Ellipsis)
      }
      Text(
        if (joining) stringResource(MR.strings.directory_search_already_joining) else membersText,
        style = MaterialTheme.typography.body2,
        color = MaterialTheme.colors.secondary
      )
    }
    if (!joining) {
      Text(
        stringResource(MR.strings.open_verb),
        color = MaterialTheme.colors.primary,
        maxLines = 1,
        softWrap = false
      )
    }
  }
}

@Composable
fun DirectoryFooterRow(footer: DirectoryFooter, onlineSearch: OnlineSearchModel) {
  when (footer) {
    DirectoryFooter.Retry ->
      DirectoryRetryRowLayout {
        onlineSearch.retry()
      }
    DirectoryFooter.LoadMore ->
      DirectoryLoadMoreRowLayout(moreCount = onlineSearch.moreCount, loading = onlineSearch.loading) {
        onlineSearch.loadMore()
      }
  }
}

@Composable
private fun DirectoryLoadMoreRowLayout(moreCount: Int?, loading: Boolean, onLoadMore: () -> Unit) {
  val color = if (loading) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
  Row(
    Modifier
      .fillMaxWidth()
      .clickable(enabled = !loading) {
        onLoadMore()
      }
      .padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(painterResource(MR.images.ic_zoom_in), null, Modifier.padding(end = DEFAULT_PADDING_HALF).size(22.dp * fontSizeSqrtMultiplier), tint = color)
    Text(
      if (moreCount != null && moreCount > 0) String.format(generalGetString(MR.strings.directory_search_load_more_n_left), moreCount)
      else stringResource(MR.strings.directory_search_load_more),
      color = color
    )
  }
}

@Composable
private fun DirectoryRetryRowLayout(onRetry: () -> Unit) {
  Row(
    Modifier
      .fillMaxWidth()
      .clickable {
        onRetry()
      }
      .padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(painterResource(MR.images.ic_refresh), null, Modifier.padding(end = DEFAULT_PADDING_HALF).size(22.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.primary)
    Text(stringResource(MR.strings.directory_search_failed_retry), color = MaterialTheme.colors.primary)
  }
}

@Composable
fun DirectoryResultNavLink(result: DirectoryResult, imageSize: Dp, previewModifier: Modifier = Modifier, close: (() -> Unit)?, cleanup: (() -> Unit)?) {
  val showMenu = remember { mutableStateOf(false) }
  val view = LocalMultiplatformView()
  val open = {
    hideKeyboard(view)
    openDirectoryResult(result, close, cleanup)
  }
  ChatListNavLinkLayout(
    chatLinkPreview = {
      DirectorySearchRow(
        entryType = result.entry.entryType,
        name = result.entry.displayName,
        simplexName = result.entry.simplexName,
        shortDescr = result.entry.shortDescr,
        image = result.entry.image,
        imageSize = imageSize,
        state = result.state,
        modifier = previewModifier
      )
    },
    click = open,
    dropdownMenuItems = {
      ItemAction(stringResource(MR.strings.open_verb), painterResource(MR.images.ic_chat_bubble), onClick = {
        showMenu.value = false
        open()
      })
    },
    showMenu = showMenu,
    disabled = false,
    selectedChat = remember { mutableStateOf(false) },
    nextChatSelected = remember { mutableStateOf(false) },
  )
}

fun openDirectoryResult(result: DirectoryResult, close: (() -> Unit)?, cleanup: (() -> Unit)?) {
  val prepareGroup = if (result.state == DirectoryResultState.Joining) PrepareGroupMode.Confirm else PrepareGroupMode.Open
  withBGApi { planAndConnect(chatModel.remoteHostId(), result.entry.link, close = close, cleanup = cleanup, prepareGroup = prepareGroup) }
}

fun confirmDirectorySearch(search: () -> Unit) {
  if (appPrefs.privacyDirectorySearch.get()) {
    search()
  } else {
    showDirectorySearchAlert(onSearch = search)
  }
}

private fun showDirectorySearchAlert(onSearch: () -> Unit) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.enable_directory_search_question),
    text = generalGetString(MR.strings.enable_directory_search_alert_desc),
    parseHtml = false,
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          appPrefs.privacyDirectorySearch.set(true)
          onSearch()
        }) {
          Text(stringResource(MR.strings.enable_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    }
  )
}

private const val DIRECTORY_PREVIEW_IMAGE = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAAT0lEQVR42u3V0QkAIAhFURdqxJZsodsCRUUqRO/i//kIzUptoWMC3gaAEIBRbgDzHABWXQHs9S3ASXqDKCB8DzI2OeMWZVxT/WgCBAjwng4ufNBwaLZEbgAAAABJRU5ErkJggg=="

@Composable
private fun PreviewDirectoryResult(
  name: String,
  entryType: DirectoryEntryType = DirectoryEntryType(summary = GroupSummary(currentMembers = 128)),
  simplexName: String? = null,
  shortDescr: String? = "Private chats",
  image: String? = null,
  state: DirectoryResultState = DirectoryResultState.Open
) {
  DirectorySearchRow(
    entryType = entryType,
    name = name,
    simplexName = simplexName,
    shortDescr = shortDescr,
    image = image,
    imageSize = 72.dp * fontSizeSqrtMultiplier,
    state = state,
    modifier = Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
  )
}

@Preview
@Composable
fun PreviewDirectorySearchRows() {
  SimpleXTheme {
    Column {
      PreviewDirectoryResult("Privacy")
      PreviewDirectoryResult(
        "SimpleX news",
        entryType = DirectoryEntryType(groupType = GroupType.Channel, summary = GroupSummary(currentMembers = 3, publicMemberCount = 2048)),
        shortDescr = "News about the app",
        image = DIRECTORY_PREVIEW_IMAGE
      )
      PreviewDirectoryResult("a")
      PreviewDirectoryResult(
        "The longest channel name that anyone could give to a group about private messaging",
        simplexName = "#private-messaging.simplex.chat",
        shortDescr = "Private messaging without user IDs: news, releases and help with servers, notifications and groups, with answers that never fit in the two lines of a row."
      )
      PreviewDirectoryResult("No description", shortDescr = null)
      PreviewDirectoryResult("Joining", state = DirectoryResultState.Joining)
    }
  }
}

@Preview
@Composable
fun PreviewDirectorySearchActionRows() {
  SimpleXTheme {
    Column {
      SearchInDirectoryRow(onSearch = {})
      DirectoryLoadMoreRowLayout(moreCount = 12, loading = false, onLoadMore = {})
      DirectoryLoadMoreRowLayout(moreCount = null, loading = false, onLoadMore = {})
      DirectoryLoadMoreRowLayout(moreCount = 12, loading = true, onLoadMore = {})
      DirectoryRetryRowLayout(onRetry = {})
    }
  }
}
