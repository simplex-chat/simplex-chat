package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.subscriberCountStr
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.async
import kotlinx.coroutines.coroutineScope
import kotlinx.serialization.json.JsonObject

enum class DirectoryFooter { Retry, ShowMore }

// Online results are not chats and are never saved: they live as long as the search text does.
// main thread only, as iOS's @MainActor: callers launch with withApi, and the generation checks rely on it
class OnlineSearchModel {
  var names by mutableStateOf<List<NameSearchResult>>(emptyList())
    private set
  var directory by mutableStateOf<List<DirectoryResult>>(emptyList())
    private set
  private val _loading = mutableStateOf(false)
  var loading: Boolean
    get() = _loading.value
    private set(value) { _loading.value = value; updateSpinner() }
  private val _resolvingNames = mutableStateOf(false)
  var resolvingNames: Boolean
    get() = _resolvingNames.value
    private set(value) { _resolvingNames.value = value; updateSpinner() }
  var failed by mutableStateOf(false)
    private set
  // tells "nothing searched yet" apart from "searched and found nothing"
  var searched by mutableStateOf(false)
    private set

  private var cursor by mutableStateOf<JsonObject?>(null)
  private var lastPageIds by mutableStateOf<Set<String>>(emptySet())
  private var searchedText = ""
  // bumped on every reset, so a reply that arrives too late cannot refill a list already cleared
  private var generation = 0
  private var spinnerShown = false

  val searching: Boolean get() = loading || resolvingNames
  val showResults: Boolean get() = searching || searched || names.isNotEmpty()
  val footer: DirectoryFooter? get() = if (failed) DirectoryFooter.Retry else if (cursor != null) DirectoryFooter.ShowMore else null

  fun reset() {
    generation += 1
    names = emptyList()
    directory = emptyList()
    cursor = null
    lastPageIds = emptySet()
    loading = false
    resolvingNames = false
    failed = false
    searched = false
    searchedText = ""
  }

  // a way out of the wait, not out of the results: what is already shown stays
  fun cancelRequest() {
    generation += 1
    loading = false
    resolvingNames = false
  }

  suspend fun search(text: String, name: String?) {
    val text = text.trim()
    if (text.isEmpty() || (searching && text == searchedText)) return
    reset()
    searchedText = text
    if (name != null) {
      resolvingNames = true
      // captured here: the async block runs later, in the child coroutine, possibly after a reset
      val gen = generation
      coroutineScope {
        val resolution = async { resolveNames(name, gen) }
        request(append = false)
        resolution.await()
      }
    } else {
      request(append = false)
    }
  }

  suspend fun searchDirectory(text: String) {
    val text = text.trim()
    if (text.isEmpty() || (loading && text == searchedText)) return
    if (text == searchedText) {
      directory = emptyList()
      cursor = null
      lastPageIds = emptySet()
      failed = false
      searched = false
    } else {
      reset()
      searchedText = text
    }
    request(append = false)
  }

  suspend fun loadMore() {
    if (cursor == null || loading) return
    request(append = true)
  }

  // a failed "Show more" keeps its cursor, so the retry resumes the page rather than starting over
  suspend fun retry() {
    if (cursor != null) {
      loadMore()
    } else if (!loading) {
      request(append = false)
    }
  }

  fun rows(shownChatIds: Set<ChatId>): OnlineSearchRows {
    val nameChatIds = names.mapNotNull { it.localChatId }.toSet()
    val nameLinks = names.mapNotNull { it.resolvedLink }.toSet()
    fun isAmong(ids: Set<ChatId>, id: ChatId?): Boolean =
      id?.let { ids.contains(it) } ?: false
    val lastPage = directory.filter { lastPageIds.contains(it.id) }
    return OnlineSearchRows(
      names = names.filter { !isAmong(shownChatIds, it.localChatId) },
      directory = directory.filter {
        !isAmong(shownChatIds, it.localChatId) && !isAmong(nameChatIds, it.localChatId) && !nameLinks.contains(it.entry.link)
      },
      // a result hidden only because a name resolved to its link is on screen, but not one of your chats
      pageInChats = lastPage.isNotEmpty() && lastPage.all {
        isAmong(shownChatIds, it.localChatId) || isAmong(nameChatIds, it.localChatId)
      },
      footer = footer
    )
  }

  // a chat opened from a result exists only once its connection flow ends, so rows without a chat look again then
  suspend fun refreshLocalChatIds() {
    val rh = chatModel.remoteHostId()
    val gen = generation
    val nameChatIds = mutableMapOf<String, ChatId>()
    for (result in names) {
      if (result.localChatId == null) localChatId(rh, result.target)?.let { nameChatIds[result.id] = it }
    }
    val directoryChatIds = mutableMapOf<String, ChatId>()
    for (result in directory) {
      if (result.localChatId == null) localChatId(rh, result.entry.link)?.let { directoryChatIds[result.id] = it }
    }
    if (gen != generation) return
    names = names.map { r -> r.withLocalChatId(r.localChatId ?: nameChatIds[r.id]) }
    directory = directory.map { r -> r.copy(localChatId = r.localChatId ?: directoryChatIds[r.id]) }
  }

  private suspend fun localChatId(rh: Long?, link: String): ChatId? {
    val plan = chatModel.controller.apiConnectPlan(rh, link, resolveMode = PlanResolveMode.PRMNever, inProgress = mutableStateOf(false), onFailure = ConnectPlanFailure.Log)
    return plan?.let { planLocalChatId(it.connectionPlan) }
  }

  private suspend fun resolveNames(name: String, gen: Int) {
    val rh = chatModel.remoteHostId()
    val targets = if (name.startsWith("@") || name.startsWith("#")) listOf(name) else listOf("@$name", "#$name")
    val results = mutableListOf<NameSearchResult>()
    for (target in targets) {
      // checked before each target: a cancelled or replaced search must not send more names to the server
      if (gen != generation) return
      val r = chatModel.controller.apiConnectPlan(rh, target, resolveMode = PlanResolveMode.PRMUnknown, inProgress = mutableStateOf(false), onFailure = ConnectPlanFailure.Log)
      val result = if (r != null) NameSearchResult(target, r) else null
      if (result != null) {
        results.add(result)
      }
    }
    if (gen != generation) return
    resolvingNames = false
    names = results
  }

  private suspend fun request(append: Boolean) {
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
        page.add(DirectoryResult(entry = entry, localChatId = localChatId(rh, entry.link)))
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
    directory = if (append) directory + page else page
    lastPageIds = page.map { it.id }.toSet()
  }

  private fun updateSpinner() {
    if (searching == spinnerShown) return
    spinnerShown = searching
    if (spinnerShown) {
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
fun DirectorySearchRow(entry: DirectorySearchEntry, modifier: Modifier = Modifier) {
  val displayName = "#" + entry.displayName
  val isChannel = entry.entryType.groupType == GroupType.Channel
  val summary = entry.entryType.summary
  val count = summary.publicMemberCount ?: summary.currentMembers
  val membersText = if (isChannel) subscriberCountStr(count) else String.format(generalGetString(MR.strings.directory_search_n_members), count)
  Row(
    modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
    horizontalArrangement = Arrangement.spacedBy(8.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    ProfileImage(
      size = 42.dp,
      image = entry.image,
      icon = if (isChannel) MR.images.ic_bigtop_updates_circle_filled else MR.images.ic_supervised_user_circle_filled
    )
    Column(Modifier.weight(1f), verticalArrangement = Arrangement.spacedBy(2.dp)) {
      Row(horizontalArrangement = Arrangement.spacedBy(4.dp), verticalAlignment = Alignment.CenterVertically) {
        Text(displayName, fontWeight = FontWeight.Bold, maxLines = 1, overflow = TextOverflow.Ellipsis)
        val simplexName = entry.simplexName
        if (simplexName != null) {
          // the directory's claim, shown as plain text: connecting uses the link
          Text(
            simplexName,
            Modifier.weight(1f, fill = false),
            color = MaterialTheme.colors.secondary,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
          )
        }
      }
      val descr = entry.shortDescr
      if (descr != null && descr.isNotEmpty()) {
        Text(descr, color = MaterialTheme.colors.secondary, maxLines = 2, overflow = TextOverflow.Ellipsis)
      }
      Text(membersText, style = MaterialTheme.typography.body2, color = MaterialTheme.colors.secondary)
    }
    Text(
      stringResource(if (isChannel) MR.strings.compose_view_join_channel else MR.strings.compose_view_join_group),
      color = MaterialTheme.colors.primary,
      maxLines = 1,
      softWrap = false
    )
  }
}

@Composable
fun NameSearchRow(result: NameSearchResult, modifier: Modifier = Modifier) {
  Row(
    modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
    horizontalArrangement = Arrangement.spacedBy(8.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    ProfileImage(size = 42.dp, image = result.image, icon = result.iconName)
    Column(Modifier.weight(1f), verticalArrangement = Arrangement.spacedBy(2.dp)) {
      val profileName = result.profileName
      if (profileName != null) {
        Text(
          buildAnnotatedString {
            withStyle(SpanStyle(fontWeight = FontWeight.Bold)) { append(profileName) }
            withStyle(SpanStyle(color = MaterialTheme.colors.secondary)) { append(" (${result.simplexName})") }
          },
          maxLines = 1,
          overflow = TextOverflow.Ellipsis
        )
      } else {
        Text(result.simplexName, fontWeight = FontWeight.Bold, maxLines = 1, overflow = TextOverflow.Ellipsis)
      }
      val descr = result.descr
      if (descr != null && descr.isNotEmpty()) {
        Text(descr, color = MaterialTheme.colors.secondary, maxLines = 2, overflow = TextOverflow.Ellipsis)
      }
    }
  }
}

@Composable
fun DirectoryPageInChatsRow() {
  Row(
    Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
  ) {
    Text(stringResource(MR.strings.directory_search_page_in_chats), color = MaterialTheme.colors.secondary)
  }
}

@Composable
fun DirectoryShowMoreRow(onlineSearch: OnlineSearchModel) {
  Row(
    Modifier
      .fillMaxWidth()
      .clickable(enabled = !onlineSearch.loading) {
        withApi { onlineSearch.loadMore() }
      }
      .padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
  ) {
    Text(
      stringResource(MR.strings.directory_search_show_more),
      color = if (onlineSearch.loading) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
    )
  }
}

@Composable
fun DirectoryRetryRow(onlineSearch: OnlineSearchModel) {
  Row(
    Modifier
      .fillMaxWidth()
      .clickable {
        withApi { onlineSearch.retry() }
      }
      .padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
  ) {
    Text(stringResource(MR.strings.directory_search_failed_retry), color = MaterialTheme.colors.primary)
  }
}

fun confirmDirectorySearch(search: () -> Unit) {
  if (appPrefs.directorySearchAlertShown.get()) {
    search()
  } else {
    showDirectorySearchAlert(onSearch = search)
  }
}

private fun showDirectorySearchAlert(onSearch: () -> Unit) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.search_in_directory_question),
    text = generalGetString(MR.strings.search_in_directory_alert_desc),
    parseHtml = false,
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          onSearch()
        }) {
          Text(stringResource(MR.strings.search_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          appPrefs.directorySearchAlertShown.set(true)
          onSearch()
        }) {
          Text(stringResource(MR.strings.search_and_dont_show_again), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
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
