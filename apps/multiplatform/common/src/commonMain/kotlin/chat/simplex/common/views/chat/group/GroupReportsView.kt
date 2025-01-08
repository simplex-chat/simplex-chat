package chat.simplex.common.views.chat.group

import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.ChatView
import chat.simplex.common.views.chat.apiLoadMessages
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch

val LocalContentTag: ProvidableCompositionLocal<MsgContentTag?> = staticCompositionLocalOf { null }

data class GroupReports(
  val reportsCount: Int,
  val reportsView: Boolean,
  val showArchived: Boolean = false
) {
  val showBar: Boolean = reportsCount > 0 && !reportsView

  fun toContentFilter(): ContentFilter? {
    if (!reportsView) return null
    return ContentFilter(MsgContentTag.Report, deleted = showArchived)
  }

  val contentTag: MsgContentTag? = if (!reportsView) null else MsgContentTag.Report
}

@Composable
fun GroupReportsView(staleChatId: State<String?>, scrollToItemId: MutableState<Long?>) {
  ChatView(staleChatId, reportsView = true, scrollToItemId, onComposed = {})
}

@Composable
fun GroupReportsAppBar(
  groupReports: State<GroupReports>,
  close: () -> Unit,
  showArchived: (Boolean) -> Unit,
  onSearchValueChanged: (String) -> Unit
) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val showMenu = remember { mutableStateOf(false) }
  val showSearch = rememberSaveable { mutableStateOf(false) }
  val onBackClicked = {
    if (!showSearch.value) {
      close()
    } else {
      onSearchValueChanged("")
      showSearch.value = false
    }
  }
  BackHandler(onBack = onBackClicked)
  DefaultAppBar(
    navigationButton = { NavigationButtonBack(onBackClicked) },
    fixedTitleText = stringResource(MR.strings.group_reports_member_reports),
    onTitleClick = null,
    onTop = !oneHandUI.value,
    showSearch = showSearch.value,
    onSearchValueChanged = onSearchValueChanged,
    buttons = {
      IconButton({ showMenu.value = true }) {
        Icon(MoreVertFilled, stringResource(MR.strings.icon_descr_more_button), tint = MaterialTheme.colors.primary)
      }
      val onClosedAction = remember { mutableStateOf({}) }
      DefaultDropdownMenu(
        showMenu,
        onClosed = onClosedAction
      ) {
        ItemAction(stringResource(MR.strings.search_verb), painterResource(MR.images.ic_search), onClick = {
          showMenu.value = false
          showSearch.value = true
        })
        ItemAction(
          if (groupReports.value.showArchived) stringResource(MR.strings.group_reports_show_active) else stringResource(MR.strings.group_reports_show_archived),
          painterResource(if (groupReports.value.showArchived) MR.images.ic_flag else MR.images.ic_inventory_2),
          onClick = {
            onClosedAction.value = {
              showArchived(!groupReports.value.showArchived)
              onClosedAction.value = {}
            }
            showMenu.value = false
          }
        )
      }
    }
  )
  ItemsReload(groupReports)
}

@Composable
private fun ItemsReload(groupReports: State<GroupReports>) {
  LaunchedEffect(Unit) {
    snapshotFlow { groupReports.value.showArchived to chatModel.chatId.value }
      .distinctUntilChanged()
      .drop(1)
      .map { it.second }
      .filterNotNull()
      .map { chatModel.getChat(it) }
      .filterNotNull()
      .filter { it.chatInfo is ChatInfo.Group }
      .collect { chat ->
        reloadItems(chat, groupReports)
      }
  }
}
private suspend fun reloadItems(chat: Chat, groupReports: State<GroupReports>) {
  val contentFilter = groupReports.value.toContentFilter()
  if (chat.chatStats.reportsCount > 0 || contentFilter?.deleted == true) {
    apiLoadMessages(chat.remoteHostId, chat.chatInfo.chatType, chat.chatInfo.apiId, contentFilter, ChatPagination.Initial(ChatPagination.INITIAL_COUNT))
  } else {
    openLoadedChat(chat.copy(chatItems = emptyList()), contentTag = contentFilter?.mcTag)
  }
}