package chat.simplex.common.views.chat.group

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.height
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.ChatView
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

data class GroupReports(
  val activeReports: Int,
  val reportsView: Boolean,
  val showArchived: Boolean = false
) {
  val showBar: Boolean = activeReports > 0 && !reportsView
}

@Composable
fun GroupReportsView(staleChatId: State<String?>) {
  ChatView(staleChatId, reportsView = true, onComposed = {})
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
          if (groupReports.value.showArchived) stringResource(MR.strings.group_reports_hide_archived) else stringResource(MR.strings.group_reports_show_archived),
          painterResource(MR.images.ic_add),
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
}
