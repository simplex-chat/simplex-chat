package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.BackHandler
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.*
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
fun SelectedItemsTopToolbar(selectedChatItems: MutableState<Set<Long>?>) {
  val onBackClicked = { selectedChatItems.value = null }
  BackHandler(onBack = onBackClicked)
  val count = selectedChatItems.value?.size ?: 0
  DefaultTopAppBar(
    navigationButton = { NavigationButtonClose(onButtonClicked = onBackClicked) },
    title = {
      Text(
        if (count == 0) {
          stringResource(MR.strings.selected_chat_items_nothing_selected)
        } else {
          stringResource(MR.strings.selected_chat_items_selected_n).format(count)
        },
        fontWeight = FontWeight.SemiBold,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
    },
    onTitleClick = null,
    showSearch = false,
    onSearchValueChanged = {},
  )
  Divider(Modifier.padding(top = AppBarHeight * fontSizeSqrtMultiplier))
}

@Composable
fun SelectedItemsBottomToolbar(
  chatInfo: ChatInfo,
  chatItems: List<ChatItem>,
  selectedChatItems: MutableState<Set<Long>?>,
  deleteItems: (Boolean) -> Unit, // Boolean - delete for everyone is possible
  moderateItems: () -> Unit,
//  shareItems: () -> Unit,
) {
  val deleteEnabled = remember { mutableStateOf(false) }
  val deleteForEveryoneEnabled = remember { mutableStateOf(false) }
  val canModerate = remember { mutableStateOf(false) }
  val moderateEnabled = remember { mutableStateOf(false) }
  val allButtonsDisabled = remember { mutableStateOf(false) }
  Box {
    // It's hard to measure exact height of ComposeView with different fontSizes. Better to depend on actual ComposeView, even empty
    ComposeView(chatModel = chatModel, Chat.sampleData, remember { mutableStateOf(ComposeState(useLinkPreviews = false)) }, remember { mutableStateOf(null) }, {})
    Row(Modifier.matchParentSize().background(MaterialTheme.colors.background), horizontalArrangement = Arrangement.SpaceBetween, verticalAlignment = Alignment.CenterVertically) {
      IconButton({ deleteItems(deleteForEveryoneEnabled.value) }, enabled = deleteEnabled.value && !allButtonsDisabled.value) {
        Icon(
          painterResource(MR.images.ic_delete),
          null,
          Modifier.size(24.dp),
          tint = if (!deleteEnabled.value || allButtonsDisabled.value) MaterialTheme.colors.secondary else MaterialTheme.colors.error
        )
      }

      IconButton({ moderateItems() }, Modifier.alpha(if (canModerate.value) 1f else 0f), enabled = moderateEnabled.value && !allButtonsDisabled.value) {
        Icon(
          painterResource(MR.images.ic_flag),
          null,
          Modifier.size(24.dp),
          tint = if (!moderateEnabled.value || allButtonsDisabled.value) MaterialTheme.colors.secondary else MaterialTheme.colors.error
        )
      }

      IconButton({ /*shareItems()*/ }, Modifier.alpha(0f), enabled = false/*!allButtonsDisabled.value*/) {
        Icon(
          painterResource(MR.images.ic_share),
          null,
          Modifier.size(24.dp),
          tint = if (allButtonsDisabled.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
        )
      }
    }
  }
  LaunchedEffect(chatInfo, chatItems, selectedChatItems.value) {
    recheckItems(chatInfo, chatItems, selectedChatItems, deleteEnabled, deleteForEveryoneEnabled, canModerate, moderateEnabled, allButtonsDisabled)
  }
}

private fun recheckItems(chatInfo: ChatInfo,
  chatItems: List<ChatItem>,
  selectedChatItems: MutableState<Set<Long>?>,
  deleteEnabled: MutableState<Boolean>,
  deleteForEveryoneEnabled: MutableState<Boolean>,
  canModerate: MutableState<Boolean>,
  moderateEnabled: MutableState<Boolean>,
  allButtonsDisabled: MutableState<Boolean>
) {
  val count = selectedChatItems.value?.size ?: 0
  allButtonsDisabled.value = count == 0 || count > 20
  canModerate.value = possibleToModerate(chatInfo)
  val selected = selectedChatItems.value ?: return
  var rDeleteEnabled = true
  var rDeleteForEveryoneEnabled = true
  var rModerateEnabled = true
  var rOnlyOwnGroupItems = true
  val rSelectedChatItems = mutableSetOf<Long>()
  for (ci in chatItems) {
    if (selected.contains(ci.id)) {
      rDeleteEnabled = rDeleteEnabled && ci.canBeDeletedForSelf
      rDeleteForEveryoneEnabled = rDeleteForEveryoneEnabled && ci.meta.deletable && !ci.localNote
      rOnlyOwnGroupItems = rOnlyOwnGroupItems && ci.chatDir is CIDirection.GroupSnd
      rModerateEnabled = rModerateEnabled && ci.content.msgContent != null && ci.memberToModerate(chatInfo) != null
      rSelectedChatItems.add(ci.id) // we are collecting new selected items here to account for any changes in chat items list
    }
  }
  rModerateEnabled = rModerateEnabled && !rOnlyOwnGroupItems
  deleteEnabled.value = rDeleteEnabled
  deleteForEveryoneEnabled.value = rDeleteForEveryoneEnabled
  moderateEnabled.value = rModerateEnabled
  selectedChatItems.value = rSelectedChatItems
}

private fun possibleToModerate(chatInfo: ChatInfo): Boolean =
  chatInfo is ChatInfo.Group && chatInfo.groupInfo.membership.memberRole >= GroupMemberRole.Admin
