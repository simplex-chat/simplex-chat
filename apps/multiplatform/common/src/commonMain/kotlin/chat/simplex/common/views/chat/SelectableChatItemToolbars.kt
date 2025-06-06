package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.input.pointer.pointerInput
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
fun BoxScope.SelectedItemsCounterToolbar(selectedItems: MutableState<Set<Long>?>, onTop: Boolean, selectAll: (() -> Unit)? = null) {
  val onBackClicked = { selectedItems.value = null }
  BackHandler(onBack = onBackClicked)
  val count = selectedItems.value?.size ?: 0
  Box(if (onTop) Modifier else Modifier.imePadding()) {
    DefaultAppBar(
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
      onTop = onTop,
      onSearchValueChanged = {},
      buttons = if (selectAll != null) { { SelectAllButton(selectAll) } } else {{}}
    )
  }
}

@Composable
private fun SelectAllButton(onClick: () -> Unit) {
  IconButton(onClick) {
    Icon(
      painterResource(MR.images.ic_checklist), stringResource(MR.strings.back), Modifier.height(24.dp), tint = MaterialTheme.colors.primary
    )
  }
}

@Composable
fun SelectedItemsButtonsToolbar(
  chatsCtx: ChatModel.ChatsContext,
  chatInfo: ChatInfo,
  selectedChatItems: MutableState<Set<Long>?>,
  deleteItems: (Boolean) -> Unit, // Boolean - delete for everyone is possible
  archiveItems: () -> Unit,
  moderateItems: () -> Unit,
  forwardItems: () -> Unit,
) {
  val deleteEnabled = remember { mutableStateOf(false) }
  val deleteForEveryoneEnabled = remember { mutableStateOf(false) }
  val canArchiveReports = remember { mutableStateOf(false) }
  val canModerate = remember { mutableStateOf(false) }
  val moderateEnabled = remember { mutableStateOf(false) }
  val forwardEnabled = remember { mutableStateOf(false) }
  val deleteCountProhibited = remember { mutableStateOf(false) }
  val forwardCountProhibited = remember { mutableStateOf(false) }
  Box {
    // It's hard to measure exact height of ComposeView with different fontSizes. Better to depend on actual ComposeView, even empty
    ComposeView(chatModel = chatModel, Chat.sampleData, remember { mutableStateOf(ComposeState(useLinkPreviews = false)) }, remember { mutableStateOf(null) }, {}, remember { FocusRequester() })
    Row(
      Modifier
        .matchParentSize()
        .background(MaterialTheme.colors.background)
        .padding(horizontal = 2.dp)
        .height(AppBarHeight * fontSizeSqrtMultiplier)
        .pointerInput(Unit) {
          detectGesture {
            true
          }
        },
      horizontalArrangement = Arrangement.SpaceBetween,
      verticalAlignment = Alignment.CenterVertically
    ) {
      IconButton({ if (canArchiveReports.value) archiveItems() else deleteItems(deleteForEveryoneEnabled.value) }, enabled = deleteEnabled.value && !deleteCountProhibited.value) {
        Icon(
          painterResource(MR.images.ic_delete),
          null,
          Modifier.size(22.dp),
          tint = if (!deleteEnabled.value || deleteCountProhibited.value) MaterialTheme.colors.secondary else MaterialTheme.colors.error
        )
      }

      IconButton({ moderateItems() }, Modifier.alpha(if (canModerate.value) 1f else 0f), enabled = moderateEnabled.value && !deleteCountProhibited.value) {
        Icon(
          painterResource(MR.images.ic_flag),
          null,
          Modifier.size(22.dp),
          tint = if (!moderateEnabled.value || deleteCountProhibited.value) MaterialTheme.colors.secondary else MaterialTheme.colors.error
        )
      }

      IconButton({ forwardItems() }, enabled = forwardEnabled.value && !forwardCountProhibited.value) {
        Icon(
          painterResource(MR.images.ic_forward),
          null,
          Modifier.size(22.dp),
          tint = if (!forwardEnabled.value || forwardCountProhibited.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
        )
      }
    }
    Divider(Modifier.align(Alignment.TopStart))
  }
  val chatItems = remember { derivedStateOf { chatsCtx.chatItems.value } }
  LaunchedEffect(chatInfo, chatItems.value, selectedChatItems.value) {
    recheckItems(chatInfo, chatItems.value, selectedChatItems, deleteEnabled, deleteForEveryoneEnabled, canArchiveReports, canModerate, moderateEnabled, forwardEnabled, deleteCountProhibited, forwardCountProhibited)
  }
}

private fun recheckItems(chatInfo: ChatInfo,
  chatItems: List<ChatItem>,
  selectedChatItems: MutableState<Set<Long>?>,
  deleteEnabled: MutableState<Boolean>,
  deleteForEveryoneEnabled: MutableState<Boolean>,
  canArchiveReports:  MutableState<Boolean>,
  canModerate: MutableState<Boolean>,
  moderateEnabled: MutableState<Boolean>,
  forwardEnabled: MutableState<Boolean>,
  deleteCountProhibited: MutableState<Boolean>,
  forwardCountProhibited: MutableState<Boolean>
) {
  val count = selectedChatItems.value?.size ?: 0
  deleteCountProhibited.value = count == 0 || count > 200
  forwardCountProhibited.value = count == 0 || count > 20
  canModerate.value = possibleToModerate(chatInfo)
  val selected = selectedChatItems.value ?: return
  var rDeleteEnabled = true
  var rDeleteForEveryoneEnabled = true
  var rCanArchiveReports = true
  var rModerateEnabled = true
  var rOnlyOwnGroupItems = true
  var rForwardEnabled = true
  val rSelectedChatItems = mutableSetOf<Long>()
  for (ci in chatItems) {
    if (selected.contains(ci.id)) {
      rDeleteEnabled = rDeleteEnabled && ci.canBeDeletedForSelf
      rDeleteForEveryoneEnabled = rDeleteForEveryoneEnabled && ci.meta.deletable && !ci.localNote && !ci.isReport
      rCanArchiveReports = rCanArchiveReports && ci.isActiveReport && ci.chatDir !is CIDirection.GroupSnd && chatInfo is ChatInfo.Group && chatInfo.groupInfo.membership.memberRole >= GroupMemberRole.Moderator
      rOnlyOwnGroupItems = rOnlyOwnGroupItems && ci.chatDir is CIDirection.GroupSnd && !ci.isReport
      rModerateEnabled = rModerateEnabled && ci.content.msgContent != null && ci.memberToModerate(chatInfo) != null && !ci.isReport
      rForwardEnabled = rForwardEnabled && ci.content.msgContent != null && ci.meta.itemDeleted == null && !ci.isLiveDummy && !ci.isReport
      rSelectedChatItems.add(ci.id) // we are collecting new selected items here to account for any changes in chat items list
    }
  }
  rModerateEnabled = rModerateEnabled && !rOnlyOwnGroupItems
  deleteEnabled.value = rDeleteEnabled
  deleteForEveryoneEnabled.value = rDeleteForEveryoneEnabled
  canArchiveReports.value = rCanArchiveReports
  moderateEnabled.value = rModerateEnabled
  forwardEnabled.value = rForwardEnabled
  selectedChatItems.value = rSelectedChatItems
}

private fun possibleToModerate(chatInfo: ChatInfo): Boolean =
  chatInfo is ChatInfo.Group && chatInfo.groupInfo.membership.memberRole >= GroupMemberRole.Moderator
