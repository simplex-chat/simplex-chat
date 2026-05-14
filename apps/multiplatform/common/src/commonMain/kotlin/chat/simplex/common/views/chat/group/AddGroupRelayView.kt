package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionCustomFooter
import SectionDividerSpaced
import SectionItemView
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.chatRelayDisplayName
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.launch

data class AvailableRelay(
  val relayId: Long,
  val relay: UserChatRelay,
  val operatorName: String?
)

@Composable
fun AddGroupRelayView(
  groupInfo: GroupInfo,
  existingRelayIds: Set<Long>,
  onRelayAdded: () -> Unit,
  close: () -> Unit
) {
  var availableRelays by remember { mutableStateOf<List<AvailableRelay>>(emptyList()) }
  var selectedRelayIds by remember { mutableStateOf<Set<Long>>(emptySet()) }
  var isLoading by remember { mutableStateOf(true) }
  var isAdding by remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()

  BackHandler(onBack = close)

  LaunchedEffect(Unit) {
    try {
      val servers = ChatController.getUserServers(null)
      if (servers != null) {
        val relays = mutableListOf<AvailableRelay>()
        for (op in servers) {
          if (op.operator != null && op.operator.enabled != true) continue
          val opName: String? = if (op.operator?.operatorTag != null) op.operator.tradeName else null
          for (relay in op.chatRelays) {
            val relayId = relay.chatRelayId
            if (relay.enabled && !relay.deleted && relayId != null && relayId !in existingRelayIds) {
              relays.add(AvailableRelay(relayId, relay, opName))
            }
          }
        }
        availableRelays = relays
      }
    } catch (e: Exception) {
      Log.e(TAG, "loadAvailableRelays error: ${e.message}")
    }
    isLoading = false
  }

  AddGroupRelayLayout(
    availableRelays = availableRelays,
    selectedRelayIds = selectedRelayIds,
    isLoading = isLoading,
    isAdding = isAdding,
    onToggleRelay = { relayId ->
      selectedRelayIds = if (relayId in selectedRelayIds) selectedRelayIds - relayId else selectedRelayIds + relayId
    },
    onAddRelays = {
      val relayIds = selectedRelayIds.toList()
      if (relayIds.isEmpty()) return@AddGroupRelayLayout
      isAdding = true
      scope.launch {
        addSelectedRelays(groupInfo, relayIds, selectedRelayIds, availableRelays, onRelayAdded, close) { newSelectedIds, newAvailableRelays ->
          selectedRelayIds = newSelectedIds
          availableRelays = newAvailableRelays
          isAdding = false
        }
      }
    }
  )
}

@Composable
private fun AddGroupRelayLayout(
  availableRelays: List<AvailableRelay>,
  selectedRelayIds: Set<Long>,
  isLoading: Boolean,
  isAdding: Boolean,
  onToggleRelay: (Long) -> Unit,
  onAddRelays: () -> Unit
) {
  ColumnWithScrollBar {
    AppBarTitle(generalGetString(MR.strings.add_relays_title))

    if (isLoading) {
      Box(Modifier.fillMaxWidth().padding(vertical = DEFAULT_PADDING), contentAlignment = Alignment.Center) {
        CircularProgressIndicator()
      }
    } else if (availableRelays.isEmpty()) {
      SectionView {
        SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          Text(
            generalGetString(MR.strings.no_available_relays),
            color = MaterialTheme.colors.secondary
          )
        }
      }
    } else {
      SectionView {
        AddRelaysButton(
          onClick = onAddRelays,
          disabled = selectedRelayIds.isEmpty() || isAdding
        )
      }
      SectionCustomFooter {
        val count = selectedRelayIds.size
        Text(
          if (count == 0) generalGetString(MR.strings.no_relays_selected)
          else String.format(generalGetString(MR.strings.num_relays_selected), count),
          color = MaterialTheme.colors.secondary,
          lineHeight = 18.sp,
          fontSize = 14.sp
        )
      }
      SectionDividerSpaced(maxTopPadding = true)
      SectionView(generalGetString(MR.strings.select_relays).uppercase()) {
        availableRelays.forEach { item ->
          val selected = item.relayId in selectedRelayIds
          SectionItemView(
            click = { onToggleRelay(item.relayId) },
            padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = 4.dp)
          ) {
            Column(Modifier.weight(1f)) {
              Text(
                chatRelayDisplayName(item.relay),
                maxLines = 1,
                color = MaterialTheme.colors.onBackground
              )
              if (item.operatorName != null) {
                Text(
                  item.operatorName,
                  fontSize = 12.sp,
                  maxLines = 1,
                  color = MaterialTheme.colors.secondary
                )
              }
            }
            Spacer(Modifier.width(8.dp))
            Icon(
              painterResource(if (selected) MR.images.ic_check_circle_filled else MR.images.ic_circle),
              contentDescription = null,
              tint = if (selected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
              modifier = Modifier.size(24.dp)
            )
          }
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun AddRelaysButton(onClick: () -> Unit, disabled: Boolean) {
  SettingsActionItem(
    painterResource(MR.images.ic_check),
    generalGetString(MR.strings.add_relays_title),
    click = onClick,
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary,
    disabled = disabled,
  )
}

private suspend fun addSelectedRelays(
  groupInfo: GroupInfo,
  relayIds: List<Long>,
  selectedRelayIds: Set<Long>,
  availableRelays: List<AvailableRelay>,
  onRelayAdded: () -> Unit,
  close: () -> Unit,
  updateState: (Set<Long>, List<AvailableRelay>) -> Unit
) {
  try {
    val result = ChatController.apiAddGroupRelays(groupInfo.groupId, relayIds)
    if (result == null) {
      updateState(selectedRelayIds, availableRelays)
      return
    }
    when (result) {
      is ChatController.AddGroupRelaysResult.Added -> {
        ChannelRelaysModel.set(groupId = result.groupInfo.groupId, groupRelays = result.groupRelays)
        onRelayAdded()
        close()
      }
      is ChatController.AddGroupRelaysResult.AddFailed -> {
        val results = result.addRelayResults
        val successIds = results.filter { it.relayError == null }.mapNotNull { it.relay.chatRelayId }.toSet()
        var newSelectedIds = selectedRelayIds
        var newAvailableRelays = availableRelays
        if (successIds.isNotEmpty()) {
          newSelectedIds = selectedRelayIds - successIds
          newAvailableRelays = availableRelays.filter { it.relayId !in successIds }
          onRelayAdded()
        }
        val errorLines = results.filter { it.relayError != null }
          .map { "${chatRelayDisplayName(it.relay)}: ${it.relayError?.let { e -> ChatController.connErrorText(e) } ?: ""}" }
        val successNames = results.filter { it.relayError == null }
          .map { chatRelayDisplayName(it.relay) }
        var msg = errorLines.joinToString("\n")
        if (successNames.isNotEmpty()) {
          msg += "\n" + String.format(generalGetString(MR.strings.relays_added_format), successNames.joinToString(", "))
        }
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.error_adding_relays),
          text = msg
        )
        updateState(newSelectedIds, newAvailableRelays)
      }
    }
  } catch (e: Exception) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.error_adding_relays),
      text = e.message ?: ""
    )
    updateState(selectedRelayIds, availableRelays)
  }
}
