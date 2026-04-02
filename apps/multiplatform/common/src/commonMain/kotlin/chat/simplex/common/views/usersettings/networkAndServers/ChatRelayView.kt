package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.sp
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.PreferenceToggle
import chat.simplex.res.MR
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch

@Composable
fun ShowRelayTestStatus(relay: UserChatRelay, modifier: Modifier = Modifier) =
  when (relay.tested) {
    true -> Icon(painterResource(MR.images.ic_check), null, modifier, tint = SimplexGreen)
    false -> Icon(painterResource(MR.images.ic_close), null, modifier, tint = MaterialTheme.colors.error)
    else -> Icon(painterResource(MR.images.ic_check), null, modifier, tint = Color.Transparent)
  }

fun validRelayName(name: String): Boolean =
  name.isNotEmpty() && isValidDisplayName(name)

fun showInvalidRelayNameAlert(name: MutableState<String>) {
  val validName = mkValidName(name.value)
  if (validName.isEmpty()) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.invalid_name)
    )
  } else {
    AlertManager.shared.showAlertDialog(
      title = generalGetString(MR.strings.invalid_name),
      text = String.format(generalGetString(MR.strings.correct_name_to), validName),
      onConfirm = {
        name.value = validName
      }
    )
  }
}

fun validRelayAddress(address: String): Boolean {
  val parsedMd = parseToMarkdown(address)
  return parsedMd != null &&
    parsedMd.size == 1 &&
    parsedMd.first().format is Format.SimplexLink &&
    (parsedMd.first().format as Format.SimplexLink).linkType == SimplexLinkType.relay
}

fun addChatRelay(
  relay: UserChatRelay,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  serverWarnings: MutableState<List<UserServersWarning>>?,
  rhId: Long?,
  close: () -> Unit
) {
  val nameEmpty = relay.name.trim().isEmpty()
  val addressEmpty = relay.address.trim().isEmpty()
  if (nameEmpty && addressEmpty) {
    close()
  } else if (!validRelayName(relay.name)) {
    close()
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.invalid_relay_name),
      text = generalGetString(MR.strings.check_relay_name)
    )
  } else if (!validRelayAddress(relay.address)) {
    close()
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.invalid_relay_address),
      text = generalGetString(MR.strings.check_relay_address)
    )
  } else {
    val i = userServers.value.indexOfFirst { it.operator == null }
    if (i != -1) {
      val updatedUserServers = userServers.value.toMutableList()
      val operatorServers = updatedUserServers[i]
      updatedUserServers[i] = operatorServers.copy(
        chatRelays = operatorServers.chatRelays + relay
      )
      userServers.value = updatedUserServers
      withBGApi {
        validateServers_(rhId, userServers.value, serverErrors, serverWarnings)
      }
      close()
    } else { // Shouldn't happen
      close()
      AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error_adding_relay))
    }
  }
}

@Composable
fun ChatRelayView(
  relay: UserChatRelay,
  onDelete: () -> Unit,
  onUpdate: (UserChatRelay) -> Unit,
  close: () -> Unit
) {
  val relayToEdit = remember { mutableStateOf(relay) }

  LaunchedEffect(Unit) {
    snapshotFlow { relayToEdit.value.address }
      .distinctUntilChanged()
      .collect {
        if (relayToEdit.value.address == relay.address) {
          relayToEdit.value = relayToEdit.value.copy(tested = relay.tested, relayProfile = relay.relayProfile)
        } else {
          relayToEdit.value = relayToEdit.value.copy(tested = null)
        }
      }
  }

  ModalView(
    close = {
      val validName = validRelayName(relayToEdit.value.name)
      val validAddress = validRelayAddress(relayToEdit.value.address)
      if (validName && validAddress) {
        onUpdate(relayToEdit.value)
        close()
      } else if (!validName) {
        close()
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.invalid_relay_name),
          text = generalGetString(MR.strings.check_relay_name)
        )
      } else {
        close()
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.invalid_relay_address),
          text = generalGetString(MR.strings.check_relay_address)
        )
      }
    }
  ) {
    ChatRelayLayout(
      relayToEdit,
      onDelete = onDelete
    )
  }
}

@Composable
private fun ChatRelayLayout(
  relay: MutableState<UserChatRelay>,
  onDelete: (() -> Unit)?
) {
  val testing = remember { mutableStateOf(false) }
  Box {
    ColumnWithScrollBar {
      AppBarTitle(stringResource(MR.strings.chat_relay))
      if (relay.value.preset) {
        PresetRelay(relay, testing)
      } else {
        CustomRelay(relay, onDelete, testing)
      }
      SectionBottomSpacer()
    }
    if (testing.value) {
      DefaultProgressView(null)
    }
  }
}

@Composable
private fun PresetRelay(relay: MutableState<UserChatRelay>, testing: MutableState<Boolean>) {
  SectionView(stringResource(MR.strings.preset_relay_address).uppercase()) {
    SelectionContainer {
      Text(
        relay.value.address,
        Modifier.padding(start = DEFAULT_PADDING, top = 5.dp, end = DEFAULT_PADDING, bottom = 10.dp),
        color = MaterialTheme.colors.secondary
      )
    }
  }
  SectionDividerSpaced()
  SectionView(stringResource(MR.strings.preset_relay_name).uppercase()) {
    SectionItemView {
      Text(relay.value.name)
    }
  }
  SectionDividerSpaced()
  UseRelaySection(relay, testing = testing)
}

@Composable
private fun CustomRelay(
  relay: MutableState<UserChatRelay>,
  onDelete: (() -> Unit)?,
  testing: MutableState<Boolean>
) {
  val relayName = remember { mutableStateOf(relay.value.name) }
  val relayAddress = remember { mutableStateOf(relay.value.address) }
  val validName = remember { derivedStateOf { validRelayName(relayName.value) } }
  val validAddress = remember { derivedStateOf { validRelayAddress(relayAddress.value) } }

  LaunchedEffect(Unit) {
    snapshotFlow { relayName.value }
      .distinctUntilChanged()
      .collect { relay.value = relay.value.copyWithName(it) }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { relay.value.name }
      .distinctUntilChanged()
      .collect { relayName.value = it }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { relayAddress.value }
      .distinctUntilChanged()
      .collect { relay.value = relay.value.copy(address = it) }
  }

  SectionView(
    stringResource(MR.strings.your_relay_address).uppercase(),
    icon = painterResource(MR.images.ic_error),
    iconTint = if (!validAddress.value) MaterialTheme.colors.error else Color.Transparent,
  ) {
    TextEditor(
      relayAddress,
      Modifier.height(144.dp)
    )
  }
  SectionDividerSpaced(maxTopPadding = true)

  Column {
    val iconSize = with(LocalDensity.current) { 21.sp.toDp() }
    Row(Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), verticalAlignment = Alignment.CenterVertically) {
      Text(
        stringResource(MR.strings.your_relay_name).uppercase(),
        color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2, fontSize = 12.sp
      )
      IconButton(
        onClick = { if (!validName.value) showInvalidRelayNameAlert(relayName) },
        enabled = !validName.value,
        modifier = Modifier.padding(start = DEFAULT_PADDING_HALF).size(iconSize)
      ) {
        Icon(
          painterResource(MR.images.ic_error), null,
          tint = if (!validName.value) MaterialTheme.colors.error else Color.Transparent
        )
      }
    }
    Column(Modifier.fillMaxWidth()) {
      TextEditor(
        relayName,
        Modifier,
        placeholder = generalGetString(MR.strings.enter_relay_name),
        enabled = relay.value.tested != true
      )
    }
  }
  if (relay.value.tested != true) {
    SectionTextFooter(annotatedStringResource(MR.strings.test_relay_to_retrieve_name))
  }
  SectionDividerSpaced(maxTopPadding = true)

  UseRelaySection(relay, validAddress.value, testing)

  if (onDelete != null) {
    SectionDividerSpaced()
    SectionView {
      SectionItemView(onDelete) {
        Text(stringResource(MR.strings.delete_relay), color = MaterialTheme.colors.error)
      }
    }
  }
}

@Composable
private fun UseRelaySection(
  relay: MutableState<UserChatRelay>,
  valid: Boolean = true,
  testing: MutableState<Boolean>
) {
  val scope = rememberCoroutineScope()
  SectionView(stringResource(MR.strings.use_relay).uppercase()) {
    SectionItemViewSpaceBetween(
      click = {
        testing.value = true
        relay.value = relay.value.copy(tested = null)
        scope.launch {
          val f = testRelayConnection(relay)
          if (f != null) {
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.relay_test_failed_alert),
              text = f.localizedDescription
            )
          }
          testing.value = false
        }
      },
      disabled = !valid || testing.value
    ) {
      Text(
        stringResource(MR.strings.test_relay),
        color = if (valid && !testing.value) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary
      )
      ShowRelayTestStatus(relay.value)
    }

    val enabled = rememberUpdatedState(relay.value.enabled)
    PreferenceToggle(
      stringResource(MR.strings.use_for_new_channels),
      checked = enabled.value
    ) {
      relay.value = relay.value.copy(enabled = it)
    }
  }
}

@Composable
fun ChatRelayViewLink(
  relay: UserChatRelay,
  duplicateRelayNames: Set<String>,
  duplicateRelayAddresses: Set<String>,
  onClick: () -> Unit
) {
  SectionItemView(onClick) {
    Box(Modifier.width(16.dp)) {
      when {
        relay.name in duplicateRelayNames || relay.address in duplicateRelayAddresses -> InvalidServer()
        !relay.enabled -> Icon(painterResource(MR.images.ic_do_not_disturb_on), null, tint = MaterialTheme.colors.secondary)
        else -> ShowRelayTestStatus(relay)
      }
    }
    Spacer(Modifier.padding(horizontal = 4.dp))
    val displayName = relay.name.ifEmpty { relay.domains.firstOrNull() ?: relay.address }
    if (relay.enabled) {
      Text(displayName, color = MaterialTheme.colors.onBackground, maxLines = 1)
    } else {
      Text(displayName, maxLines = 1, color = MaterialTheme.colors.secondary)
    }
  }
}

@Composable
fun ModalData.NewChatRelayView(
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  serverWarnings: MutableState<List<UserServersWarning>>,
  rhId: Long?,
  close: () -> Unit
) {
  val relayToEdit = remember {
    mutableStateOf(
      UserChatRelay(
        chatRelayId = null, address = "", name = "", domains = emptyList(),
        preset = false, tested = null, enabled = true, deleted = false
      )
    )
  }

  LaunchedEffect(Unit) {
    snapshotFlow { relayToEdit.value.address }
      .distinctUntilChanged()
      .collect {
        relayToEdit.value = relayToEdit.value.copy(tested = null)
      }
  }

  ModalView(close = {
    addChatRelay(relayToEdit.value, userServers, serverErrors, serverWarnings, rhId, close)
  }) {
    NewChatRelayLayout(relayToEdit)
  }
}

@Composable
private fun NewChatRelayLayout(relay: MutableState<UserChatRelay>) {
  val testing = remember { mutableStateOf(false) }
  Box {
    ColumnWithScrollBar {
      AppBarTitle(stringResource(MR.strings.new_chat_relay))
      CustomRelay(relay, onDelete = null, testing = testing)
      SectionBottomSpacer()
    }
    if (testing.value) {
      DefaultProgressView(null)
    }
  }
}

suspend fun testRelayConnection(relay: MutableState<UserChatRelay>): RelayTestFailure? =
  try {
    val (relayProfile, testFailure) = chatModel.controller.testChatRelay(chatModel.remoteHostId(), relay.value.address)
    if (testFailure != null) {
      relay.value = relay.value.copy(tested = false)
      testFailure
    } else {
      relay.value = relay.value.copy(tested = true).let {
        if (relayProfile != null) it.copyWithName(relayProfile.name) else it
      }
      null
    }
  } catch (e: Exception) {
    Log.e(TAG, "testRelayConnection ${e.stackTraceToString()}")
    relay.value = relay.value.copy(tested = false)
    null
  }
