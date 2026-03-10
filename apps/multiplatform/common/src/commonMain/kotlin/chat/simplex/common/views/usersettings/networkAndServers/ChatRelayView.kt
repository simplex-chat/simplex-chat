package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
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

// TODO [relays] TBC matching relay to operator by domain (relay address can be hosted on operator server)
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
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  serverWarnings: MutableState<List<UserServersWarning>>,
  relay: MutableState<UserChatRelay>,
  rhId: Long?,
  close: () -> Unit
) {
  val relayToEdit = remember { mutableStateOf(relay.value) }

  ModalView(
    close = {
      val validName = validRelayName(relayToEdit.value.name)
      val validAddress = validRelayAddress(relayToEdit.value.address)
      if (validName && validAddress) {
        relay.value = relayToEdit.value
        withBGApi {
          validateServers_(rhId, userServers.value, serverErrors, serverWarnings)
        }
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
      onDelete = {
        relay.value = relay.value.copy(deleted = true)
        withBGApi {
          validateServers_(rhId, userServers.value, serverErrors, serverWarnings)
        }
        close()
      }
    )
  }
}

@Composable
private fun ChatRelayLayout(
  relay: MutableState<UserChatRelay>,
  onDelete: (() -> Unit)?
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.chat_relay))
    if (relay.value.preset) {
      PresetRelay(relay)
    } else {
      CustomRelay(relay, onDelete)
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun PresetRelay(relay: MutableState<UserChatRelay>) {
  SectionView(stringResource(MR.strings.preset_relay_name).uppercase()) {
    SectionItemView {
      Text(relay.value.name)
    }
  }
  SectionDividerSpaced()
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
  UseRelaySection(relay)
}

@Composable
private fun CustomRelay(
  relay: MutableState<UserChatRelay>,
  onDelete: (() -> Unit)?
) {
  val relayName = remember { mutableStateOf(relay.value.name) }
  val relayAddress = remember { mutableStateOf(relay.value.address) }
  val validName = remember { derivedStateOf { validRelayName(relayName.value) } }
  val validAddress = remember { derivedStateOf { validRelayAddress(relayAddress.value) } }

  LaunchedEffect(Unit) {
    snapshotFlow { relayName.value }
      .distinctUntilChanged()
      .collect { relay.value = relay.value.copy(name = it) }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { relayAddress.value }
      .distinctUntilChanged()
      .collect { relay.value = relay.value.copy(address = it) }
  }

  SectionView(
    stringResource(MR.strings.your_relay_name).uppercase(),
    icon = painterResource(MR.images.ic_error),
    iconTint = if (!validName.value) MaterialTheme.colors.error else Color.Transparent,
  ) {
    TextEditor(
      relayName,
      Modifier,
      placeholder = generalGetString(MR.strings.enter_relay_name)
    )
  }
  SectionDividerSpaced(maxTopPadding = true)

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

  UseRelaySection(relay, validAddress.value)

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
  valid: Boolean = true
) {
  SectionView(stringResource(MR.strings.use_relay).uppercase()) {
    SectionItemViewSpaceBetween(
      click = {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.not_implemented),
          text = generalGetString(MR.strings.relay_testing_not_available)
        )
      },
      disabled = !valid
    ) {
      Text(
        stringResource(MR.strings.test_relay),
        color = if (valid) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary
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
  onClick: () -> Unit
) {
  SectionItemView(onClick) {
    Box(Modifier.width(16.dp)) {
      if (!relay.enabled) {
        Icon(painterResource(MR.images.ic_do_not_disturb_on), null, tint = MaterialTheme.colors.secondary)
      } else {
        ShowRelayTestStatus(relay)
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

  ModalView(close = {
    addChatRelay(relayToEdit.value, userServers, serverErrors, serverWarnings, rhId, close)
  }) {
    NewChatRelayLayout(relayToEdit)
  }
}

@Composable
private fun NewChatRelayLayout(relay: MutableState<UserChatRelay>) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.new_chat_relay))
    CustomRelay(relay, onDelete = null)
    SectionBottomSpacer()
  }
}
