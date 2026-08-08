package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch

// Names other people have sent to this profile.
//
// A name lands at a one-time address that only this profile can find, and stays
// there until the user decides. Accepting is what links this profile to the
// name on chain, and it costs a purchase; declining writes nothing and leaves
// no trace. Both facts belong on the screen before either button is pressed,
// not in a confirmation afterwards.
@Composable
fun IncomingNamesView(rhId: Long?, close: () -> Unit) {
  val incoming = remember { mutableStateOf(emptyList<IncomingName>()) }
  val loading = remember { mutableStateOf(true) }
  val canReceive = remember { mutableStateOf(true) }
  val scope = rememberCoroutineScope()

  suspend fun reload() {
    // Read-only: opening this screen must not create keys by itself.
    canReceive.value = chatModel.controller.apiNameStatus(rhId)?.nameHasWallet ?: false
    incoming.value = if (canReceive.value) chatModel.controller.apiNameIncoming(rhId) ?: emptyList() else emptyList()
    loading.value = false
  }

  LaunchedEffect(Unit) { reload() }

  IncomingNamesLayout(
    incoming = incoming.value,
    loading = loading.value,
    canReceive = canReceive.value,
    enableReceiving = {
      scope.launch {
        loading.value = true
        // Creates the wallet and publishes the receiving address to contacts.
        val addr = chatModel.controller.apiNameAddress(rhId)
        reload()
        if (addr != null) {
          // A recovery key now exists and is unsaved, and it is the only way
          // back to anything received here.
          AlertManager.shared.showAlertDialog(
            title = generalGetString(MR.strings.names_receive_enabled_title),
            text = generalGetString(MR.strings.names_receive_enabled_text),
            confirmText = generalGetString(MR.strings.names_bought_save_key),
            dismissText = generalGetString(MR.strings.names_bought_later),
            onConfirm = { ModalManager.start.showModalCloseable { c -> NameRecoveryKeyView(rhId, c) } },
          )
        }
      }
    },
    rescan = {
      scope.launch {
        loading.value = true
        val found = chatModel.controller.apiNameRescan(rhId)
        reload()
        if (found != null) {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.names_rescan_done_title),
            text = if (found == 0) generalGetString(MR.strings.names_rescan_none)
            else generalGetString(MR.strings.names_rescan_found).format(found)
          )
        }
      }
    },
    accept = { item -> confirmAccept(rhId, item) { scope.launch { reload() } } },
    decline = { item -> confirmDecline(rhId, item) { scope.launch { reload() } } },
    close = close,
  )
}

@Composable
private fun IncomingNamesLayout(
  incoming: List<IncomingName>,
  loading: Boolean,
  canReceive: Boolean,
  enableReceiving: () -> Unit,
  rescan: () -> Unit,
  accept: (IncomingName) -> Unit,
  decline: (IncomingName) -> Unit,
  close: () -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_incoming_title))

    if (!canReceive && !loading) {
      // Without a wallet there is nowhere for a name to be sent, so this is the
      // one thing worth offering.
      SectionView {
        SectionItemView { Text(stringResource(MR.strings.names_receive_off), color = MaterialTheme.colors.secondary) }
        SettingsActionItem(
          painterResource(MR.images.ic_check),
          stringResource(MR.strings.names_receive_enable),
          enableReceiving,
          textColor = MaterialTheme.colors.primary,
          iconColor = MaterialTheme.colors.primary,
        )
      }
      SectionTextFooter(stringResource(MR.strings.names_receive_off_footer))
    } else if (incoming.isEmpty()) {
      SectionView {
        SectionItemView {
          Text(
            if (loading) stringResource(MR.strings.names_incoming_loading)
            else stringResource(MR.strings.names_incoming_none),
            color = MaterialTheme.colors.secondary
          )
        }
      }
      SectionTextFooter(stringResource(MR.strings.names_incoming_ready_footer))
    } else {
      SectionView(stringResource(MR.strings.names_incoming_section).uppercase()) {
        incoming.forEach { item ->
          SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)) {
            Column(Modifier.fillMaxWidth()) {
              Text(item.inNames.joinToString(", "), fontWeight = androidx.compose.ui.text.font.FontWeight.Medium)
              Spacer(Modifier.height(DEFAULT_PADDING_HALF))
              Row(horizontalArrangement = Arrangement.spacedBy(DEFAULT_PADDING)) {
                TextButton(onClick = { accept(item) }) {
                  Text(stringResource(MR.strings.names_incoming_accept), color = MaterialTheme.colors.primary)
                }
                TextButton(onClick = { decline(item) }) {
                  Text(stringResource(MR.strings.names_incoming_decline), color = Color.Red)
                }
              }
            }
          }
        }
      }
      // The trade-off, stated before either button rather than after.
      SectionTextFooter(stringResource(MR.strings.names_incoming_footer))
    }

    if (canReceive) {
    SectionDividerSpaced(maxTopPadding = true)
    SectionView {
      SettingsActionItem(
        painterResource(MR.images.ic_search),
        stringResource(MR.strings.names_rescan),
        rescan,
        disabled = loading,
      )
    }
    SectionTextFooter(stringResource(MR.strings.names_rescan_footer))
    }
    SectionBottomSpacer()
  }
}

private fun confirmAccept(rhId: Long?, item: IncomingName, onDone: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.names_accept_title),
    text = generalGetString(MR.strings.names_accept_text).format(item.inNames.joinToString(", ")),
    confirmText = generalGetString(MR.strings.names_incoming_accept),
    onConfirm = {
      withBGApi {
        chatModel.controller.apiNameAccept(rhId, item.inAddress)
        onDone()
      }
    }
  )
}

private fun confirmDecline(rhId: Long?, item: IncomingName, onDone: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.names_decline_title),
    text = generalGetString(MR.strings.names_decline_text),
    confirmText = generalGetString(MR.strings.names_incoming_decline),
    destructive = true,
    onConfirm = {
      withBGApi {
        chatModel.controller.apiNameDecline(rhId, item.inAddress)
        onDone()
      }
    }
  )
}
