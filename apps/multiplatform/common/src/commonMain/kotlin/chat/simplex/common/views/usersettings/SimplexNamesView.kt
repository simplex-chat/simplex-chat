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
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

// Entry point for everything about this profile's SimpleX names.
//
// Names bought by this profile, names other people have sent to it, the
// meta-address that lets contacts send without a handshake, and the recovery
// key. The last one carries a badge until it is acknowledged, because for most
// users it is the only backup they have.
@Composable
fun SimplexNamesView(rhId: Long?, close: () -> Unit) {
  val names = remember { mutableStateOf<List<OwnedName>?>(null) }
  val incomingCount = remember { mutableStateOf(0) }
  val keySaved = remember { mutableStateOf(true) }
  val hasKey = remember { mutableStateOf(false) }
  val loadFailed = remember { mutableStateOf(false) }

  LaunchedEffect(Unit) {
    // Status first, and nothing else until it says a wallet exists. Every other
    // name call goes through userWalletAccount, which creates the seed as a
    // side effect - so asking for the list first would give a wallet to anyone
    // who merely opened this screen.
    val st = chatModel.controller.apiNameStatus(rhId)
    // A failed read is not the same as "you have nothing": saying so would hide
    // the unsaved-recovery-key warning from someone who has names.
    loadFailed.value = st == null
    hasKey.value = st?.nameHasWallet ?: false
    keySaved.value = st?.nameKeySaved ?: true
    if (hasKey.value) {
      names.value = chatModel.controller.apiNameList(rhId)
      incomingCount.value = chatModel.controller.apiNameIncoming(rhId)?.size ?: 0
      chatModel.namesWaiting.value = incomingCount.value
    } else {
      names.value = emptyList()
    }
  }

  // Development overlay: a name claimed on this profile the legacy way, i.e.
  // registered elsewhere. Shown for reference; wallet actions do not apply.
  val claimed = chatModel.currentUser.value?.profile?.contactDomain?.domain

  SimplexNamesLayout(
    names = names.value,
    claimed = claimed,
    incomingCount = incomingCount.value,
    keySaved = keySaved.value,
    hasKey = hasKey.value,
    loadFailed = loadFailed.value,
    rhId = rhId,
  )
}

@Composable
private fun SimplexNamesLayout(
  names: List<OwnedName>?,
  claimed: String?,
  incomingCount: Int,
  keySaved: Boolean,
  hasKey: Boolean,
  loadFailed: Boolean,
  rhId: Long?,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_title))

    SectionView(stringResource(MR.strings.names_your_names).uppercase()) {
      when {
        loadFailed ->
          SectionItemView { Text(stringResource(MR.strings.names_list_load_failed), color = WarningOrange) }
        names == null -> SectionItemView { Text(stringResource(MR.strings.names_incoming_loading), color = MaterialTheme.colors.secondary) }
        names.isEmpty() -> SectionItemView { Text(stringResource(MR.strings.names_none_yet), color = MaterialTheme.colors.secondary) }
        else -> names.forEach { n ->
          val expired = n.onExpires.toLong() - System.currentTimeMillis() / 1000 < 0
          SectionItemView(click = { ModalManager.start.showModalCloseable { c -> NameDetailView(rhId, n.onFqdn, c) } }) {
            Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
              // Expired names stay listed - they are still recoverable until
              // someone else takes them - but read as inactive.
              Text(n.onFqdn, color = if (expired) MaterialTheme.colors.secondary else Color.Unspecified)
              if (expired) Text(stringResource(MR.strings.names_list_expired), color = MaterialTheme.colors.secondary)
            }
          }
        }
      }
    }
    SectionTextFooter(stringResource(MR.strings.names_your_names_footer))

    if (claimed != null && names?.any { it.onFqdn == claimed } != true) {
      SectionDividerSpaced(maxTopPadding = true)
      SectionView(stringResource(MR.strings.names_legacy_section).uppercase()) {
        SectionItemView(click = {
          AlertManager.shared.showAlertMsg(
            title = claimed,
            text = generalGetString(MR.strings.names_legacy_explain),
          )
        }) {
          Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
            Text(claimed)
            Text(stringResource(MR.strings.names_legacy_tag), color = MaterialTheme.colors.secondary)
          }
        }
      }
      SectionTextFooter(stringResource(MR.strings.names_legacy_footer))
    }

    SectionDividerSpaced(maxTopPadding = true)
    SectionView {
      SettingsActionItem(
        painterResource(MR.images.ic_add),
        stringResource(MR.strings.names_buy_title),
        click = { ModalManager.start.showModalCloseable { close -> BuyNameView(rhId, close) } },
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
      )
      SettingsActionItemWithContent(
        painterResource(MR.images.ic_mail),
        stringResource(MR.strings.names_incoming_title),
        click = { ModalManager.start.showModalCloseable { close -> IncomingNamesView(rhId, close) } },
      ) {
        if (incomingCount > 0) {
          Text(incomingCount.toString(), color = MaterialTheme.colors.primary)
        }
      }
      if (!hasKey) SettingsActionItem(
        painterResource(MR.images.ic_add),
        stringResource(MR.strings.names_setup_title),
        click = { ModalManager.start.showModalCloseable { c -> WalletSetupView(rhId, hasOtherSeed = false, onDone = c) } },
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
      )
      SettingsActionItem(
        painterResource(MR.images.ic_download),
        stringResource(MR.strings.names_import_title),
        click = { ModalManager.start.showModalCloseable { c -> ImportRecoveryKeyView(rhId, c) } },
      )
      if (hasKey) SettingsActionItem(
        painterResource(MR.images.ic_lock),
        stringResource(MR.strings.names_recovery_key_title),
        click = { ModalManager.start.showModalCloseable { close -> NameRecoveryKeyView(rhId, close) } },
        // Unacknowledged recovery key is the one thing on this screen that can
        // cost the user everything, so it is coloured until they confirm it.
        textColor = if (keySaved) Color.Unspecified else WarningOrange,
        iconColor = if (keySaved) MaterialTheme.colors.secondary else WarningOrange,
      )
    }

    SectionBottomSpacer()
  }
}
