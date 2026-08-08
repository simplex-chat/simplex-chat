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
  val names = remember { mutableStateOf<List<String>?>(null) }
  val incomingCount = remember { mutableStateOf(0) }
  val keySaved = remember { mutableStateOf(true) }
  val hasKey = remember { mutableStateOf(false) }

  LaunchedEffect(Unit) {
    names.value = chatModel.controller.apiNameList(rhId)
    incomingCount.value = chatModel.controller.apiNameIncoming(rhId)?.size ?: 0
    // Deliberately not asking for the receiving address here: that would create
    // the wallet and tell every contact about it, for someone who only opened a
    // settings screen. It comes into existence on the first purchase.
    val st = chatModel.controller.apiNameStatus(rhId)
    hasKey.value = st?.nameHasWallet ?: false
    keySaved.value = st?.nameKeySaved ?: true
  }

  // Development overlay: names owned through the wallet sit alongside a name
  // claimed on this profile the legacy way (registered externally, via the
  // dApp). Resolution and display are shared; ownership is not, so the two are
  // listed separately and wallet actions are offered only where they apply.
  val claimed = chatModel.currentUser.value?.profile?.contactDomain?.domain

  SimplexNamesLayout(
    names = names.value,
    claimed = claimed,
    incomingCount = incomingCount.value,
    keySaved = keySaved.value,
    hasKey = hasKey.value,
    rhId = rhId,
  )
}

@Composable
private fun SimplexNamesLayout(
  names: List<String>?,
  claimed: String?,
  incomingCount: Int,
  keySaved: Boolean,
  hasKey: Boolean,
  rhId: Long?,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_title))

    SectionView(stringResource(MR.strings.names_your_names).uppercase()) {
      when {
        names == null -> SectionItemView { Text(stringResource(MR.strings.names_incoming_loading), color = MaterialTheme.colors.secondary) }
        names.isEmpty() -> SectionItemView { Text(stringResource(MR.strings.names_none_yet), color = MaterialTheme.colors.secondary) }
        else -> names.forEach { n ->
          SectionItemView(click = { ModalManager.start.showModalCloseable { c -> NameDetailView(rhId, n, c) } }) { Text(n) }
        }
      }
    }
    SectionTextFooter(stringResource(MR.strings.names_your_names_footer))

    if (claimed != null && names?.contains(claimed) != true) {
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
