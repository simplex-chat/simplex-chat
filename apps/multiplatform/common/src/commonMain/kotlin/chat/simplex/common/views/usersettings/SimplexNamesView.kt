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
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontFamily
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
  val address = remember { mutableStateOf<CR.NameAddress?>(null) }
  val keySaved = remember { mutableStateOf(true) }

  LaunchedEffect(Unit) {
    names.value = chatModel.controller.apiNameList(rhId)
    incomingCount.value = chatModel.controller.apiNameIncoming(rhId)?.size ?: 0
    address.value = chatModel.controller.apiNameAddress(rhId)
    keySaved.value = chatModel.controller.apiNameRecoveryKey(rhId)?.recoveryKeySaved ?: true
  }

  SimplexNamesLayout(
    names = names.value,
    incomingCount = incomingCount.value,
    address = address.value,
    keySaved = keySaved.value,
    rhId = rhId,
  )
}

@Composable
private fun SimplexNamesLayout(
  names: List<String>?,
  incomingCount: Int,
  address: CR.NameAddress?,
  keySaved: Boolean,
  rhId: Long?,
) {
  val clipboard = LocalClipboardManager.current
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_title))

    SectionView(stringResource(MR.strings.names_your_names).uppercase()) {
      when {
        names == null -> SectionItemView { Text(stringResource(MR.strings.names_incoming_loading), color = MaterialTheme.colors.secondary) }
        names.isEmpty() -> SectionItemView { Text(stringResource(MR.strings.names_none_yet), color = MaterialTheme.colors.secondary) }
        else -> names.forEach { n -> SectionItemView { Text(n) } }
      }
    }
    SectionTextFooter(stringResource(MR.strings.names_your_names_footer))

    SectionDividerSpaced(maxTopPadding = true)
    SectionView {
      SettingsActionItemWithContent(
        painterResource(MR.images.ic_mail),
        stringResource(MR.strings.names_incoming_title),
        click = { ModalManager.start.showModalCloseable { close -> IncomingNamesView(rhId, close) } },
      ) {
        if (incomingCount > 0) {
          Text(incomingCount.toString(), color = MaterialTheme.colors.primary)
        }
      }
      SettingsActionItem(
        painterResource(MR.images.ic_lock),
        stringResource(MR.strings.names_recovery_key_title),
        click = { ModalManager.start.showModalCloseable { close -> NameRecoveryKeyView(rhId, close) } },
        // Unacknowledged recovery key is the one thing on this screen that can
        // cost the user everything, so it is coloured until they confirm it.
        textColor = if (keySaved) Color.Unspecified else WarningOrange,
        iconColor = if (keySaved) MaterialTheme.colors.secondary else WarningOrange,
      )
    }

    if (address != null) {
      SectionDividerSpaced(maxTopPadding = true)
      SectionView(stringResource(MR.strings.names_meta_address_section).uppercase()) {
        SectionItemView(padding = PaddingValues(DEFAULT_PADDING)) {
          Text(address.nameMetaAddress, fontFamily = FontFamily.Monospace, maxLines = 3)
        }
        SectionItemView(click = {
          clipboard.setText(AnnotatedString(address.nameMetaAddress))
          showToast(generalGetString(MR.strings.copied))
        }) {
          Text(stringResource(MR.strings.names_meta_address_copy), color = MaterialTheme.colors.primary)
        }
      }
      SectionTextFooter(stringResource(MR.strings.names_meta_address_footer))
    }

    SectionBottomSpacer()
  }
}
