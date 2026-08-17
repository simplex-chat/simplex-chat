package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.material.*
import androidx.compose.runtime.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch

// How this profile gets its recovery key, asked once, the first time it needs
// one.
//
// Sharing the key across profiles is the default because it means one thing to
// write down. A separate key exists for a profile kept deliberately apart: its
// names are then not derivable from the others. Importing is here rather than
// hidden elsewhere because restoring is the same decision, made differently.
@Composable
fun WalletSetupView(rhId: Long?, hasOtherSeed: Boolean, onDone: () -> Unit) {
  val busy = remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()

  fun setup(arg: String) {
    scope.launch {
      busy.value = true
      try {
        if (chatModel.controller.apiNameSetup(rhId, arg)) onDone()
      } finally { busy.value = false }
    }
  }

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_setup_title))
    SectionTextFooter(stringResource(MR.strings.names_setup_intro))

    SectionView {
      if (hasOtherSeed) {
        SettingsActionItem(
          painterResource(MR.images.ic_check),
          stringResource(MR.strings.names_setup_existing),
          { setup("existing") },
          textColor = MaterialTheme.colors.primary,
          iconColor = MaterialTheme.colors.primary,
          disabled = busy.value,
        )
      }
      SettingsActionItem(
        painterResource(MR.images.ic_add),
        stringResource(if (hasOtherSeed) MR.strings.names_setup_new else MR.strings.names_setup_first),
        { setup("new") },
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
        disabled = busy.value,
      )
      SettingsActionItem(
        painterResource(MR.images.ic_download),
        stringResource(MR.strings.names_import_title),
        { ModalManager.start.showModalCloseable { c -> ImportRecoveryKeyView(rhId, c) } },
        disabled = busy.value,
      )
    }
    SectionTextFooter(
      stringResource(if (hasOtherSeed) MR.strings.names_setup_footer_shared else MR.strings.names_setup_footer_first)
    )
    SectionBottomSpacer()
  }
}
