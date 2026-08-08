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
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch

// The recovery key for the user's names.
//
// For most users this is the only backup they have of anything: database export
// and Migrate-to-another-device both refuse to run while the app is on its
// initial random passphrase, which is where onboarding leaves everyone. So this
// screen is not a wallet formality - it is frequently the first and only backup
// the app has ever offered, and it is worded accordingly.
//
// The stakes are impersonation, not loss. Whoever holds this key can point a
// name at their own address, so anyone who knows the name would find them
// instead. That is a sharper risk than "you could lose a name" and the copy
// says so.
@Composable
fun NameRecoveryKeyView(rhId: Long?, close: () -> Unit) {
  val phrase = remember { mutableStateOf<String?>(null) }
  val loadFailed = remember { mutableStateOf(false) }
  val saved = remember { mutableStateOf(false) }
  val revealed = remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()

  LaunchedEffect(Unit) {
    val r = chatModel.controller.apiNameRecoveryKey(rhId)
    if (r != null) {
      phrase.value = r.recoveryPhrase
      saved.value = r.recoveryKeySaved
    } else {
      loadFailed.value = true
    }
  }

  NameRecoveryKeyLayout(
    phrase = phrase.value,
    loadFailed = loadFailed.value,
    saved = saved.value,
    revealed = revealed.value,
    reveal = { revealed.value = true },
    markSaved = {
      scope.launch {
        if (chatModel.controller.apiNameRecoveryKeySaved(rhId)) {
          saved.value = true
        }
      }
    },
    close = close,
  )
}

@Composable
private fun NameRecoveryKeyLayout(
  phrase: String?,
  loadFailed: Boolean,
  saved: Boolean,
  revealed: Boolean,
  reveal: () -> Unit,
  markSaved: () -> Unit,
  close: () -> Unit,
) {
  val clipboard = LocalClipboardManager.current
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_recovery_key_title))

    SectionView {
      SectionItemView {
        Text(stringResource(MR.strings.names_recovery_key_what), color = MaterialTheme.colors.secondary)
      }
    }

    SectionDividerSpaced(maxTopPadding = true)

    if (loadFailed) {
      SectionView {
        SectionItemView { Text(stringResource(MR.strings.names_list_load_failed), color = WarningOrange) }
      }
    } else if (phrase == null) {
      SectionView {
        SectionItemView { Text(stringResource(MR.strings.names_recovery_key_none), color = MaterialTheme.colors.secondary) }
      }
      SectionTextFooter(stringResource(MR.strings.names_recovery_key_none_footer))
    } else if (!revealed) {
      // Not shown until asked for: this screen can be opened with someone
      // looking over your shoulder.
      SectionView {
        SettingsActionItem(
          painterResource(MR.images.ic_visibility),
          stringResource(MR.strings.names_recovery_key_reveal),
          reveal,
          textColor = MaterialTheme.colors.primary,
          iconColor = MaterialTheme.colors.primary,
        )
      }
      SectionTextFooter(stringResource(MR.strings.names_recovery_key_reveal_footer))
    } else {
      SectionView(stringResource(MR.strings.names_recovery_key_section).uppercase()) {
        SectionItemView(padding = PaddingValues(DEFAULT_PADDING)) {
          Text(
            phrase,
            fontFamily = FontFamily.Monospace,
            fontWeight = FontWeight.Medium,
          )
        }
        SectionItemView(click = {
          clipboard.setText(AnnotatedString(phrase))
          showToast(generalGetString(MR.strings.copied))
        }) {
          Text(stringResource(MR.strings.names_recovery_key_copy), color = MaterialTheme.colors.primary)
        }
      }
      SectionTextFooter(stringResource(MR.strings.names_recovery_key_write_down))

      SectionDividerSpaced(maxTopPadding = true)
      SectionView {
        if (saved) {
          SectionItemView {
            Text(stringResource(MR.strings.names_recovery_key_already_saved), color = MaterialTheme.colors.secondary)
          }
        } else {
          SettingsActionItem(
            painterResource(MR.images.ic_check),
            stringResource(MR.strings.names_recovery_key_mark_saved),
            markSaved,
            textColor = MaterialTheme.colors.primary,
            iconColor = MaterialTheme.colors.primary,
          )
        }
      }
    }

    SectionDividerSpaced(maxTopPadding = true)
    SectionView(stringResource(MR.strings.names_recovery_key_risk_section).uppercase()) {
      SectionItemView {
        Text(stringResource(MR.strings.names_recovery_key_risk), color = Color.Red)
      }
    }
    SectionTextFooter(stringResource(MR.strings.names_recovery_key_passphrase_hint))
    SectionBottomSpacer()
  }
}
