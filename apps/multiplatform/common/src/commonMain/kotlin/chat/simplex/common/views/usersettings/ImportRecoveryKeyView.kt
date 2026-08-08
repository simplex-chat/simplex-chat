package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch

// Restoring names from a recovery key.
//
// This is the path that makes the whole design true: a name survives losing the
// device, because the key that owns it comes back from twelve words. Importing
// binds this profile to that key, then a scan finds what it already owns —
// buying nothing and asking no one.
@Composable
fun ImportRecoveryKeyView(rhId: Long?, close: () -> Unit) {
  val phrase = rememberSaveable { mutableStateOf("") }
  val working = remember { mutableStateOf(false) }
  val hasWallet = remember { mutableStateOf<Boolean?>(null) }
  val scope = rememberCoroutineScope()

  LaunchedEffect(Unit) {
    hasWallet.value = chatModel.controller.apiNameStatus(rhId)?.nameHasWallet ?: false
  }

  fun doImport() {
    scope.launch {
      working.value = true
      try {
        if (!chatModel.controller.apiNameRecoveryKeyImport(rhId, phrase.value.trim())) return@launch
        // The key says which names are ours; the chain says which exist. Only
        // a scan joins the two, so it runs here rather than making the user
        // discover an empty list and go looking for a button.
        val found = chatModel.controller.apiNameRescan(rhId) ?: 0
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.names_import_done_title),
          text = if (found == 0) generalGetString(MR.strings.names_import_done_none)
          else generalGetString(MR.strings.names_import_done_found).format(found),
        )
        close()
      } finally {
        working.value = false
      }
    }
  }

  val words = phrase.value.trim().split(Regex("\\s+")).filter { it.isNotEmpty() }
  val looksComplete = words.size == 12 || words.size == 15 || words.size == 18 || words.size == 21 || words.size == 24

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_import_title))

    if (hasWallet.value == true) {
      // The core refuses to import over an existing key, so say so plainly
      // rather than warn about a replacement that will not happen.
      SectionView {
        SectionItemView { Text(stringResource(MR.strings.names_import_already), color = MaterialTheme.colors.secondary) }
      }
      SectionBottomSpacer()
      return@ColumnWithScrollBar
    }

    SectionTextFooter(stringResource(MR.strings.names_import_intro))

    SectionView(stringResource(MR.strings.names_import_section).uppercase()) {
      SectionItemView(padding = PaddingValues(DEFAULT_PADDING)) {
        TextField(
          value = phrase.value,
          onValueChange = { phrase.value = it },
          placeholder = { Text(stringResource(MR.strings.names_import_placeholder)) },
          modifier = Modifier.fillMaxWidth(),
          enabled = !working.value,
        )
      }
    }
    SectionTextFooter(
      if (phrase.value.isBlank()) stringResource(MR.strings.names_import_hint)
      else if (words.size == 1) stringResource(MR.strings.names_import_word_count_one)
      else stringResource(MR.strings.names_import_word_count).format(words.size),
      if (phrase.value.isBlank() || looksComplete) MaterialTheme.colors.secondary else WarningOrange
    )

    SectionDividerSpaced(maxTopPadding = true)
    SectionView {
      SectionItemView(
        click = if (working.value || !looksComplete) null else { { doImport() } },
        disabled = working.value || !looksComplete,
      ) {
        Text(
          if (working.value) stringResource(MR.strings.names_import_working) else stringResource(MR.strings.names_import_action),
          color = if (working.value || !looksComplete) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
        )
      }
    }
    SectionBottomSpacer()
  }
}


