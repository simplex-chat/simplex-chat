package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.AnnotatedString
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.*
import chat.simplex.common.views.chat.item.openBrowserAlert
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*

// Each dot-separated label is ASCII letters/digits with single internal hyphens (mirrors simplexmq SimplexName.hs nameLabelP).
private val simplexNameLabelRegex = Regex("[A-Za-z0-9]+(-[A-Za-z0-9]+)*")

// Set the user's own (prefix "@") or a channel's (prefix "#") SimpleX name.
// The field is prefilled with the full prefixed name; `save` receives the encoded name (or null to
// clear) and returns true on success (it shows its own error alert otherwise).
// `registerBackgroundClose` is set by the contact/start-panel call site so a desktop background click
// routes through the save-on-close prompt; the channel call site (opened via ModalManager.end) leaves it false.
@Composable
fun SetSimplexDomainView(
  title: String,
  footer: String,
  placeholder: String,
  simplexName: String,
  registerBackgroundClose: Boolean = false,
  save: suspend (String?) -> Boolean,
  close: () -> Unit
) {
  val name = rememberSaveable { mutableStateOf(simplexName) }
  val saving = remember { mutableStateOf(false) }
  val editing = rememberSaveable { mutableStateOf(simplexName.isBlank()) }
  val uriHandler = LocalUriHandler.current
  val clipboard = LocalClipboardManager.current

  fun addSimplexTLD(s: String): String {
    return if (s.contains(".")) s else "$s.simplex"
  }

  fun normalized(s: String): String? {
    val t = s.trim()
    return when {
      t.isEmpty() -> null
      t.startsWith("@") || t.startsWith("#") -> addSimplexTLD(t.substring(1))
      else -> addSimplexTLD(t)
    }
  }

  // An empty field is valid (it means "remove the name"). Otherwise check the SimpleX-name grammar on
  // the normalized value; A-Z is accepted because the core lowercases the name on accept.
  fun isValidName(s: String): Boolean {
    val n = normalized(s) ?: return true
    if (n.length > 253) return false
    val labels = n.split(".")
    if (labels.size < 2) return false
    return labels.all { it.length in 1..63 && simplexNameLabelRegex.matches(it) }
  }

  val unchanged = normalized(name.value) == normalized(simplexName)
  val isValid = isValidName(name.value)

  fun doSave(close: () -> Unit) {
    withBGApi {
      saving.value = true
      val ok = try { save(normalized(name.value)) } catch (e: Exception) {
        Log.e(TAG, "SetSimplexDomainView save: ${e.stackTraceToString()}")
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_saving_simplex_name), e.message ?: "")
        false
      }
      saving.value = false
      if (ok) withContext(Dispatchers.Main) {
        chatModel.centerPanelBackgroundClickHandler = null
        close()
      }
    }
  }

  // Reads name.value live so it stays correct when invoked from the background-click handler registered once.
  // Returns true when it consumes the close (shows the prompt), false when it lets the close proceed.
  fun onClose(close: () -> Unit): Boolean {
    val valid = isValidName(name.value)
    val changed = normalized(name.value) != normalized(simplexName)
    return if (changed && valid) {
      AlertManager.shared.showAlertDialog(
        title = generalGetString(MR.strings.save_simplex_name_question),
        confirmText = generalGetString(MR.strings.save_verb),
        onConfirm = { doSave(close) },
        dismissText = generalGetString(MR.strings.exit_without_saving),
        onDismiss = {
          chatModel.centerPanelBackgroundClickHandler = null
          close()
        }
      )
      true
    } else {
      chatModel.centerPanelBackgroundClickHandler = null
      close()
      false
    }
  }

  DisposableEffect(Unit) {
    if (registerBackgroundClose) {
      chatModel.centerPanelBackgroundClickHandler = {
        onClose(close = { ModalManager.start.closeModals() })
      }
    }
    onDispose {
      chatModel.centerPanelBackgroundClickHandler = null
    }
  }

  ModalView(close = { onClose(close) }, cardScreen = true) {
    ColumnWithScrollBar {
      AppBarTitle(title)
      SectionView {
        if (editing.value) {
          TextEditor(
            name,
            Modifier,
            placeholder = placeholder,
            isValid = { isValidName(it) }
          )
        } else {
          SectionItemViewSpaceBetween(click = {
            clipboard.setText(AnnotatedString(name.value))
            showToast(generalGetString(MR.strings.copied))
          }) {
            Text(name.value)
            Icon(painterResource(MR.images.ic_content_copy), stringResource(MR.strings.copy_verb), tint = MaterialTheme.colors.secondary)
          }
        }
      }
      SectionTextFooter(footer)
      SectionDividerSpaced()
      SectionView {
        if (editing.value) {
          if (name.value.isBlank()) {
            SectionItemView({ openBrowserAlert("https://github.com/simplex-chat/simplex-chat/blob/master/docs/guide/register-simplex-name.md", uriHandler) }) {
              Text(stringResource(MR.strings.register_test_name), color = MaterialTheme.colors.primary)
            }
          }
          SectionItemView({ doSave(close) }, disabled = unchanged || saving.value || !isValid) {
            Text(
              stringResource(MR.strings.save_verb),
              color = if (unchanged || saving.value || !isValid) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
            )
          }
        } else {
          SectionItemView({ name.value = ""; editing.value = true }) {
            Text(stringResource(MR.strings.remove_name), color = MaterialTheme.colors.primary)
          }
        }
      }
      SectionBottomSpacer()
    }
  }
}
