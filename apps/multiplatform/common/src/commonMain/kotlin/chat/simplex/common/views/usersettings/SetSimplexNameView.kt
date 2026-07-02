package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import chat.simplex.common.platform.*
import chat.simplex.common.views.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*

// Set the user's own (prefix "@") or a channel's (prefix "#") SimpleX name.
// The field is prefilled with the full prefixed name; `save` receives the encoded name (or null to
// clear) and returns true on success (it shows its own error alert otherwise).
@Composable
fun SetSimplexDomainView(
  title: String,
  footer: String,
  placeholder: String,
  simplexName: String,
  save: suspend (String?) -> Boolean,
  close: () -> Unit
) {
  val name = rememberSaveable { mutableStateOf(simplexName) }
  val saving = remember { mutableStateOf(false) }
  val unchanged = name.value.trim() == simplexName.trim()

  fun addSimplexTLD(s: String): String {
    return if (s.contains(".")) s else "$s.simplex"
  }

  fun normalized(): String? {
    val s = name.value.trim()
    return when {
      s.isEmpty() -> null
      s.startsWith("@") || s.startsWith("#") -> addSimplexTLD(s.substring(1))
      else -> addSimplexTLD(s)
    }
  }

  val doSave: () -> Unit = {
    withBGApi {
      saving.value = true
      val ok = try { save(normalized()) } catch (e: Exception) {
        Log.e(TAG, "SetSimplexDomainView save: ${e.stackTraceToString()}")
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_saving_simplex_name), e.message ?: "")
        false
      }
      saving.value = false
      if (ok) withContext(Dispatchers.Main) { close() }
    }
  }

  ModalView(close = close) {
    ColumnWithScrollBar {
      AppBarTitle(title)
      SectionView {
        PlainTextEditor(name, placeholder)
      }
      SectionTextFooter(footer)
      SectionDividerSpaced()
      SectionView {
        SectionItemView(doSave, disabled = unchanged || saving.value) {
          Text(
            stringResource(MR.strings.save_verb),
            color = if (unchanged || saving.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
          )
        }
      }
      SectionBottomSpacer()
    }
  }
}
