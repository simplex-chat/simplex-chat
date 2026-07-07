package chat.simplex.common.views.chat

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.ImageResource
import chat.simplex.common.model.SimplexNameInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*

// Renders a contact's / channel's SimpleX name with its 3-state verification indicator.
// `verification`: null = not attempted, false = failed, true = verified.
// `verify` runs the verify API, updates the model and returns (newVerification, failureReason);
// null on network error. With `autoVerify`, it runs once on open when state is null.
@Composable
fun SimplexNameView(
  simplexName: String,
  verified: Boolean?,
  verify: suspend () -> Pair<Boolean?, String?>?
) {
  val scope = rememberCoroutineScope()
  val inFlight = remember { mutableStateOf(false) }
  val showSpinner = remember { mutableStateOf(false) }

  fun runVerify(manual: Boolean) {
    if (inFlight.value) return
    inFlight.value = true
    scope.launch {
      // delay the spinner so a fast result on open doesn't flash it
      val spinner = launch { delay(300); if (inFlight.value) showSpinner.value = true }
      val res = try {
        verify()
      } catch (e: Exception) {
        Log.e(TAG, "verify SimplexName: ${e.stackTraceToString()}")
        null
      }
      spinner.cancel()
      inFlight.value = false
      showSpinner.value = false
      if (res != null) {
        val (newV, reason) = res
        // show the reason on a manual run, or on an inconclusive auto run (state stayed null)
        if (reason != null && (manual || newV == null)) {
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.simplex_name_not_verified), reason)
        }
      }
    }
  }

  LaunchedEffect(Unit) {
    if (chatModel.controller.appPrefs.privacyVerifySimplexNames.get() && verified == null) runVerify(manual = false)
  }

  val clipboard = LocalClipboardManager.current
  val nameStyle = MaterialTheme.typography.body2.copy(
    color = if (verified == true) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
  )
  Row(
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(6.dp),
    modifier = Modifier.padding(top = DEFAULT_PADDING_HALF)
  ) {
    when {
      showSpinner.value -> {
        Text(simplexName, style = nameStyle)
        CircularProgressIndicator(Modifier.size(16.dp), strokeWidth = 2.dp, color = MaterialTheme.colors.secondary)
      }
      verified == true ->
        SimplexNameWithIcon(simplexName, nameStyle, MR.images.ic_check_filled, MaterialTheme.colors.primary) {
          clipboard.setText(AnnotatedString(simplexName))
        }
      verified == false ->
        SimplexNameWithIcon(simplexName, nameStyle, MR.images.ic_close, Color.Red) { runVerify(manual = true) }
      else -> {
        Text(simplexName, style = nameStyle)
        Text(
          stringResource(MR.strings.verify_simplex_name_action),
          color = MaterialTheme.colors.primary,
          modifier = Modifier.clickable { runVerify(manual = true) }
        )
      }
    }
  }
}

// The check/cross drawable is centered in its box with ~27% padding top and bottom, so its glyph bottom sits at
// ~73% of the box height. Align that line with the text baseline so the glyph rests on the baseline; the box is
// sized so the visible glyph is about the name's cap height. Only the icon is tinted, never the name.
@Composable
private fun SimplexNameWithIcon(name: String, style: TextStyle, icon: ImageResource, tint: Color, onClick: () -> Unit) {
  Row(
    horizontalArrangement = Arrangement.spacedBy(2.dp),
    modifier = Modifier.clickable { onClick() }
  ) {
    Text(name, Modifier.alignByBaseline(), style = style)
    Icon(
      painterResource(icon), null,
      Modifier.size(22.dp).alignBy { it.measuredHeight * 73 / 100 },
      tint = tint
    )
  }
}
