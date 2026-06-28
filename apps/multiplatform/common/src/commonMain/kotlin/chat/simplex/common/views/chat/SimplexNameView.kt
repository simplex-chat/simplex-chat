package chat.simplex.common.views.chat

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
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
  name: SimplexNameInfo,
  verification: Boolean?,
  autoVerify: Boolean,
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
    if (autoVerify && verification == null) runVerify(manual = false)
  }

  Row(
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(6.dp),
    modifier = Modifier.padding(top = DEFAULT_PADDING_HALF)
  ) {
    Text(
      name.shortName,
      style = MaterialTheme.typography.body2.copy(
        color = if (verification == true) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
        fontFamily = if (verification == true) FontFamily.Default else FontFamily.Monospace
      )
    )
    when {
      showSpinner.value ->
        CircularProgressIndicator(Modifier.size(16.dp), strokeWidth = 2.dp, color = MaterialTheme.colors.secondary)
      verification == true ->
        Icon(painterResource(MR.images.ic_check_filled), null, Modifier.size(18.dp), tint = MaterialTheme.colors.onBackground)
      verification == false ->
        Icon(
          painterResource(MR.images.ic_close), null, tint = Color.Red,
          modifier = Modifier.size(18.dp).clickable { runVerify(manual = true) }
        )
      else ->
        Text(
          stringResource(MR.strings.verify_simplex_name_action),
          color = MaterialTheme.colors.primary,
          modifier = Modifier.clickable { runVerify(manual = true) }
        )
    }
  }
}
