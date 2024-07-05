package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Row
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.chatModel
import chat.simplex.common.model.OnionHosts
import chat.simplex.common.model.PresentedServersSummary
import chat.simplex.common.model.SMPServerSubs
import chat.simplex.common.model.ServerSessions
import kotlinx.coroutines.Job
import kotlinx.coroutines.cancel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlin.math.floor
import kotlin.math.roundToInt
import kotlin.time.Duration
import kotlin.time.Duration.Companion.seconds

data class SubscriptionStatus(
  val color: Color,
  val variableValue: Float,
  val opacity: Float,
  val statusPercent: Float
)

fun subscriptionStatusColorAndPercentage(
  online: Boolean,
  onionHosts: OnionHosts,
  subs: SMPServerSubs,
  sess: ServerSessions
): SubscriptionStatus {

  fun roundedToQuarter(n: Float): Float {
    return when {
      n >= 1 -> 1f
      n <= 0 -> 0f
      else -> (n * 4).roundToInt() / 4f
    }
  }

  val activeColor: Color = if (onionHosts == OnionHosts.REQUIRED) Color(0xFF4B0082) else Color(0xFF00796B)
  val noConnColorAndPercent = SubscriptionStatus(Color(0xFFB0BEC5), 1f, 1f, 0f)
  val activeSubsRounded = roundedToQuarter(subs.shareOfActive)

  return if (online && subs.total > 0) {
    if (subs.ssActive == 0) {
      if (sess.ssConnected == 0) noConnColorAndPercent else SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    } else { // ssActive > 0
      if (sess.ssConnected == 0) SubscriptionStatus(Color(0xFFFFA500), activeSubsRounded, subs.shareOfActive, subs.shareOfActive) // This would mean implementation error
      else SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    }
  } else noConnColorAndPercent
}


@Composable
fun SubscriptionStatusIndicatorView(subs: SMPServerSubs, sess: ServerSessions) {
  val netCfg = remember { chatModel.controller.getNetCfg() }
  val onionHosts = remember { netCfg.onionHosts }
  val statusColorAndPercentage = subscriptionStatusColorAndPercentage(chatModel.networkInfo.value.online, onionHosts, subs, sess)
  val pref = remember { chatModel.controller.appPrefs.networkShowSubscriptionPercentage }

  Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
    SubscriptionStatusIcon(color = MaterialTheme.colors.primary, variableValue = statusColorAndPercentage.variableValue)
    if (pref.state.value) {
      Text("${(floor(statusColorAndPercentage.statusPercent * 100)).toInt()}%", color = MaterialTheme.colors.secondary)
    }
  }
}

@Composable
fun SubscriptionStatusIndicator() {
  var subs by remember { mutableStateOf(SMPServerSubs.newSMPServerSubs) }
  var sess by remember { mutableStateOf(ServerSessions.newServerSessions) }
  var timerCounter by remember { mutableStateOf(0) }
  var timer: Job? by remember { mutableStateOf(null) }

  val initialInterval: Duration = 1.seconds
  val regularInterval: Duration = 3.seconds
  val initialPhaseDuration: Duration = 10.seconds

  val scope = rememberCoroutineScope()

  fun setServersSummary() {
    withBGApi {
      val summary: PresentedServersSummary? = chatModel.controller.getAgentServersSummary(chatModel.remoteHostId())

      if (summary != null) {
        subs = summary.allUsersSMP.smpTotals.subs
        sess = summary.allUsersSMP.smpTotals.sessions
      }
    }
  }

  fun stopTimer() {
    timer?.cancel()
    timer = null
  }

  fun switchToRegularTimer() {
    stopTimer()
    timer = timer ?: scope.launch {
      while (true) {
        delay(regularInterval.inWholeMilliseconds)
        setServersSummary()
      }
    }
  }

  fun startInitialTimer() {
    timer = timer ?: scope.launch {
      while (true) {
        delay(initialInterval.inWholeMilliseconds)
        setServersSummary()
        timerCounter++
        if (timerCounter *  initialInterval.inWholeSeconds >= initialPhaseDuration.inWholeSeconds) {
          switchToRegularTimer()
        }
      }
    }
  }

  DisposableEffect(Unit) {
    onDispose {
      stopTimer()
      scope.cancel()
    }
  }

  LaunchedEffect(Unit) {
    startInitialTimer()
  }

  Row {
    SubscriptionStatusIndicatorView(subs = subs, sess = sess)
  }
}
