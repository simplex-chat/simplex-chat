package chat.simplex.common.views.helpers

import chat.simplex.common.model.AgentErrorType
import chat.simplex.common.views.database.restartChatAlert
import chat.simplex.res.MR
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlin.math.max

class ProcessedErrors <T: AgentErrorType>(val interval: Long) {
  private var lastShownTimestamp: Long = System.currentTimeMillis() - interval
  private var lastShownOfferRestart: Boolean = false
  private var timer: Job = Job()

  fun newError(error: T, offerRestart: Boolean) {
    timer.cancel()
    val job = withBGApi {
      if (lastShownOfferRestart || !offerRestart) {
        delay(max((lastShownTimestamp + interval) - System.currentTimeMillis(), 0))
      }
      lastShownTimestamp = System.currentTimeMillis()
      lastShownOfferRestart = offerRestart
      AlertManager.shared.hideAllAlerts()
      when (error) {
        is AgentErrorType.CRITICAL -> {
          AlertManager.shared.showAlertDialog(
            title = generalGetString(MR.strings.agent_critical_error_title),
            text = generalGetString(MR.strings.agent_critical_error_desc).format(error.criticalErr),
            confirmText = if (offerRestart) generalGetString(MR.strings.restart_chat_button) else generalGetString(MR.strings.ok),
            onConfirm = {
              if (offerRestart) {
                withApi { restartChatAlert() }
              }
            }
          )
        }
        is AgentErrorType.INTERNAL -> {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.agent_internal_error_title),
            text = generalGetString(MR.strings.agent_internal_error_desc).format(error.internalErr),
          )
        }
      }
    }
    timer = job
  }
}
