package chat.simplex.common.views.helpers

import chat.simplex.common.model.AgentErrorType
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.views.database.restartChatOrApp
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
          val title = generalGetString(MR.strings.agent_critical_error_title)
          val text = generalGetString(MR.strings.agent_critical_error_desc).format(error.criticalErr)
          try {
            ntfManager.showMessage(title, text)
          } catch (e: Throwable) {
            Log.e(TAG, e.stackTraceToString())
          }
          if (offerRestart) {
            AlertManager.shared.showAlertDialog(
              title = title,
              text = text,
              confirmText = generalGetString(MR.strings.restart_chat_button),
              onConfirm = {
                withApi { restartChatOrApp() }
              })
          } else {
            AlertManager.shared.showAlertMsg(
              title = title,
              text = text,
            )
          }
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
