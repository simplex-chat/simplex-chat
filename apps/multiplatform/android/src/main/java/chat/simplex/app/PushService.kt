package chat.simplex.app

import chat.simplex.common.model.CC
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import org.unifiedpush.android.connector.FailedReason
import org.unifiedpush.android.connector.PushService
import org.unifiedpush.android.connector.data.PushEndpoint
import org.unifiedpush.android.connector.data.PushMessage

class PushService: PushService() {
  private val json = Json {
    ignoreUnknownKeys = true
  }

  @Serializable
  data class PNMessage(
    val verification: String? = null
  )

  private fun onVerification(code: String) {
    CoroutineScope(Dispatchers.Default).launch {
      chatModel.controller.sendCmd(
        null,
        CC.APIVerifySavedNtf(code),
        log = true
      )
    }
  }

  override fun onMessage(message: PushMessage, instance: String) {
    Log.d(TAG, "onMessage")
    val pn: PNMessage = json.decodeFromString(String(message.content))
    when {
      pn.verification != null -> onVerification(pn.verification)
    }
    MessagesFetcherWorker.scheduleWork(200, 0)
  }

  override fun onNewEndpoint(endpoint: PushEndpoint, instance: String) {
    Log.d(TAG, "onNewEndpoint")
    endpoint.pubKeySet ?: run {
      // Should not happen
      Log.w(TAG, "Missing pubKeySet")
      return
    }
    CoroutineScope(Dispatchers.Default).launch {
      chatModel.controller.sendCmd(
        null,
        CC.APIRegisterWebPush(endpoint.url, endpoint.pubKeySet!!.auth, endpoint.pubKeySet!!.pubKey),
        log = true
      )
    }
  }

  override fun onRegistrationFailed(reason: FailedReason, instance: String) {
    Log.d(TAG, "onRegistrationFailed: $reason")
    val title = generalGetString(MR.strings.icon_descr_instant_notifications)
    val text = when (reason) {
      FailedReason.NETWORK -> generalGetString(MR.strings.unifiedpush_registration_failed_network)
      FailedReason.VAPID_REQUIRED, // Should not happen, VAPID will be required
      FailedReason.INTERNAL_ERROR -> generalGetString(MR.strings.unifiedpush_registration_failed_unknown)
      FailedReason.ACTION_REQUIRED -> generalGetString(MR.strings.unifiedpush_registration_failed_action)

    }
    ntfManager.showMessage(title, text)
  }

  override fun onUnregistered(instance: String) {
    Log.d(TAG, "onUnregistered")
    val title = generalGetString(MR.strings.icon_descr_instant_notifications)
    val text = generalGetString(MR.strings.unifiedpush_unregistered)
    ntfManager.showMessage(title, text)
    CoroutineScope(Dispatchers.Default).launch {
      chatModel.controller.sendCmd(
        null,
        CC.APIDeleteSavedNtf(),
        log = true
      )
    }
  }

  companion object {
    private const val TAG = "PushService"
  }
}