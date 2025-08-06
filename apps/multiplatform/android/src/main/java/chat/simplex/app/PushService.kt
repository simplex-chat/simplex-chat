package chat.simplex.app

import chat.simplex.common.model.CC
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.chatModel
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
    // TODO: notification to inform about failed registration
  }

  override fun onUnregistered(instance: String) {
    Log.d(TAG, "onUnregistered")
    // TODO: notification to inform about unregistration
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