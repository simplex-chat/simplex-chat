package chat.simplex.app

import android.app.Application
import android.net.LocalServerSocket
import android.util.Log
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import androidx.work.*
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.CloseSheetBar
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.newchat.ModalView
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.*
import java.util.concurrent.Semaphore
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

// ghc's rts
external fun initHS()
// android-support
external fun pipeStdOutToSocket(socketName: String) : Int

// SimpleX API
typealias ChatCtrl = Long
external fun chatInit(path: String): ChatCtrl
external fun chatSendCmd(ctrl: ChatCtrl, msg: String) : String
external fun chatRecvMsg(ctrl: ChatCtrl) : String

class SimplexApp: Application() {
  private lateinit var controller: ChatController
  lateinit var chatModel: ChatModel
  private lateinit var ntfManager: NtfManager

  fun initiateBackgroundWork() {
    val backgroundConstraints = Constraints.Builder()
      .setRequiredNetworkType(NetworkType.CONNECTED)
      .build()
    val request = OneTimeWorkRequestBuilder<BackgroundAPIWorker>()
      .setInitialDelay(5, TimeUnit.MINUTES)
      .setConstraints(backgroundConstraints)
      .build()
    WorkManager.getInstance(applicationContext)
      .enqueue(request)
  }

  override fun onCreate() {
    super.onCreate()
    ntfManager = NtfManager(applicationContext)
    val ctrl = chatInit(applicationContext.filesDir.toString())
    controller = ChatController(ctrl, AlertManager(), ntfManager, applicationContext)
    chatModel = controller.chatModel
    withApi {
      val user = controller.apiGetActiveUser()
      if (user != null) controller.startChat(user)
    }
  }

  class AlertManager {
    var alertView = mutableStateOf<(@Composable () -> Unit)?>(null)
    var presentAlert = mutableStateOf<Boolean>(false)

    fun showAlert(alert: @Composable () -> Unit) {
      Log.d("SIMPLEX", "AlertManager.showAlert")
      alertView.value = alert
      presentAlert.value = true
    }

    fun hideAlert() {
      presentAlert.value = false
      alertView.value = null
    }

    fun showAlertDialog(
      title: String,
      text: String? = null,
      confirmText: String = "Ok",
      onConfirm: (() -> Unit)? = null,
      dismissText: String = "Cancel",
      onDismiss: (() -> Unit)? = null
    ) {
      val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
      showAlert {
        AlertDialog(
          onDismissRequest = this::hideAlert,
          title = { Text(title) },
          text = alertText,
          confirmButton = {
            Button(onClick = {
              onConfirm?.invoke()
              hideAlert()
            }) { Text(confirmText) }
          },
          dismissButton = {
            Button(onClick = {
              onDismiss?.invoke()
              hideAlert()
            }) { Text(dismissText) }
          }
        )
      }
    }

    fun showAlertMsg(
      title: String, text: String? = null,
      confirmText: String = "Ok", onConfirm: (() -> Unit)? = null
    ) {
      val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
      showAlert {
        AlertDialog(
          onDismissRequest = this::hideAlert,
          title = { Text(title) },
          text = alertText,
          confirmButton = {
            Button(onClick = {
              onConfirm?.invoke()
              hideAlert()
            }) { Text(confirmText) }
          }
        )
      }
    }
  }

  companion object {
    init {
      val socketName = "local.socket.address.listen.native.cmd2"

      val s = Semaphore(0)
      thread(name="stdout/stderr pipe") {
        Log.d("SIMPLEX", "starting server")
        val server = LocalServerSocket(socketName)
        Log.d("SIMPLEX", "started server")
        s.release()
        val receiver = server.accept()
        Log.d("SIMPLEX", "started receiver")
        val logbuffer = FifoQueue<String>(500)
        if (receiver != null) {
          val inStream = receiver.inputStream
          val inStreamReader = InputStreamReader(inStream)
          val input = BufferedReader(inStreamReader)

          while(true) {
            val line = input.readLine() ?: break
            Log.d("SIMPLEX (stdout/stderr)", line)
            logbuffer.add(line)
          }
        }
      }

      System.loadLibrary("app-lib")

      s.acquire()
      pipeStdOutToSocket(socketName)

      initHS()
    }
  }
}

class ModalManager {
  var modalView = mutableStateOf<(@Composable (close: () -> Unit) -> Unit)?>(null)
  var presentModal = mutableStateOf(false)

  fun showModal(content: @Composable () -> Unit) {
    showCustomModal { close -> ModalView(close, content) }
  }

  fun showCustomModal(modal: @Composable (close: () -> Unit) -> Unit) {
    Log.d("SIMPLEX", "ModalManager.showModal")
    modalView.value = modal
    presentModal.value = true
  }

  fun closeModal() {
    modalView.value = null
    presentModal.value = true
  }

  companion object {
    val shared = ModalManager()
  }
}

class FifoQueue<E>(private var capacity: Int) : LinkedList<E>() {
  override fun add(element: E): Boolean {
    if(size > capacity) removeFirst()
    return super.add(element)
  }
}
