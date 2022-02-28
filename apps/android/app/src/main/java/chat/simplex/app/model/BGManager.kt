package chat.simplex.app.model

import android.content.Context
import android.util.Log
import androidx.work.*
import chat.simplex.app.TAG
import chat.simplex.app.chatRecvMsg
import java.util.concurrent.TimeUnit

class BackgroundAPIWorker(appContext: Context, workerParams: WorkerParameters, ctrl: ChatCtrl):
  Worker(appContext, workerParams) {
  val controller = ctrl
  override fun doWork(): Result {
    Log.d("BackgroundAPIWorker", "running")
    getNewItems()

    // Enqueue another request for later to make this periodic
    val request = buildRequest()
    WorkManager.getInstance(applicationContext)
      .enqueue(request)

    return Result.success()
  }

  private fun getNewItems() {
    val json = chatRecvMsg(controller)
    val r = APIResponse.decodeStr(json).resp
    Log.d(TAG, "chatRecvMsg: ${r.responseType}")
  }

  private fun buildRequest(): OneTimeWorkRequest {
    val backgroundConstraints = Constraints.Builder()
      .setRequiredNetworkType(NetworkType.CONNECTED)
      .build()
    return OneTimeWorkRequestBuilder<BackgroundAPIWorker>()
      .setInitialDelay(5, TimeUnit.MINUTES)
      .setConstraints(backgroundConstraints)
      .build()
  }
}
