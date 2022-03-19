package chat.simplex.app.model

import android.content.Context
import android.util.Log
import androidx.work.*
import chat.simplex.app.TAG
import kotlinx.datetime.Clock
import java.time.Duration

class BGManager(appContext: Context, workerParams: WorkerParameters): //, ctrl: ChatCtrl):
  Worker(appContext, workerParams) {
//  val controller = ctrl

  init {}

  override fun doWork(): Result {
    Log.e(TAG, "BGManager doWork ${Clock.System.now()}")
    schedule(applicationContext)
    getNewItems()
    return Result.success()
  }

  private fun getNewItems() {
    Log.e(TAG, "BGManager getNewItems")
//    val json = chatRecvMsg(controller)
//    val r = APIResponse.decodeStr(json).resp
//    Log.d(TAG, "chatRecvMsg: ${r.responseType}")
  }

  companion object {
    val constraints = Constraints.Builder()
      .setRequiredNetworkType(NetworkType.CONNECTED)
      .build()

    fun schedule(appContext: Context) {
      val request = OneTimeWorkRequestBuilder<BGManager>()
        .setInitialDelay(Duration.ofMinutes(10))
        .setConstraints(constraints)
        .build()
      WorkManager.getInstance(appContext)
        .enqueue(request)
    }
  }
}
