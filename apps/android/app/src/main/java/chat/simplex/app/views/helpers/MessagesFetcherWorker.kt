package chat.simplex.app.views.helpers

import android.content.Context
import android.util.Log
import androidx.work.*
import chat.simplex.app.*
import kotlinx.coroutines.*
import java.util.Date
import java.util.concurrent.TimeUnit

object MessagesFetcherWorker {
  private const val UNIQUE_WORK_TAG = BuildConfig.APPLICATION_ID + ".UNIQUE_MESSAGES_FETCHER"

  fun scheduleWork(intervalSec: Int = 600, durationSec: Int = 10) {
    val initialDelaySec = intervalSec.toLong()
    Log.d(TAG, "Scheduling work to run at ${Date(System.currentTimeMillis() + initialDelaySec * 1000)} for $durationSec sec")
    val periodicWorkRequest = OneTimeWorkRequest.Builder(MessagesFetcherWork::class.java)
      .setInitialDelay(initialDelaySec, TimeUnit.SECONDS)
      .setInputData(
        Data.Builder()
          .putInt(MessagesFetcherWork.INPUT_DATA_INTERVAL, intervalSec)
          .putInt(MessagesFetcherWork.INPUT_DATA_DURATION, durationSec)
          .build()
      )
      .setConstraints(Constraints.Builder().setRequiredNetworkType(NetworkType.CONNECTED).build())
      .build()

    cancelAll()
    WorkManager.getInstance(SimplexApp.context).enqueueUniqueWork(UNIQUE_WORK_TAG, ExistingWorkPolicy.REPLACE, periodicWorkRequest)
  }

  fun cancelAll() {
    WorkManager.getInstance(SimplexApp.context).cancelUniqueWork(UNIQUE_WORK_TAG)
  }
}

class MessagesFetcherWork(
  context: Context,
  workerParams: WorkerParameters
): CoroutineWorker(context, workerParams) {
  companion object {
    const val INPUT_DATA_INTERVAL = "interval"
    const val INPUT_DATA_DURATION = "duration"
  }

  override suspend fun doWork(): Result {
    // Skip when Simplex service is currently working
    if (SimplexService.getServiceState(SimplexApp.context) == SimplexService.ServiceState.STARTED) {
      reschedule()
      return Result.success()
    }
    val durationSeconds = inputData.getInt(INPUT_DATA_DURATION, 10)
    try {
      withTimeout(durationSeconds * 1000L) {
        try {
          val chatController = (applicationContext as SimplexApp).chatController
          val user = chatController.apiGetActiveUser() ?: return@withTimeout
          Log.w(TAG, "Starting work")
          chatController.startChat(user)
          while (!isStopped) {
            delay(100)
          }
        } catch (e: TimeoutCancellationException) {
          Log.d(TAG, "Work is done")
        } catch (e: Exception) {
          Log.e(TAG, e.stackTraceToString())
        }
      }
    } catch (_: TimeoutCancellationException) {
    }

    reschedule()
    return Result.success()
  }

  private fun reschedule() = MessagesFetcherWorker.scheduleWork()
}
