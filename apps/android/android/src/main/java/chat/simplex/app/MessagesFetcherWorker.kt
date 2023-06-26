package chat.simplex.app

import android.content.Context
import chat.simplex.common.platform.Log
import androidx.work.*
import chat.simplex.app.SimplexService.Companion.showPassphraseNotification
import chat.simplex.common.model.ChatController
import chat.simplex.common.platform.TAG
import chat.simplex.common.views.helpers.DBMigrationResult
import kotlinx.coroutines.*
import java.util.Date
import java.util.concurrent.TimeUnit

object MessagesFetcherWorker {
  private const val UNIQUE_WORK_TAG = BuildConfig.APPLICATION_ID + ".UNIQUE_MESSAGES_FETCHER"

  fun scheduleWork(intervalSec: Int = 600, durationSec: Int = 60) {
    val initialDelaySec = intervalSec.toLong()
    Log.d(TAG, "Worker: scheduling work to run at ${Date(System.currentTimeMillis() + initialDelaySec * 1000)} for $durationSec sec")
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

    WorkManager.getInstance(SimplexApp.context).enqueueUniqueWork(UNIQUE_WORK_TAG, ExistingWorkPolicy.REPLACE, periodicWorkRequest)
  }

  fun cancelAll() {
    Log.d(TAG, "Worker: canceled all tasks")
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
    private const val WAIT_AFTER_LAST_MESSAGE: Long = 10_000
  }

  override suspend fun doWork(): Result {
    // Skip when Simplex service is currently working
    if (SimplexService.getServiceState(SimplexApp.context) == SimplexService.ServiceState.STARTED) {
      reschedule()
      return Result.success()
    }
    val durationSeconds = inputData.getInt(INPUT_DATA_DURATION, 60)
    var shouldReschedule = true
    try {
      withTimeout(durationSeconds * 1000L) {
        val chatController = ChatController
        SimplexService.waitDbMigrationEnds(chatController)
        val chatDbStatus = chatController.chatModel.chatDbStatus.value
        if (chatDbStatus != DBMigrationResult.OK) {
          Log.w(TAG, "Worker: problem with the database: $chatDbStatus")
          showPassphraseNotification(chatDbStatus)
          shouldReschedule = false
          return@withTimeout
        }
        Log.w(TAG, "Worker: starting work")
        // Give some time to start receiving messages
        delay(10_000)
        while (!isStopped) {
          if (chatController.lastMsgReceivedTimestamp + WAIT_AFTER_LAST_MESSAGE < System.currentTimeMillis()) {
            Log.d(TAG, "Worker: work is done")
            break
          }
          delay(5000)
        }
      }
    } catch (_: TimeoutCancellationException) { // When timeout happens
      Log.d(TAG, "Worker: work is done (took $durationSeconds sec)")
    } catch (_: CancellationException) { // When user opens the app while the worker is still working
      Log.d(TAG, "Worker: interrupted")
    } catch (e: Exception) {
      Log.d(TAG, "Worker: unexpected exception: ${e.stackTraceToString()}")
    }

    if (shouldReschedule) reschedule()
    return Result.success()
  }

  private fun reschedule() = MessagesFetcherWorker.scheduleWork()
}
