package chat.simplex.app

import android.content.Context
import android.util.Log
import androidx.work.*
import chat.simplex.app.SimplexService.Companion.showPassphraseNotification
import chat.simplex.common.model.ChatController
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.DBMigrationResult
import chat.simplex.common.views.helpers.DatabaseUtils
import kotlinx.coroutines.*
import java.util.Date
import java.util.concurrent.TimeUnit

object MessagesFetcherWorker {
  private const val UNIQUE_WORK_TAG = BuildConfig.APPLICATION_ID + ".UNIQUE_MESSAGES_FETCHER"

  /**
   * Schedule [MessagesFetcherWork]
   *
   * @param initialDelayMs Delay in milliseconds before starting the jobs, usually = [intervalMs]
   * @param intervalMs Interval in milliseconds between the end of a jobs and the start of a new one.
   *     If set to `0`, the job isn't restarted
   * @param timeoutMs Timeout in milliseconds until we stop a job, if there are still new messages, a new job is started directly
   * @param durationMs Duration in milliseconds to wait for new messages
   */
  fun scheduleWork(
    initialDelayMs: Int = MessagesFetcherWork.DEFAULT_INTERVAL_MS,
    intervalMs: Int = MessagesFetcherWork.DEFAULT_INTERVAL_MS,
    timeoutMs: Int = MessagesFetcherWork.DEFAULT_TIMEOUT_MS,
    durationMs: Int = MessagesFetcherWork.DEFAULT_DURATION_MS
  ) {
    Int.MAX_VALUE
    Log.d(TAG, "Worker: scheduling work to run at ${Date(System.currentTimeMillis() + initialDelayMs)} for $timeoutMs ms max")
    val periodicWorkRequest = OneTimeWorkRequest.Builder(MessagesFetcherWork::class.java)
      .setInitialDelay(initialDelayMs.toLong(), TimeUnit.MILLISECONDS)
      .setInputData(
        Data.Builder()
          .putInt(MessagesFetcherWork.INPUT_DATA_INTERVAL, intervalMs)
          .putInt(MessagesFetcherWork.INPUT_DATA_TIMEOUT, timeoutMs)
          .putInt(MessagesFetcherWork.INPUT_DATA_DURATION, durationMs)
          .build()
      )
      .setConstraints(Constraints.Builder().setRequiredNetworkType(NetworkType.CONNECTED).build())
      .build()

    SimplexApp.context.getWorkManagerInstance().enqueueUniqueWork(UNIQUE_WORK_TAG, ExistingWorkPolicy.REPLACE, periodicWorkRequest)
  }

  fun cancelAll(withLog: Boolean = true) {
    if (withLog) {
      Log.d(TAG, "Worker: canceled all tasks")
    }
    SimplexApp.context.getWorkManagerInstance().cancelUniqueWork(UNIQUE_WORK_TAG)
  }
}

class MessagesFetcherWork(
  context: Context,
  workerParams: WorkerParameters
): CoroutineWorker(context, workerParams) {
  companion object {
    /**
     * Interval, in milliseconds, between the end of a job and the start of a new one
     */
    const val INPUT_DATA_INTERVAL = "interval"

    /**
     * Timeout, in milliseconds, until we force stop the jobs, if there are new messages, a new job is started directly
     */
    const val INPUT_DATA_TIMEOUT = "timeout"

    /**
     * Minimum duration, in milliseconds, to wait for new messages
     */
    const val INPUT_DATA_DURATION = "duration"
    const val DEFAULT_DURATION_MS = 10_000
    const val DEFAULT_TIMEOUT_MS = 60_000
    const val DEFAULT_INTERVAL_MS = 600_000
  }

  override suspend fun doWork(): Result {
    // Skip when Simplex service is currently working
    val durationMs = inputData.getInt(INPUT_DATA_DURATION, DEFAULT_DURATION_MS)
    val timeoutMs = inputData.getInt(INPUT_DATA_TIMEOUT, DEFAULT_TIMEOUT_MS)
    val intervalMs = inputData.getInt(INPUT_DATA_INTERVAL, DEFAULT_INTERVAL_MS)
    // initialDelayMs may be = 0
    // in this case, reschedule won't start a worker.
    var initialDelayMs = intervalMs
    if (SimplexService.getServiceState(SimplexApp.context) == SimplexService.ServiceState.STARTED) {
      reschedule(
        initialDelayMs = initialDelayMs,
        intervalMs = intervalMs,
        timeoutMs = timeoutMs,
        durationMs = durationMs
      )
      return Result.success()
    }
    var shouldReschedule = true
    try {
      // In case of self-destruct is enabled the initialization process will not start in SimplexApp, Let's start it here
      if (DatabaseUtils.ksSelfDestructPassword.get() != null && chatModel.chatDbStatus.value == null) {
        initChatControllerOnStart()
      }
      withTimeout(timeoutMs.toLong()) {
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
        delay(durationMs + 200L)
        while (!isStopped) {
          val lastMsgMs = System.currentTimeMillis() - chatController.lastMsgReceivedTimestamp
          if (lastMsgMs > durationMs) {
            Log.d(TAG, "Worker: work is done")
            break
          }
          delay(durationMs - lastMsgMs + 200L)
        }
      }
    } catch (_: TimeoutCancellationException) { // When timeout happens
      Log.d(TAG, "Worker: Still work to do, restarting a new work (took $durationMs sec)")
      // We reschedule a new work now (in 200 ms)
      shouldReschedule = true
      initialDelayMs = 200
    } catch (_: CancellationException) { // When user opens the app while the worker is still working
      Log.d(TAG, "Worker: interrupted")
    } catch (e: Exception) {
      Log.d(TAG, "Worker: unexpected exception: ${e.stackTraceToString()}")
    }

    if (shouldReschedule) {
      reschedule(
        initialDelayMs = initialDelayMs,
        intervalMs = intervalMs,
        timeoutMs = timeoutMs,
        durationMs = durationMs
      )
    }
    return Result.success()
  }

  /**
   * Reschedule a work if [initialDelayMs] > 0
   */
  private fun reschedule(
    initialDelayMs: Int,
    intervalMs: Int,
    timeoutMs: Int,
    durationMs: Int
  ) = {
    if (initialDelayMs > 0)
      MessagesFetcherWorker.scheduleWork(initialDelayMs, intervalMs, timeoutMs, durationMs)
  }
}
