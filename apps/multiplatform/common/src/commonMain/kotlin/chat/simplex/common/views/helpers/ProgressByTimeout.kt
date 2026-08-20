package chat.simplex.common.views.helpers

import androidx.compose.runtime.*
import kotlinx.coroutines.delay

private const val PROGRESS_TIMEOUT_MS = 1000L

@Composable
fun ProgressByTimeoutEffect(inProgress: Boolean, setProgressByTimeout: (Boolean) -> Unit) {
  LaunchedEffect(inProgress) {
    if (inProgress) {
      delay(PROGRESS_TIMEOUT_MS)
      setProgressByTimeout(true)
    } else {
      setProgressByTimeout(false)
    }
  }
}

@Composable
fun rememberProgressByTimeout(inProgress: State<Boolean>): State<Boolean> {
  val progressByTimeout = remember { mutableStateOf(false) }
  ProgressByTimeoutEffect(inProgress.value) { progressByTimeout.value = it }
  return progressByTimeout
}
