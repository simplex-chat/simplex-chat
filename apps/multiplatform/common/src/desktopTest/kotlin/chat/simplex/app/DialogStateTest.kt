package chat.simplex.app

import chat.simplex.common.simplexWindowState
import kotlinx.coroutines.*
import kotlin.test.Test
import kotlin.test.assertNull

class DialogStateTest {
  @Test
  fun saveRequestWhileOneIsPendingGetsCancelValue() = runBlocking {
    val pending = launch(Dispatchers.Default) { simplexWindowState.saveDialog.awaitResult() }
    delay(500)
    val second = async(Dispatchers.Default) { simplexWindowState.saveDialog.awaitResult() }
    try {
      assertNull(withTimeout(5000) { second.await() })
    } finally {
      second.cancelAndJoin()
      pending.cancelAndJoin()
    }
  }

  @Test
  fun cancelledSaveRequestFreesTheSlot() = runBlocking {
    val cancelled = launch(Dispatchers.Default) { simplexWindowState.saveDialog.awaitResult() }
    delay(500)
    cancelled.cancelAndJoin()
    val next = async(Dispatchers.Default) { simplexWindowState.saveDialog.awaitResult() }
    try {
      assertNull(withTimeoutOrNull(1000) { next.await(); "completed" })
    } finally {
      next.cancelAndJoin()
    }
  }
}
