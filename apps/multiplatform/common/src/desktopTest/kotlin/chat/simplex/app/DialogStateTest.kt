package chat.simplex.app

import chat.simplex.common.simplexWindowState
import kotlinx.coroutines.*
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class DialogStateTest {
  @Test
  fun saveRequestWhileOneIsPendingGetsCancelValue() = runBlocking {
    val pending = launch { simplexWindowState.saveDialog.awaitResult() }
    yield()
    val second = async { simplexWindowState.saveDialog.awaitResult() }
    yield()
    try {
      assertTrue(second.isCompleted)
      assertNull(second.await())
    } finally {
      second.cancelAndJoin()
      pending.cancelAndJoin()
    }
  }

  @Test
  fun saveRequestAfterCancelledOneWaitsForItsDialog() = runBlocking {
    val cancelled = launch { simplexWindowState.saveDialog.awaitResult() }
    yield()
    cancelled.cancelAndJoin()
    val next = async { simplexWindowState.saveDialog.awaitResult() }
    yield()
    try {
      assertFalse(next.isCompleted)
    } finally {
      next.cancelAndJoin()
    }
  }
}
