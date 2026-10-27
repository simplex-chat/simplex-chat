package chat.simplex.app

import androidx.compose.ui.geometry.Rect
import chat.simplex.common.views.chat.item.ChatSendAnimations
import kotlinx.coroutines.*
import kotlin.test.*

class ChatSendAnimationTest {
  private fun withController(test: (ChatSendAnimations) -> Unit) {
    val scope = CoroutineScope(SupervisorJob() + Dispatchers.Unconfined)
    val controller = ChatSendAnimations(scope)
    try {
      test(controller)
    } finally {
      controller.dispose()
      scope.cancel()
    }
  }

  @Test
  fun aResponseAfterLeavingChatCannotAnimateInAnotherSession() = withController { old ->
    old.composerBounds = Rect(10f, 600f, 350f, 650f)
    val pending = assertNotNull(old.capture())
    old.dispose()
    pending.register(42)
    assertTrue(old.transitions.isEmpty())
    assertNull(old.capture())
    withController { reopened ->
      assertTrue(reopened.transitions.isEmpty())
    }
  }

  @Test
  fun rapidSendsKeepTheirOwnCapturedInputBounds() = withController { controller ->
    val firstBounds = Rect(10f, 600f, 350f, 650f)
    controller.composerBounds = firstBounds
    val first = assertNotNull(controller.capture())
    val secondBounds = Rect(10f, 550f, 350f, 650f)
    controller.composerBounds = secondBounds
    val second = assertNotNull(controller.capture())
    second.register(43)
    first.register(42)
    assertEquals(firstBounds, controller.transitions[42]?.source)
    assertEquals(secondBounds, controller.transitions[43]?.source)
    controller.finish(42, assertNotNull(controller.transitions[42]))
    assertNotNull(controller.transitions[43])
  }

  @Test
  fun missingInputBoundsAndUnmarkedHistoryHaveNoTransition() = withController { controller ->
    assertNull(controller.capture())
    controller.composerBounds = Rect.Zero
    assertNull(controller.capture())
    controller.composerBounds = Rect(10f, 600f, 350f, 650f)
    assertNotNull(controller.capture())
    assertTrue(controller.transitions.isEmpty()) // Capture alone never marks history.
  }
}