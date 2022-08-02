/*
 * Copyright 2020 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package chat.simplex.app.views.helpers

import androidx.compose.foundation.gestures.forEachGesture
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.pointer.AwaitPointerEventScope
import androidx.compose.ui.input.pointer.PointerEventTimeoutCancellationException
import androidx.compose.ui.input.pointer.PointerEvent
import androidx.compose.ui.input.pointer.PointerEventPass
import androidx.compose.ui.input.pointer.PointerInputChange
import androidx.compose.ui.input.pointer.PointerInputScope
import androidx.compose.ui.input.pointer.changedToDown
import androidx.compose.ui.input.pointer.changedToDownIgnoreConsumed
import androidx.compose.ui.input.pointer.changedToUp
import androidx.compose.ui.input.pointer.consumeAllChanges
import androidx.compose.ui.input.pointer.consumeDownChange
import androidx.compose.ui.input.pointer.isOutOfBounds
import androidx.compose.ui.input.pointer.positionChangeConsumed
import androidx.compose.ui.unit.Density
import androidx.compose.ui.util.fastAll
import androidx.compose.ui.util.fastAny
import androidx.compose.ui.util.fastForEach
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.sync.Mutex

/**
 * See original code here: [androidx.compose.foundation.gestures.detectTapGestures]
 * */

interface PressGestureScope : Density {
  suspend fun tryAwaitRelease(): Boolean
}

private val NoPressGesture: suspend PressGestureScope.(Offset) -> Unit = { }

suspend fun PointerInputScope.detectGesture(
  onLongPress: ((Offset) -> Unit)? = null,
  onPress: suspend PressGestureScope.(Offset) -> Unit = NoPressGesture,
  shouldConsumeEvent: (Offset) -> Boolean
) = coroutineScope {
  val pressScope = PressGestureScopeImpl(this@detectGesture)

  forEachGesture {
    awaitPointerEventScope {
      val down = awaitFirstDown()
      // If shouldConsumeEvent == false, all touches will be propagated to parent
      val shouldConsume = shouldConsumeEvent(down.position)
      if (shouldConsume)
        down.consumeDownChange()
      pressScope.reset()
      if (onPress !== NoPressGesture) launch {
        pressScope.onPress(down.position)
      }

      val longPressTimeout = onLongPress?.let {
        viewConfiguration.longPressTimeoutMillis
      } ?: (Long.MAX_VALUE / 2)

      try {
        val upOrCancel: PointerInputChange? = withTimeout(longPressTimeout) {
          waitForUpOrCancellation()
        }
        if (upOrCancel == null) {
          pressScope.cancel()
        } else {
          if (shouldConsume)
            upOrCancel.consumeDownChange()

          // If onLongPress event is needed, cancel short press event
          if (onLongPress != null)
            pressScope.cancel()
          else
            pressScope.release()
        }
      } catch (_: PointerEventTimeoutCancellationException) {
        onLongPress?.invoke(down.position)
        if (shouldConsume)
          consumeUntilUp()
        pressScope.release()
      }
    }
  }
}

private suspend fun AwaitPointerEventScope.consumeUntilUp() {
  do {
    val event = awaitPointerEvent()
    event.changes.fastForEach { it.consumeAllChanges() }
  } while (event.changes.fastAny { it.pressed })
}

suspend fun AwaitPointerEventScope.awaitFirstDown(
  requireUnconsumed: Boolean = true
): PointerInputChange =
  awaitFirstDownOnPass(pass = PointerEventPass.Main, requireUnconsumed = requireUnconsumed)

internal suspend fun AwaitPointerEventScope.awaitFirstDownOnPass(
  pass: PointerEventPass,
  requireUnconsumed: Boolean
): PointerInputChange {
  var event: PointerEvent
  do {
    event = awaitPointerEvent(pass)
  } while (
    !event.changes.fastAll {
      if (requireUnconsumed) it.changedToDown() else it.changedToDownIgnoreConsumed()
    }
  )
  return event.changes[0]
}

suspend fun AwaitPointerEventScope.waitForUpOrCancellation(): PointerInputChange? {
  while (true) {
    val event = awaitPointerEvent(PointerEventPass.Main)
    if (event.changes.fastAll { it.changedToUp() }) {
      return event.changes[0]
    }

    if (event.changes.fastAny {
        it.consumed.downChange || it.isOutOfBounds(size, extendedTouchPadding)
      }
    ) {
      return null
    }

    val consumeCheck = awaitPointerEvent(PointerEventPass.Final)
    if (consumeCheck.changes.fastAny { it.positionChangeConsumed() }) {
      return null
    }
  }
}

private class PressGestureScopeImpl(
  density: Density
) : PressGestureScope, Density by density {
  private var isReleased = false
  private var isCanceled = false
  private val mutex = Mutex(locked = false)

  fun cancel() {
    isCanceled = true
    mutex.unlock()
  }

  fun release() {
    isReleased = true
    mutex.unlock()
  }

  fun reset() {
    mutex.tryLock()
    isReleased = false
    isCanceled = false
  }

  override suspend fun tryAwaitRelease(): Boolean {
    if (!isReleased && !isCanceled) {
      mutex.lock()
    }
    return isCanceled
  }
}