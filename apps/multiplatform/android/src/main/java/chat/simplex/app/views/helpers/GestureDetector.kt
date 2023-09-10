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

import android.util.Log
import androidx.compose.foundation.clickable
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.gestures.*
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.interaction.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.platform.LocalViewConfiguration
import androidx.compose.ui.unit.Density
import chat.simplex.app.TAG
import kotlinx.coroutines.*
import kotlinx.coroutines.sync.Mutex
import kotlin.math.PI
import kotlin.math.abs

/**
 * See original code here: [androidx.compose.foundation.gestures.detectTapGestures]
 * */
interface PressGestureScope: Density {
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
          pressScope.release()
        }
      } catch (_: PointerEventTimeoutCancellationException) {
        if (onLongPress != null) {
          onLongPress(down.position)
          if (shouldConsume)
            consumeUntilUp()
          pressScope.cancel()
        } else {
          if (shouldConsume)
            consumeUntilUp()
          pressScope.release()
        }
      }
    }
  }
}

private suspend fun AwaitPointerEventScope.consumeUntilUp() {
  do {
    val event = awaitPointerEvent()
    event.changes.forEach { it.consumeAllChanges() }
  } while (event.changes.any { it.pressed })
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
    !event.changes.all {
      if (requireUnconsumed) it.changedToDown() else it.changedToDownIgnoreConsumed()
    }
  )
  return event.changes[0]
}

suspend fun AwaitPointerEventScope.waitForUpOrCancellation(): PointerInputChange? {
  while (true) {
    val event = awaitPointerEvent(PointerEventPass.Main)
    if (event.changes.all { it.changedToUp() }) {
      return event.changes[0]
    }

    if (event.changes.any {
        it.consumed.downChange || it.isOutOfBounds(size, extendedTouchPadding)
      }
    ) {
      return null
    }
    val consumeCheck = awaitPointerEvent(PointerEventPass.Final)
    if (consumeCheck.changes.any { it.positionChangeConsumed() }) {
      return null
    }
  }
}

private class PressGestureScopeImpl(
  density: Density
): PressGestureScope, Density by density {
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
    return isReleased && !isCanceled
  }
}

/**
 * Captures click events and calls [onLongClick] or [onClick] when such even happens. Otherwise, does nothing.
 * Apply [MutableInteractionSource] to any element that allows to pass it in (for example, in [Modifier.clickable]).
 * Works in situations when using [Modifier.combinedClickable] doesn't work because external element overrides [Modifier.clickable]
 * */
@Composable
fun interactionSourceWithDetection(onClick: () -> Unit, onLongClick: () -> Unit): MutableInteractionSource {
  val interactionSource = remember { MutableInteractionSource() }
  val longPressTimeoutMillis = LocalViewConfiguration.current.longPressTimeoutMillis
  var topLevelInteraction: Interaction? by remember { mutableStateOf(null) }
  LaunchedEffect(interactionSource) {
    interactionSource.interactions.collect { interaction ->
      topLevelInteraction = interaction
    }
  }
  LaunchedEffect(topLevelInteraction is PressInteraction.Press) {
    if (topLevelInteraction !is PressInteraction.Press) return@LaunchedEffect
    try {
      withTimeout(longPressTimeoutMillis) {
        while (isActive) {
          delay(10)
          when (topLevelInteraction) {
            is PressInteraction.Press -> {}
            is PressInteraction.Release -> {
              onClick(); break
            }
            is PressInteraction.Cancel -> break
          }
        }
      }
    } catch (_: TimeoutCancellationException) {
      // Long click happened
      onLongClick()
    } catch (ex: CancellationException) {
      // Canceled coroutine + PressInteraction.Release == short click
      if (topLevelInteraction is PressInteraction.Release)
        onClick()
      Log.e(TAG, ex.stackTraceToString())
    } catch (ex: Exception) {
      // Should never be called
      Log.e(TAG, ex.stackTraceToString())
    }
  }
  return interactionSource
}

@Composable
fun interactionSourceWithTapDetection(onPress: () -> Unit, onClick: () -> Unit, onCancel: () -> Unit, onRelease: ()-> Unit): MutableInteractionSource {
  val interactionSource = remember { MutableInteractionSource() }
  LaunchedEffect(interactionSource) {
    var firstTapTime = 0L
    interactionSource.interactions.collect { interaction ->
      when (interaction) {
        is PressInteraction.Press -> {
          firstTapTime = System.currentTimeMillis(); onPress()
        }
        is PressInteraction.Release -> if (firstTapTime + 1000L < System.currentTimeMillis()) onRelease() else onClick()
        is PressInteraction.Cancel -> onCancel()
      }
    }
  }
  return interactionSource
}

suspend fun PointerInputScope.detectTransformGestures(
  allowIntercept: () -> Boolean,
  panZoomLock: Boolean = false,
  onGesture: (centroid: Offset, pan: Offset, zoom: Float, rotation: Float) -> Unit
) {
  var zoom = 1f
  forEachGesture {
    awaitPointerEventScope {
      var rotation = 0f
      var pan = Offset.Zero
      var pastTouchSlop = false
      val touchSlop = viewConfiguration.touchSlop
      var lockedToPanZoom = false

      awaitFirstDown(requireUnconsumed = false)
      do {
        val event = awaitPointerEvent()
        val canceled = event.changes.any { it.isConsumed }
        if (!canceled) {
          val zoomChange = event.calculateZoom()
          val rotationChange = event.calculateRotation()
          val panChange = event.calculatePan()

          if (!pastTouchSlop) {
            zoom *= zoomChange
            rotation += rotationChange
            pan += panChange

            val centroidSize = event.calculateCentroidSize(useCurrent = false)
            val zoomMotion = abs(1 - zoom) * centroidSize
            val rotationMotion = abs(rotation * PI.toFloat() * centroidSize / 180f)
            val panMotion = pan.getDistance()

            if (zoomMotion > touchSlop ||
              rotationMotion > touchSlop ||
              panMotion > touchSlop
            ) {
              pastTouchSlop = true
              lockedToPanZoom = panZoomLock && rotationMotion < touchSlop
            }
          }

          if (pastTouchSlop) {
            val centroid = event.calculateCentroid(useCurrent = false)
            val effectiveRotation = if (lockedToPanZoom) 0f else rotationChange
            if (effectiveRotation != 0f ||
              zoomChange != 1f ||
              panChange != Offset.Zero
            ) {
              onGesture(centroid, panChange, zoomChange, effectiveRotation)
            }
            event.changes.forEach {
              if (it.positionChanged() && zoom != 1f && allowIntercept()) {
                it.consume()
              }
            }
          }
        }
      } while (!canceled && event.changes.any { it.pressed })
    }
  }
}
