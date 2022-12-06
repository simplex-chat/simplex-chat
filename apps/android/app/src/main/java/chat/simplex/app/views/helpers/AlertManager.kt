package chat.simplex.app.views.helpers

import android.util.Log
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Shape
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.layout.Placeable
import androidx.compose.ui.unit.*
import androidx.compose.ui.util.fastForEachIndexed
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import kotlin.math.max

class AlertManager {
  var alertViews = mutableStateListOf<(@Composable () -> Unit)>()

  fun showAlert(alert: @Composable () -> Unit) {
    Log.d(TAG, "AlertManager.showAlert")
    alertViews.add(alert)
  }

  fun hideAlert() {
    alertViews.removeLastOrNull()
  }

  fun showAlertDialogButtons(
    title: String,
    text: String? = null,
    buttons: @Composable () -> Unit,
  ) {
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = { Text(title) },
        text = alertText,
        buttons = buttons
      )
    }
  }

  fun showAlertDialogButtonsColumn(
    title: String,
    text: String? = null,
    buttons: @Composable () -> Unit,
  ) {
    showAlert {
      Dialog(onDismissRequest = this::hideAlert) {
        Column(Modifier.background(MaterialTheme.colors.background)) {
          Text(title, Modifier.padding(DEFAULT_PADDING), fontSize = 18.sp)
          if (text != null) {
            Text(text)
          }
          CompositionLocalProvider(LocalContentAlpha provides ContentAlpha.high) {
            buttons()
          }
        }
      }
    }
  }

  fun showAlertDialog(
    title: String,
    text: String? = null,
    confirmText: String = generalGetString(R.string.ok),
    onConfirm: (() -> Unit)? = null,
    dismissText: String = generalGetString(R.string.cancel_verb),
    onDismiss: (() -> Unit)? = null,
    onDismissRequest: (() -> Unit)? = null,
    destructive: Boolean = false
  ) {
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = { Text(title) },
        text = alertText,
        buttons = {
          Box(Modifier.fillMaxWidth().padding(horizontal = 8.dp, vertical = 2.dp)) {
            AlertDialogFlowRow(
              mainAxisSpacing = 8.dp,
              crossAxisSpacing = 0.dp
            ) {
              TextButton(onClick = {
                onDismiss?.invoke()
                hideAlert()
              }) { Text(dismissText) }
              TextButton(onClick = {
                onConfirm?.invoke()
                hideAlert()
              }) { Text(confirmText, color = if (destructive) MaterialTheme.colors.error else Color.Unspecified) }
            }
          }
        }
      )
    }
  }

  fun showAlertMsg(
    title: String, text: String? = null,
    confirmText: String = generalGetString(R.string.ok), onConfirm: (() -> Unit)? = null
  ) {
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = { Text(title) },
        text = alertText,
        confirmButton = {
          TextButton(onClick = {
            onConfirm?.invoke()
            hideAlert()
          }) { Text(confirmText) }
        }
      )
    }
  }

  fun showAlertMsg(
    title: Int,
    text: Int? = null,
    confirmText: Int = R.string.ok,
    onConfirm: (() -> Unit)? = null
  ) = showAlertMsg(generalGetString(title), if (text != null) generalGetString(text) else null, generalGetString(confirmText), onConfirm)

  @Composable
  fun showInView() {
    remember { alertViews }.lastOrNull()?.invoke()
  }

  companion object {
    val shared = AlertManager()
  }
}

/**
 * Cloned from Compose source to be able to pass [crossAxisSpacing] parameter since default one is too large which makes vertically placed
 * buttons too far from each other.
 * [AlertDialogFlowRow] is internal in Compose
 * */
@Composable
private fun AlertDialogFlowRow(
  mainAxisSpacing: Dp,
  crossAxisSpacing: Dp,
  content: @Composable () -> Unit
) {
  Layout(content) { measurables, constraints ->
    val sequences = mutableListOf<List<Placeable>>()
    val crossAxisSizes = mutableListOf<Int>()
    val crossAxisPositions = mutableListOf<Int>()

    var mainAxisSpace = 0
    var crossAxisSpace = 0

    val currentSequence = mutableListOf<Placeable>()
    var currentMainAxisSize = 0
    var currentCrossAxisSize = 0

    val childConstraints = Constraints(maxWidth = constraints.maxWidth)

    // Return whether the placeable can be added to the current sequence.
    fun canAddToCurrentSequence(placeable: Placeable) =
      currentSequence.isEmpty() || currentMainAxisSize + mainAxisSpacing.roundToPx() +
          placeable.width <= constraints.maxWidth

    // Store current sequence information and start a new sequence.
    fun startNewSequence() {
      if (sequences.isNotEmpty()) {
        crossAxisSpace += crossAxisSpacing.roundToPx()
      }
      sequences += currentSequence.toList()
      crossAxisSizes += currentCrossAxisSize
      crossAxisPositions += crossAxisSpace

      crossAxisSpace += currentCrossAxisSize
      mainAxisSpace = max(mainAxisSpace, currentMainAxisSize)

      currentSequence.clear()
      currentMainAxisSize = 0
      currentCrossAxisSize = 0
    }

    for (measurable in measurables) {
      // Ask the child for its preferred size.
      val placeable = measurable.measure(childConstraints)

      // Start a new sequence if there is not enough space.
      if (!canAddToCurrentSequence(placeable)) startNewSequence()

      // Add the child to the current sequence.
      if (currentSequence.isNotEmpty()) {
        currentMainAxisSize += mainAxisSpacing.roundToPx()
      }
      currentSequence.add(placeable)
      currentMainAxisSize += placeable.width
      currentCrossAxisSize = max(currentCrossAxisSize, placeable.height)
    }

    if (currentSequence.isNotEmpty()) startNewSequence()

    val mainAxisLayoutSize = if (constraints.maxWidth != Constraints.Infinity) {
      constraints.maxWidth
    } else {
      max(mainAxisSpace, constraints.minWidth)
    }
    val crossAxisLayoutSize = max(crossAxisSpace, constraints.minHeight)

    val layoutWidth = mainAxisLayoutSize

    val layoutHeight = crossAxisLayoutSize

    layout(layoutWidth, layoutHeight) {
      sequences.fastForEachIndexed { i, placeables ->
        val childrenMainAxisSizes = IntArray(placeables.size) { j ->
          placeables[j].width +
              if (j < placeables.lastIndex) mainAxisSpacing.roundToPx() else 0
        }
        val arrangement = Arrangement.Bottom
        // Handle vertical direction
        val mainAxisPositions = IntArray(childrenMainAxisSizes.size) { 0 }
        with(arrangement) {
          arrange(mainAxisLayoutSize, childrenMainAxisSizes, mainAxisPositions)
        }
        placeables.fastForEachIndexed { j, placeable ->
          placeable.place(
            x = mainAxisPositions[j],
            y = crossAxisPositions[i]
          )
        }
      }
    }
  }
}
