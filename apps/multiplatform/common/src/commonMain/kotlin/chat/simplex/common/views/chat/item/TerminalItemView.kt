package chat.simplex.common.views.chat.item

import androidx.compose.foundation.LocalIndication
import androidx.compose.foundation.hoverable
import androidx.compose.foundation.indication
import androidx.compose.foundation.interaction.Interaction
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.PressInteraction
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.pointer.PointerEventPass
import androidx.compose.ui.input.pointer.changedToDownIgnoreConsumed
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.AlignmentLine
import androidx.compose.ui.layout.HorizontalAlignmentLine
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.layout.layout
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.offset
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.Flow
import kotlin.math.max

val LocalTerminalLayout = staticCompositionLocalOf { false }

val TERMINAL_TEXT_BOTTOM_PADDING = 1.dp

fun terminalItemShowsSender(item: ChatItem, older: ChatItem?): Boolean =
  showsSender(item, older) { it.content.hasTerminalSender }

private val CIContent.hasTerminalSender: Boolean get() = when (this) {
  is CIContent.SndMsgContent,
  is CIContent.SndDeleted,
  is CIContent.SndCall,
  is CIContent.SndGroupInvitation,
  is CIContent.SndModerated -> true
  else -> showMemberName
}

class TerminalRowInteractionSource : MutableInteractionSource {
  private val source = MutableInteractionSource()
  private var press: PressInteraction.Press? = null
  var downPosition = Offset.Zero
  override val interactions: Flow<Interaction> = source.interactions
  override suspend fun emit(interaction: Interaction) = source.emit(rowInteraction(interaction))
  override fun tryEmit(interaction: Interaction): Boolean = source.tryEmit(rowInteraction(interaction))

  private fun rowInteraction(interaction: Interaction): Interaction = when (interaction) {
    is PressInteraction.Press -> PressInteraction.Press(downPosition).also { press = it }
    is PressInteraction.Release -> PressInteraction.Release(press ?: interaction.press)
    is PressInteraction.Cancel -> PressInteraction.Cancel(press ?: interaction.press)
    else -> interaction
  }
}

@Composable
fun Modifier.terminalRowIndication(interactionSource: TerminalRowInteractionSource): Modifier =
  this
    .pointerInput(interactionSource) {
      awaitPointerEventScope {
        while (true) {
          awaitPointerEvent(PointerEventPass.Initial).changes.firstOrNull { it.changedToDownIgnoreConsumed() }?.let { interactionSource.downPosition = it.position }
        }
      }
    }
    .hoverable(interactionSource)
    .indication(interactionSource, LocalIndication.current)

fun Modifier.terminalStartOffset(offset: () -> Dp): Modifier =
  layout { measurable, constraints ->
    val start = offset().roundToPx()
    val placeable = measurable.measure(constraints.offset(horizontal = -start))
    layout(placeable.width + start, placeable.height) { placeable.placeRelative(start, 0) }
  }

private val MessageTextBottom = HorizontalAlignmentLine(::max)

fun Modifier.messageTextBottom(): Modifier =
  layout { measurable, constraints ->
    val placeable = measurable.measure(constraints)
    layout(placeable.width, placeable.height, mapOf(MessageTextBottom to placeable.height)) { placeable.placeRelative(0, 0) }
  }

@Composable
fun ItemWithMeta(meta: @Composable () -> Unit, overlayMeta: (@Composable () -> Unit)? = null, content: @Composable (inlineMeta: (@Composable () -> Unit)?) -> Unit) {
  if (LocalTerminalLayout.current) {
    TerminalMetaLayout {
      content(null)
      listOfNotNull(meta, overlayMeta).forEach { m ->
        Box(Modifier.padding(bottom = 6.dp, end = 12.dp)) {
          m()
        }
      }
    }
  } else {
    content(meta)
  }
}

@Composable
private fun TerminalMetaLayout(contentWithMeta: @Composable () -> Unit) {
  Layout(contentWithMeta) { measurables, constraints ->
    val metaConstraints = constraints.copy(minWidth = 0, minHeight = 0)
    val meta = measurables[1].measure(metaConstraints)
    val overlay = measurables.getOrNull(2)?.measure(metaConstraints)
    val contentMaxWidth = if (overlay == null && constraints.hasBoundedWidth) (constraints.maxWidth - meta.width).coerceAtLeast(0) else constraints.maxWidth
    val content = measurables[0].measure(constraints.copy(minWidth = 0, maxWidth = contentMaxWidth))
    val width = if (constraints.hasBoundedWidth) constraints.maxWidth else content.width + meta.width
    val textBottom = content[MessageTextBottom]
    val metaBottom = if (textBottom == AlignmentLine.Unspecified) content.height else textBottom + CHAT_ITEM_TEXT_VERTICAL_PADDING.roundToPx()
    layout(width, content.height) {
      content.placeRelative(0, 0)
      if (overlay != null && content.width + meta.width > width) {
        overlay.placeRelative(content.width - overlay.width, content.height - overlay.height)
      } else {
        meta.placeRelative(width - meta.width, metaBottom - meta.height)
      }
    }
  }
}

@Composable
fun terminalGutter(chatInfo: ChatInfo): Dp =
  if (chatInfo is ChatInfo.Group) MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier else GO_TO_ITEM_BUTTON_SIZE

@Composable
fun TerminalChatItemRow(
  sender: ChatItemSender?,
  gutter: Dp,
  topGap: Dp,
  modifier: Modifier = Modifier,
  senderTopGap: Dp = 8.dp,
  goToButton: @Composable () -> Unit = {},
  content: @Composable () -> Unit
) {
  Layout(
    content = {
      Box {
        sender?.image?.invoke()
      }
      Box {
        goToButton()
      }
      Box {
        if (sender != null) {
          SenderNameAndRole(sender, Modifier.padding(start = CHAT_ITEM_TEXT_PADDING, bottom = 2.dp + topGap))
        }
      }
      Box(Modifier.padding(top = if (sender != null) 2.dp else 0.dp)) {
        content()
      }
    },
    modifier
      .padding(top = if (sender != null) senderTopGap else topGap, start = 8.dp, end = 12.dp)
      .fillMaxWidth()
  ) { measurables, constraints ->
    val gutterWidth = gutter.roundToPx()
    val messageStart = gutterWidth + 4.dp.roundToPx()
    val looseConstraints = constraints.copy(minWidth = 0, minHeight = 0)
    val messageConstraints = looseConstraints.copy(maxWidth = (constraints.maxWidth - messageStart).coerceAtLeast(0))
    val avatar = measurables[0].measure(looseConstraints)
    val button = measurables[1].measure(looseConstraints)
    val name = measurables[2].measure(messageConstraints)
    val message = measurables[3].measure(messageConstraints)
    val messageTop = name.height
    val buttonTop = max(avatar.height, messageTop + (message.height - button.height) / 2)
    val height = maxOf(messageTop + message.height, avatar.height, buttonTop + button.height)
    layout(constraints.maxWidth, height) {
      avatar.placeRelative(0, 0)
      button.placeRelative((gutterWidth - button.width) / 2, buttonTop)
      name.placeRelative(messageStart, 0)
      message.placeRelative(messageStart, messageTop)
    }
  }
}
