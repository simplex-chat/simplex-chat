package chat.simplex.common.views.chat.item

import androidx.compose.foundation.LocalIndication
import androidx.compose.foundation.clickable
import androidx.compose.foundation.hoverable
import androidx.compose.foundation.indication
import androidx.compose.foundation.interaction.Interaction
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.PressInteraction
import androidx.compose.foundation.layout.*
import androidx.compose.material.LocalTextStyle
import androidx.compose.material.MaterialTheme
import androidx.compose.material.ProvideTextStyle
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.compositeOver
import androidx.compose.ui.graphics.luminance
import androidx.compose.ui.input.pointer.PointerEventPass
import androidx.compose.ui.input.pointer.changedToDownIgnoreConsumed
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.AlignmentLine
import androidx.compose.ui.layout.FirstBaseline
import androidx.compose.ui.layout.HorizontalAlignmentLine
import androidx.compose.ui.layout.LastBaseline
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.layout.Placeable
import androidx.compose.ui.layout.VerticalAlignmentLine
import androidx.compose.ui.layout.layout
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.offset
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.Flow
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow
import kotlin.math.roundToInt

val LocalTerminalLayout = staticCompositionLocalOf { false }

val TERMINAL_BLOCK_TOP_GAP = 5.75.sp
val TERMINAL_BLOCK_BOTTOM_GAP = 5.sp
val TERMINAL_LEVEL_STEP = 4.75.sp
val TERMINAL_HEADING_BOTTOM_GAP = TERMINAL_LEVEL_STEP / 2
val TERMINAL_SENDER_TOP_GAP = 6.5.sp
private val TERMINAL_SENDER_BOTTOM_GAP = 5.5.sp
val TERMINAL_HEADER_TOP_GAP = 2.5.sp
val TERMINAL_HEADER_BOTTOM_GAP = 1.5.sp
val TERMINAL_EVENT_TOP_GAP = 8.25.sp
val TERMINAL_EVENT_BOTTOM_GAP = 1.5.sp
val TERMINAL_FILE_TOP_GAP = 2.sp
val TERMINAL_EMOJI_TOP_GAP = 4.25.sp
val TERMINAL_EMOJI_BOTTOM_GAP = 1.5.sp
val TERMINAL_META_GAP = 6.sp
const val TERMINAL_HEADING_RATIO = 1.3125f
private const val TERMINAL_LINE_HEIGHT_RATIO = 22f / 16f
private const val TERMINAL_LINE_HEIGHT_CONTRAST_SLOPE = 0.1515f
const val INTER_CAP_HEIGHT = 2048f / 2816f
private val TERMINAL_LINE_HEIGHT_STYLE = LineHeightStyle(LineHeightStyle.Alignment.Proportional, LineHeightStyle.Trim.None)

@Composable
fun terminalTextStyle(fontSize: TextUnit, color: Color = messageTextColor()): TextStyle =
  if (LocalTerminalLayout.current) {
    TextStyle(lineHeight = fontSize * (TERMINAL_LINE_HEIGHT_RATIO * contrastLineHeightFactor(color)), lineHeightStyle = TERMINAL_LINE_HEIGHT_STYLE)
  } else {
    TextStyle.Default
  }

@Composable
private fun contrastLineHeightFactor(color: Color): Float {
  val background = MaterialTheme.colors.background
  val textContrast = lightnessContrast(messageTextColor(), background)
  val weight = if (textContrast > 0f) (lightnessContrast(color, background) / textContrast).coerceIn(0f, 1f) else 1f
  return 1 - TERMINAL_LINE_HEIGHT_CONTRAST_SLOPE * (1 - weight)
}

private fun lightnessContrast(color: Color, background: Color): Float =
  abs(lightness(color.compositeOver(background)) - lightness(background))

private fun lightness(color: Color): Float {
  val y = color.luminance()
  return if (y > 216f / 24389f) 116f * y.pow(1f / 3) - 16f else y * 24389f / 27f
}

fun terminalItemShowsSender(item: ChatItem, older: ChatItem?): Boolean =
  item.content.hasTerminalSender && (older == null || !older.content.hasTerminalSender || !sameSender(item, older))

private fun sameSender(item: ChatItem, older: ChatItem): Boolean {
  val d = item.chatDir
  val o = older.chatDir
  return when {
    item.channelIsSender || older.channelIsSender -> item.channelIsSender && older.channelIsSender
    d is CIDirection.GroupRcv -> o is CIDirection.GroupRcv && o.groupMember.memberId == d.groupMember.memberId
    else -> d::class == o::class
  }
}

private val ChatItem.channelIsSender: Boolean get() =
  chatDir is CIDirection.ChannelRcv || (chatDir is CIDirection.GroupSnd && meta.showGroupAsSender)

fun terminalRowGap(item: ChatItem, older: ChatItem?, separated: Boolean, largeGap: Boolean, hasSenderRow: Boolean): TextUnit =
  if (older != null && !separated && (largeGap || hasSenderRow || !item.content.hasTerminalSender || !older.content.hasTerminalSender || item.textAtItemTop && item.startsWithHeading)) TERMINAL_LEVEL_STEP else 0.sp

fun terminalHeadingGap(item: ChatItem): TextUnit =
  if (!item.textAtItemTop && item.startsWithHeading) TERMINAL_LEVEL_STEP else 0.sp

private val ChatItem.textAtItemTop: Boolean get() =
  (content.msgContent is MsgContent.MCText || content.msgContent is MsgContent.MCUnknown && file == null) && !hasQuoteOrHeader

private val ChatItem.startsWithHeading: Boolean get() =
  markdownBlocks?.firstOrNull() is MarkdownBlock.Heading

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

private val LastLineLeft = VerticalAlignmentLine(::min)
private val LastLineRight = VerticalAlignmentLine(::max)
private val LastBlockBottom = HorizontalAlignmentLine(::max)

fun Modifier.messageLastLine(): Modifier = lastLine { emptyMap() }

fun Modifier.messageLastBlock(): Modifier = lastLine { mapOf(LastBlockBottom to it.height) }

private fun Modifier.lastLine(extraAlignmentLines: (Placeable) -> Map<AlignmentLine, Int>): Modifier =
  layout { measurable, constraints ->
    val placeable = measurable.measure(constraints)
    layout(placeable.width, placeable.height, mapOf(LastLineLeft to 0, LastLineRight to placeable.width) + extraAlignmentLines(placeable)) { placeable.placeRelative(0, 0) }
  }

@Composable
fun Modifier.inlineIcon(): Modifier =
  if (LocalTerminalLayout.current) {
    layout { measurable, constraints ->
      val placeable = measurable.measure(constraints)
      layout(placeable.width, 0) { placeable.placeRelative(0, -placeable.height / 2) }
    }
  } else {
    this
  }

@Composable
fun ItemWithMeta(meta: @Composable () -> Unit, overlayMeta: (@Composable () -> Unit)? = null, metaReserved: Boolean = false, endButton: (@Composable () -> Unit)? = null, content: @Composable (inlineMeta: (@Composable () -> Unit)?) -> Unit) {
  if (LocalTerminalLayout.current) {
    TerminalMetaLayout(metaReserved, hasOverlay = overlayMeta != null) {
      content(null)
      listOfNotNull(meta, overlayMeta, endButton).forEach { m ->
        Box(Modifier.padding(end = 12.dp)) {
          m()
        }
      }
    }
  } else {
    content(meta)
  }
}

@Composable
private fun TerminalMetaLayout(metaReserved: Boolean, hasOverlay: Boolean, contentWithMeta: @Composable () -> Unit) {
  Layout(contentWithMeta) { measurables, constraints ->
    val metaConstraints = constraints.copy(minWidth = 0, minHeight = 0)
    val meta = measurables[1].measure(metaConstraints)
    val overlay = if (hasOverlay) measurables[2].measure(metaConstraints) else null
    val endButton = measurables.getOrNull(if (hasOverlay) 3 else 2)?.measure(metaConstraints)
    val besideMeta = !metaReserved && overlay == null
    val reservedWidth = max(if (besideMeta) meta.width else 0, endButton?.width ?: 0)
    val contentMaxWidth = if (constraints.hasBoundedWidth) (constraints.maxWidth - reservedWidth).coerceAtLeast(0) else constraints.maxWidth
    val content = measurables[0].measure(constraints.copy(minWidth = 0, maxWidth = contentMaxWidth))
    val width = if (constraints.hasBoundedWidth) constraints.maxWidth else max(content.width + reservedWidth, meta.width)
    val gap = TERMINAL_META_GAP.toPx().roundToInt()
    val lineEnd = if (layoutDirection == LayoutDirection.Ltr) {
      content[LastLineRight].takeIf { it != AlignmentLine.Unspecified }
    } else {
      content[LastLineLeft].takeIf { it != AlignmentLine.Unspecified }?.let { content.width - it }
    }
    val metaBelow = lineEnd != null && lineEnd + meta.width > width
    val textBaseline = content[LastBaseline]
    val metaBaseline = meta[FirstBaseline].takeIf { it != AlignmentLine.Unspecified } ?: ((meta.height + 12.sp.toPx() * INTER_CAP_HEIGHT) / 2).roundToInt()
    val metaTop = when {
      metaBelow -> content.height + gap
      overlay == null && textBaseline != AlignmentLine.Unspecified -> textBaseline - metaBaseline
      else -> content.height - gap - meta.height
    }.coerceAtLeast(0)
    val blockGap = if (content[LastBlockBottom] == content.height) TERMINAL_BLOCK_BOTTOM_GAP.toPx().roundToInt() else 0
    val placedOverlay = overlay?.takeIf { content.width + meta.width > width }
    val endButtonHeight = endButton?.height ?: 0
    val endButtonTop = (((if (placedOverlay != null) content.height else metaTop) - endButtonHeight) / 2).coerceAtLeast(0)
    layout(width, maxOf(content.height + blockGap, metaTop + meta.height, placedOverlay?.height ?: 0, endButtonTop + endButtonHeight)) {
      content.placeRelative(0, 0)
      if (placedOverlay != null) {
        placedOverlay.placeRelative(content.width - placedOverlay.width, (content.height - gap - placedOverlay.height).coerceAtLeast(0))
      } else {
        meta.placeRelative(width - meta.width, metaTop)
      }
      endButton?.placeRelative(width - endButton.width, endButtonTop)
    }
  }
}

class ChatItemSender(val name: String, val badge: LocalBadge?, val role: String?, val own: Boolean, val image: (@Composable () -> Unit)?)

@Composable
fun chatItemSender(
  chatInfo: ChatInfo,
  cItem: ChatItem,
  range: State<IntRange?>,
  reversedChatItems: State<List<ChatItem>>,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  showChatInfo: () -> Unit
): ChatItemSender {
  val dir = cItem.chatDir
  return when {
    cItem.channelIsSender && chatInfo is ChatInfo.Group ->
      ChatItemSender(chatInfo.chatViewName, null, stringResource(MR.strings.channel_role_label), dir.sent) {
        Box(Modifier.clickable { showChatInfo() }) {
          ProfileImage(MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier, chatInfo.groupInfo.image, chatInfo.groupInfo.chatIconName, backgroundColor = MaterialTheme.colors.background)
        }
      }
    dir is CIDirection.DirectRcv ->
      ChatItemSender(chatInfo.displayName, chatInfo.nameBadge, null, false, null)
    dir is CIDirection.GroupRcv && chatInfo is ChatInfo.Group -> {
      val member = dir.groupMember
      val rangeValue = range.value
      val (prevMember, memCount) = if (rangeValue != null) chatModel.getPrevHiddenMember(member, rangeValue, reversedChatItems.value) else null to 1
      ChatItemSender(
        memberNames(member, prevMember, memCount),
        if (prevMember == null && memCount == 1) member.nameBadge else null,
        if (memCount == 1) memberRole(member, chatInfo.isChannel) else null,
        false
      ) {
        Box(Modifier.clickable { showMemberInfo(chatInfo.groupInfo, member) }) {
          MemberImage(member)
        }
      }
    }
    dir is CIDirection.GroupSnd && chatInfo is ChatInfo.Group -> {
      val membership = chatInfo.groupInfo.membership
      ChatItemSender(membership.displayName, membership.nameBadge, memberRole(membership, chatInfo.isChannel), true) {
        MemberImage(membership)
      }
    }
    chatInfo.incognito ->
      ChatItemSender(stringResource(MR.strings.incognito), null, null, true, null)
    else -> {
      val user = chatModel.currentUser.value
      ChatItemSender(user?.displayName ?: "", user?.profile?.localBadge, null, true, null)
    }
  }
}

private fun memberRole(member: GroupMember, isChannel: Boolean): String? =
  if (member.memberRole > GroupMemberRole.Member) member.memberRole.text(isChannel) else null

@Composable
private fun SenderNameAndRole(sender: ChatItemSender, modifier: Modifier) {
  Row(modifier) {
    NameWithBadge(
      sender.name,
      sender.badge,
      Modifier.weight(1f, false),
      fontSize = 13.5.sp,
      color = if (sender.own) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
      overflow = TextOverflow.Ellipsis,
      maxLines = 1
    )
    if (sender.role != null) {
      Text(
        sender.role,
        Modifier.padding(start = DEFAULT_PADDING_HALF * 1.5f),
        fontSize = 13.5.sp,
        fontWeight = FontWeight.Medium,
        color = MaterialTheme.colors.secondary,
        maxLines = 1
      )
    }
  }
}

@Composable
fun terminalGutter(chatInfo: ChatInfo): Dp =
  if (chatInfo is ChatInfo.Group) MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier + 4.dp else 0.dp

@Composable
fun TerminalChatItemRow(
  sender: ChatItemSender?,
  gutter: Dp,
  topGap: Dp,
  modifier: Modifier = Modifier,
  senderTopGap: Dp = TERMINAL_SENDER_TOP_GAP.toDp(),
  content: @Composable () -> Unit
) {
  Layout(
    content = {
      Box {
        sender?.image?.invoke()
      }
      Box {
        if (sender != null) {
          ProvideTextStyle(LocalTextStyle.current.merge(terminalTextStyle(13.5.sp))) {
            SenderNameAndRole(sender, Modifier.padding(start = CHAT_ITEM_TEXT_PADDING, bottom = TERMINAL_SENDER_BOTTOM_GAP.toDp()))
          }
        }
      }
      Box {
        content()
      }
    },
    modifier
      .padding(top = topGap + if (sender != null) senderTopGap else 0.dp, start = 8.dp, end = 12.dp)
      .fillMaxWidth()
  ) { measurables, constraints ->
    val messageStart = gutter.roundToPx()
    val looseConstraints = constraints.copy(minWidth = 0, minHeight = 0)
    val messageConstraints = looseConstraints.copy(maxWidth = (constraints.maxWidth - messageStart).coerceAtLeast(0))
    val avatar = measurables[0].measure(looseConstraints)
    val name = measurables[1].measure(messageConstraints)
    val message = measurables[2].measure(messageConstraints)
    val messageTop = name.height
    layout(constraints.maxWidth, max(messageTop + message.height, avatar.height)) {
      avatar.placeRelative(0, 0)
      name.placeRelative(messageStart, 0)
      message.placeRelative(messageStart, messageTop)
    }
  }
}
