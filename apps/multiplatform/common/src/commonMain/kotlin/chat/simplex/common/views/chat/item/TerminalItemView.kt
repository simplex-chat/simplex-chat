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
import androidx.compose.ui.layout.Measured
import androidx.compose.ui.layout.Placeable
import androidx.compose.ui.layout.VerticalAlignmentLine
import androidx.compose.ui.layout.layout
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.offset
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.appPlatform
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
val LocalBubbleLayout = staticCompositionLocalOf { false }

val TERMINAL_BLOCK_TOP_GAP = 5.75.sp
val TERMINAL_BLOCK_BOTTOM_GAP = 5.sp
val TERMINAL_LEVEL_STEP = 4.75.sp
val TERMINAL_HEADING_BOTTOM_GAP = TERMINAL_LEVEL_STEP / 2
val TERMINAL_PARAGRAPH_GAP = 16.sp * CHAT_LINE_HEIGHT_RATIO / 3
val TERMINAL_HEADER_TOP_GAP = 2.5.sp
val TERMINAL_HEADER_BOTTOM_GAP = 1.5.sp
val TERMINAL_META_GAP = 6.sp
const val TERMINAL_NAME_REPEAT_SECONDS = 450
const val TERMINAL_HEADING_RATIO = 1.25f
private const val CHAT_LINE_HEIGHT_RATIO = 22f / 16f
private const val CHAT_LINE_HEIGHT_CONTRAST_SLOPE = 0.1515f
const val INTER_CAP_HEIGHT = 2048f / 2816f
const val INTER_LINE_HEIGHT = 3408f / 2816f
private val CHAT_LINE_HEIGHT_STYLE = LineHeightStyle(LineHeightStyle.Alignment.Proportional, LineHeightStyle.Trim.None)

@Composable
fun usesChatTypography(): Boolean = LocalTerminalLayout.current || LocalBubbleLayout.current

@Composable
fun chatTextStyle(fontSize: TextUnit, color: Color = messageTextColor()): TextStyle =
  if (usesChatTypography()) {
    val lineHeight = fontSize * (CHAT_LINE_HEIGHT_RATIO * contrastLineHeightFactor(color))
    TextStyle(lineHeight = lineHeight, lineHeightStyle = CHAT_LINE_HEIGHT_STYLE).merge(bubbleLineHeight(lineHeight))
  } else {
    TextStyle.Default
  }

@Composable
fun bubbleTextStyle(fontSize: TextUnit, color: Color): TextStyle =
  if (LocalBubbleLayout.current) chatTextStyle(fontSize, color) else TextStyle.Default

@Composable
fun bubbleLineHeight(lineHeight: TextUnit): TextStyle =
  if (LocalBubbleLayout.current && appPlatform.isDesktop) {
    TextStyle(lineHeight = with(LocalDensity.current) { lineHeight.toPx().roundToInt().toSp() }, lineHeightStyle = CHAT_LINE_HEIGHT_STYLE)
  } else {
    TextStyle.Default
  }

@Composable
private fun contrastLineHeightFactor(color: Color): Float {
  val background = MaterialTheme.colors.background
  val textContrast = lightnessContrast(messageTextColor(), background)
  val weight = if (textContrast > 0f) (lightnessContrast(color, background) / textContrast).coerceIn(0f, 1f) else 1f
  return 1 - CHAT_LINE_HEIGHT_CONTRAST_SLOPE * (1 - weight)
}

private fun lightnessContrast(color: Color, background: Color): Float =
  abs(lightness(color.compositeOver(background)) - lightness(background))

private fun lightness(color: Color): Float {
  val y = color.luminance()
  return if (y > 216f / 24389f) 116f * y.pow(1f / 3) - 16f else y * 24389f / 27f
}

fun terminalItemShowsSender(item: ChatItem, older: ChatItem?): Boolean =
  item.content.hasTerminalSender && (older == null || !older.content.hasTerminalSender || !sameSender(item, older) || afterPause(item, older))

private fun sameSender(item: ChatItem, older: ChatItem): Boolean {
  val d = item.chatDir
  val o = older.chatDir
  return when {
    item.channelIsSender || older.channelIsSender -> item.channelIsSender && older.channelIsSender
    d is CIDirection.GroupRcv -> o is CIDirection.GroupRcv && o.groupMember.memberId == d.groupMember.memberId
    else -> d::class == o::class
  }
}

private fun afterPause(item: ChatItem, older: ChatItem): Boolean =
  item.meta.itemTs.epochSeconds - older.meta.itemTs.epochSeconds >= TERMINAL_NAME_REPEAT_SECONDS

private val ChatItem.channelIsSender: Boolean get() =
  chatDir is CIDirection.ChannelRcv || (chatDir is CIDirection.GroupSnd && meta.showGroupAsSender)

fun terminalRowGap(item: ChatItem, older: ChatItem?, separated: Boolean, largeGap: Boolean, hasSenderRow: Boolean): TextUnit =
  if (older != null && !separated && (largeGap || hasSenderRow || !item.isOneLine || !older.isOneLine || older.reactions.isNotEmpty())) TERMINAL_PARAGRAPH_GAP else 0.sp

fun terminalTextTop(item: ChatItem): TextUnit = when {
  item.cardAboveText || item.hasQuoteOrHeader && item.startsWithCard -> if (item.startsWithEmptyLines) 0.sp else TERMINAL_PARAGRAPH_GAP
  item.hasQuoteOrHeader && item.startsWithHeading -> TERMINAL_LEVEL_STEP
  else -> 0.sp
}

fun terminalCardTop(item: ChatItem): TextUnit =
  if (item.hasQuoteOrHeader) TERMINAL_PARAGRAPH_GAP else 0.sp

fun terminalQuoteTop(item: ChatItem): TextUnit =
  if (item.isReport || item.meta.itemDeleted != null || item.meta.isLive) TERMINAL_PARAGRAPH_GAP else 0.sp

fun terminalReactionsGap(item: ChatItem): TextUnit =
  if (item.reactions.isNotEmpty()) TERMINAL_PARAGRAPH_GAP else 0.sp

val ChatItem.isOneLine: Boolean get() =
  textAtItemTop && markdownBlocks == null && '\n' !in content.text

private val ChatItem.textAtItemTop: Boolean get() =
  textOnly && !hasQuoteOrHeader

private val ChatItem.cardAboveText: Boolean get() =
  quotedItem != null || !textOnly

private val ChatItem.textOnly: Boolean get() =
  content.msgContent is MsgContent.MCText || content.msgContent is MsgContent.MCReport || content.msgContent is MsgContent.MCUnknown && file == null

private val ChatItem.startsWithHeading: Boolean get() =
  markdownBlocks?.firstOrNull() is MarkdownBlock.Heading

private val ChatItem.startsWithCard: Boolean get() =
  markdownBlocks?.firstOrNull()?.isCard == true

private val ChatItem.startsWithEmptyLines: Boolean get() {
  val fts = formattedText
  val blocks = markdownBlocks
  return if (fts != null && blocks != null) leadingEmptyLines(fts, blocks) > 0 else !isReport && (content.text.startsWith('\n') || content.text.startsWith("\r\n"))
}

val CIContent.hasTerminalSender: Boolean get() = when (this) {
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
private val CardTop = HorizontalAlignmentLine(::min)
private val CardBottom = HorizontalAlignmentLine(::max)

fun Modifier.messageLastLine(): Modifier = marked { mapOf(LastLineLeft to 0, LastLineRight to it.width) }

fun Modifier.messageCard(): Modifier = marked { mapOf(CardTop to 0, CardBottom to it.height) }

private fun Modifier.marked(alignmentLines: (Placeable) -> Map<AlignmentLine, Int>): Modifier =
  layout { measurable, constraints ->
    val placeable = measurable.measure(constraints)
    layout(placeable.width, placeable.height, alignmentLines(placeable)) { placeable.placeRelative(0, 0) }
  }

@Composable
fun Modifier.inlineIcon(): Modifier =
  if (usesChatTypography()) {
    layout { measurable, constraints ->
      val placeable = measurable.measure(constraints)
      layout(placeable.width, 0) { placeable.placeRelative(0, -placeable.height / 2) }
    }
  } else {
    this
  }

@Composable
fun ItemWithMeta(meta: @Composable () -> Unit, overlayMeta: (@Composable () -> Unit)? = null, metaReserved: Boolean = false, endButton: (@Composable () -> Unit)? = null, gapBelowCard: Dp = 0.dp, content: @Composable (inlineMeta: (@Composable () -> Unit)?) -> Unit) {
  if (LocalTerminalLayout.current) {
    TerminalMetaLayout(metaReserved, hasOverlay = overlayMeta != null, gapBelowCard) {
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
private fun TerminalMetaLayout(metaReserved: Boolean, hasOverlay: Boolean, gapBelowCard: Dp, contentWithMeta: @Composable () -> Unit) {
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
    val metaBaseline = metaBaseline(meta)
    val metaTop = when {
      metaBelow -> content.height + gap
      overlay == null && textBaseline != AlignmentLine.Unspecified -> textBaseline - metaBaseline
      else -> content.height - gap - meta.height
    }.coerceAtLeast(0)
    val cardGap = if (content[CardBottom] == content.height) gapBelowCard.roundToPx() else 0
    val placedOverlay = overlay?.takeIf { content.width + meta.width > width }
    val endButtonHeight = endButton?.height ?: 0
    val endButtonTop = (((if (placedOverlay != null) content.height else metaTop) - endButtonHeight) / 2).coerceAtLeast(0)
    layout(width, maxOf(content.height + cardGap, metaTop + meta.height, placedOverlay?.height ?: 0, endButtonTop + endButtonHeight)) {
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

fun Density.metaBaseline(meta: Measured): Int =
  meta[FirstBaseline].takeIf { it != AlignmentLine.Unspecified } ?: ((meta.measuredHeight + 12.sp.toPx() * INTER_CAP_HEIGHT) / 2).roundToInt()

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
  val imageSize = TERMINAL_MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier
  return when {
    cItem.channelIsSender && chatInfo is ChatInfo.Group ->
      ChatItemSender(chatInfo.chatViewName, null, stringResource(MR.strings.channel_role_label), dir.sent) {
        Box(Modifier.clickable { showChatInfo() }) {
          ProfileImage(imageSize, chatInfo.groupInfo.image, chatInfo.groupInfo.chatIconName, backgroundColor = MaterialTheme.colors.background, toEdge = true)
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
          MemberImage(member, imageSize, toEdge = true)
        }
      }
    }
    dir is CIDirection.GroupSnd && chatInfo is ChatInfo.Group -> {
      val membership = chatInfo.groupInfo.membership
      ChatItemSender(membership.displayName, membership.nameBadge, memberRole(membership, chatInfo.isChannel), true) {
        MemberImage(membership, imageSize, toEdge = true)
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
      fontSize = MaterialTheme.typography.body1.fontSize,
      fontWeight = FontWeight.SemiBold,
      color = if (sender.own) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
      overflow = TextOverflow.Ellipsis,
      maxLines = 1
    )
    if (sender.role != null) {
      Text(
        sender.role,
        Modifier.padding(start = DEFAULT_PADDING_HALF * 1.5f),
        fontSize = MaterialTheme.typography.body1.fontSize,
        fontWeight = FontWeight.Medium,
        color = MaterialTheme.colors.secondary,
        maxLines = 1
      )
    }
  }
}

val TERMINAL_MEMBER_IMAGE_SIZE: Dp = 27.dp

@Composable
fun terminalGutter(chatInfo: ChatInfo): Dp =
  if (chatInfo is ChatInfo.Group) TERMINAL_MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier + CHAT_ITEM_TEXT_PADDING else 0.dp

@Composable
fun TerminalChatItemRow(
  sender: ChatItemSender?,
  gutter: Dp,
  topGap: Dp,
  modifier: Modifier = Modifier,
  content: @Composable () -> Unit
) {
  val cardGap = TERMINAL_PARAGRAPH_GAP.toDp()
  val avatarGap = if (sender?.image != null) TERMINAL_HEADING_BOTTOM_GAP.toDp() else 0.dp
  Layout(
    content = {
      Box {
        sender?.image?.invoke()
      }
      Box {
        if (sender != null) {
          ProvideTextStyle(LocalTextStyle.current.merge(chatTextStyle(MaterialTheme.typography.body1.fontSize))) {
            SenderNameAndRole(sender, Modifier.padding(start = CHAT_ITEM_TEXT_PADDING))
          }
        }
      }
      Box {
        content()
      }
    },
    modifier.fillMaxWidth()
  ) { measurables, constraints ->
    val nameStart = gutter.roundToPx()
    val looseConstraints = constraints.copy(minWidth = 0, minHeight = 0)
    val avatar = measurables[0].measure(looseConstraints)
    val name = measurables[1].measure(looseConstraints.copy(maxWidth = (constraints.maxWidth - nameStart).coerceAtLeast(0)))
    val message = measurables[2].measure(looseConstraints)
    val avatarOffset = (name.height - avatar.height) / 2
    val nameTop = max(topGap.roundToPx(), -avatarOffset)
    val avatarTop = nameTop + avatarOffset
    val gapBelowSender = if (sender != null && message[CardTop] == 0) cardGap else avatarGap
    val messageTop = max(nameTop + name.height, avatarTop + avatar.height) + gapBelowSender.roundToPx()
    layout(constraints.maxWidth, messageTop + message.height) {
      avatar.placeRelative(CHAT_ITEM_TEXT_PADDING.roundToPx(), avatarTop)
      name.placeRelative(nameStart, nameTop)
      message.placeRelative(0, messageTop)
    }
  }
}
