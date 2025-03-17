package chat.simplex.common.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.*
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.UriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlin.math.ceil

@Composable
fun FramedItemView(
  chatInfo: ChatInfo,
  ci: ChatItem,
  uriHandler: UriHandler? = null,
  imageProvider: (() -> ImageGalleryProvider)? = null,
  linkMode: SimplexLinkMode,
  showViaProxy: Boolean,
  showMenu: MutableState<Boolean>,
  showTimestamp: Boolean,
  tailVisible: Boolean = false,
  receiveFile: (Long) -> Unit,
  onLinkLongClick: (link: String) -> Unit = {},
  scrollToItem: (Long) -> Unit = {},
  scrollToQuotedItemFromItem: (Long) -> Unit = {},
) {
  val sent = ci.chatDir.sent
  val chatTTL = chatInfo.timedMessagesTTL

  fun membership(): GroupMember? {
    return if (chatInfo is ChatInfo.Group) chatInfo.groupInfo.membership else null
  }

  @Composable
  fun ciQuotedMsgTextView(qi: CIQuote, lines: Int, showTimestamp: Boolean) {
    MarkdownText(
      qi.text,
      qi.formattedText,
      toggleSecrets = true,
      maxLines = lines,
      overflow = TextOverflow.Ellipsis,
      style = TextStyle(fontSize = 15.sp, color = MaterialTheme.colors.onSurface),
      linkMode = linkMode,
      uriHandler = if (appPlatform.isDesktop) uriHandler else null,
      showTimestamp = showTimestamp,
    )
  }

  @Composable
  fun ciQuotedMsgView(qi: CIQuote) {
    Box(
      Modifier
        // this width limitation prevents crash on calculating constraints that may happen if you post veeeery long message and then quote it.
        // Top level layout wants `IntrinsicWidth.Max` and very long layout makes the crash in this case
        .widthIn(max = 50000.dp)
        .padding(vertical = 6.dp, horizontal = 12.dp),
      contentAlignment = Alignment.TopStart
    ) {
      val sender = qi.sender(membership())
      if (sender != null) {
        Column(
          horizontalAlignment = Alignment.Start
        ) {
          Text(
            sender,
            style = TextStyle(fontSize = 13.5.sp, color = if (qi.chatDir is CIDirection.GroupSnd) CurrentColors.value.colors.primary else CurrentColors.value.colors.secondary),
            maxLines = 1
          )
          ciQuotedMsgTextView(qi, lines = 2,  showTimestamp = showTimestamp)
        }
      } else {
        ciQuotedMsgTextView(qi, lines = 3,  showTimestamp = showTimestamp)
      }
    }
  }

  @Composable
  fun FramedItemHeader(caption: String, italic: Boolean, icon: Painter? = null, pad: Boolean = false, iconColor: Color? = null) {
    val sentColor = MaterialTheme.appColors.sentQuote
    val receivedColor = MaterialTheme.appColors.receivedQuote
    Row(
      Modifier
        .background(if (sent) sentColor else receivedColor)
        .fillMaxWidth()
        .padding(start = 8.dp, top = 6.dp, end = 12.dp, bottom = if (pad || (ci.quotedItem == null && ci.meta.itemForwarded == null)) 6.dp else 0.dp),
      horizontalArrangement = Arrangement.spacedBy(4.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      if (icon != null) {
        Icon(
          icon,
          caption,
          Modifier.size(18.dp),
          tint = iconColor ?: if (isInDarkTheme()) FileDark else FileLight
        )
      }
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(fontSize = 12.sp, fontStyle = if (italic) FontStyle.Italic else FontStyle.Normal, color = MaterialTheme.colors.secondary)) {
            append(caption)
          }
        },
        style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
    }
  }

  @Composable
  fun ciQuoteView(qi: CIQuote) {
    val sentColor = MaterialTheme.appColors.sentQuote
    val receivedColor = MaterialTheme.appColors.receivedQuote
    Row(
      Modifier
        .background(if (sent) sentColor else receivedColor)
        .fillMaxWidth()
    ) {
      when (qi.content) {
        is MsgContent.MCImage -> {
          Box(Modifier.fillMaxWidth().weight(1f)) {
            ciQuotedMsgView(qi)
          }
          val imageBitmap = base64ToBitmap(qi.content.image)
          Image(
            imageBitmap,
            contentDescription = stringResource(MR.strings.image_descr),
            contentScale = ContentScale.Crop,
            modifier = Modifier.size(68.dp).clipToBounds()
          )
        }
        is MsgContent.MCVideo -> {
          Box(Modifier.fillMaxWidth().weight(1f)) {
            ciQuotedMsgView(qi)
          }
          val imageBitmap = base64ToBitmap(qi.content.image)
          Image(
            imageBitmap,
            contentDescription = stringResource(MR.strings.video_descr),
            contentScale = ContentScale.Crop,
            modifier = Modifier.size(68.dp).clipToBounds()
          )
        }
        is MsgContent.MCFile, is MsgContent.MCVoice -> {
          Box(Modifier.fillMaxWidth().weight(1f)) {
            ciQuotedMsgView(qi)
          }
          Icon(
            if (qi.content is MsgContent.MCFile) painterResource(MR.images.ic_draft_filled) else painterResource(MR.images.ic_mic_filled),
            if (qi.content is MsgContent.MCFile) stringResource(MR.strings.icon_descr_file) else stringResource(MR.strings.voice_message),
            Modifier
              .padding(top = 6.dp, end = 4.dp)
              .size(22.dp),
            tint = if (isInDarkTheme()) FileDark else FileLight
          )
        }
        else -> ciQuotedMsgView(qi)
      }
    }
  }

  @Composable
  fun ciFileView(ci: ChatItem, text: String) {
    CIFileView(ci.file, ci.meta.itemEdited, showMenu, false, receiveFile)
    if (text != "" || ci.meta.isLive) {
      CIMarkdownText(ci, chatInfo, chatTTL, linkMode = linkMode, uriHandler, showViaProxy = showViaProxy,  showTimestamp = showTimestamp)
    }
  }

  val transparentBackground = (ci.content.msgContent is MsgContent.MCImage || ci.content.msgContent is MsgContent.MCVideo) &&
      !ci.meta.isLive && ci.content.text.isEmpty() && ci.quotedItem == null && ci.meta.itemForwarded == null

  val sentColor = MaterialTheme.appColors.sentMessage
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Box(Modifier
    .clipChatItem(ci, tailVisible, revealed = true)
    .background(
      when {
        transparentBackground -> Color.Transparent
        sent -> sentColor
        else -> receivedColor
      }
    )) {
    var metaColor = MaterialTheme.colors.secondary
    Box(contentAlignment = Alignment.BottomEnd) {
      val chatItemTail = remember { appPreferences.chatItemTail.state }
      val style = shapeStyle(ci, chatItemTail.value, tailVisible, revealed = true)
      val tailRendered = style is ShapeStyle.Bubble && style.tailVisible
      Column(
        Modifier
          .width(IntrinsicSize.Max)
          .padding(start = if (!sent && tailRendered) msgTailWidthDp else 0.dp, end = if (sent && tailRendered) msgTailWidthDp else 0.dp)
      ) {
        PriorityLayout(Modifier, CHAT_IMAGE_LAYOUT_ID) {
          @Composable
          fun Header() {
            if (ci.isReport) {
              if (ci.meta.itemDeleted == null) {
                FramedItemHeader(
                  stringResource(if (ci.chatDir.sent) MR.strings.report_item_visibility_submitter else MR.strings.report_item_visibility_moderators),
                  true,
                  painterResource(MR.images.ic_flag),
                  iconColor = Color.Red
                )
              } else {
                val text = if (ci.meta.itemDeleted is CIDeleted.Moderated && ci.meta.itemDeleted.byGroupMember.groupMemberId != (chatInfo as ChatInfo.Group?)?.groupInfo?.membership?.groupMemberId) {
                  stringResource(MR.strings.report_item_archived_by).format(ci.meta.itemDeleted.byGroupMember.displayName)
                } else {
                  stringResource(MR.strings.report_item_archived)
                }
                FramedItemHeader(text, true, painterResource(MR.images.ic_flag))
              }
            } else if (ci.meta.itemDeleted != null) {
              when (ci.meta.itemDeleted) {
                is CIDeleted.Moderated -> {
                  FramedItemHeader(String.format(stringResource(MR.strings.moderated_item_description), ci.meta.itemDeleted.byGroupMember.chatViewName), true, painterResource(MR.images.ic_flag))
                }
                is CIDeleted.Blocked -> {
                  FramedItemHeader(stringResource(MR.strings.blocked_item_description), true, painterResource(MR.images.ic_back_hand))
                }
                is CIDeleted.BlockedByAdmin -> {
                  FramedItemHeader(stringResource(MR.strings.blocked_by_admin_item_description), true, painterResource(MR.images.ic_back_hand))
                }
                is CIDeleted.Deleted -> {
                  FramedItemHeader(stringResource(MR.strings.marked_deleted_description), true, painterResource(MR.images.ic_delete))
                }
              }
            } else if (ci.meta.isLive) {
              FramedItemHeader(stringResource(MR.strings.live), false)
            }
          }
          if (ci.quotedItem != null) {
            Column(
              Modifier
                .combinedClickable(
                  onLongClick = { showMenu.value = true },
                  onClick = {
                    if (ci.quotedItem.itemId != null) {
                      scrollToItem(ci.quotedItem.itemId)
                    } else {
                      scrollToQuotedItemFromItem(ci.id)
                    }
                  }
                )
                .onRightClick { showMenu.value = true }
            ) {
              Header()
              ciQuoteView(ci.quotedItem)
            }
          } else {
            Header()
            if (ci.meta.itemForwarded != null) {
              FramedItemHeader(ci.meta.itemForwarded.text(chatInfo.chatType), true, painterResource(MR.images.ic_forward), pad = true)
            }
          }
          if (ci.file == null && ci.formattedText == null && !ci.meta.isLive && isShortEmoji(ci.content.text)) {
            Box(Modifier.padding(vertical = 6.dp, horizontal = 12.dp)) {
              Column(
                Modifier
                  .padding(bottom = 2.dp)
                  .fillMaxWidth(),
                horizontalAlignment = Alignment.CenterHorizontally
              ) {
                EmojiText(ci.content.text)
                Text("")
              }
            }
          } else {
            when (val mc = ci.content.msgContent) {
              is MsgContent.MCImage -> {
                CIImageView(image = mc.image, file = ci.file, imageProvider ?: return@PriorityLayout, showMenu, false, receiveFile)
                if (mc.text == "" && !ci.meta.isLive) {
                  metaColor = Color.White
                } else {
                  CIMarkdownText(ci, chatInfo, chatTTL, linkMode, uriHandler, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
                }
              }
              is MsgContent.MCVideo -> {
                CIVideoView(image = mc.image, mc.duration, file = ci.file, imageProvider ?: return@PriorityLayout, showMenu, smallView = false, receiveFile = receiveFile)
                if (mc.text == "" && !ci.meta.isLive) {
                  metaColor = Color.White
                } else {
                  CIMarkdownText(ci, chatInfo, chatTTL, linkMode, uriHandler, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
                }
              }
              is MsgContent.MCVoice -> {
                CIVoiceView(mc.duration, ci.file, ci.meta.itemEdited, ci.chatDir.sent, hasText = true, ci, timedMessagesTTL = chatTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp, longClick = { onLinkLongClick("") }, receiveFile = receiveFile)
                if (mc.text != "") {
                  CIMarkdownText(ci, chatInfo, chatTTL, linkMode, uriHandler, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
                }
              }
              is MsgContent.MCFile -> ciFileView(ci, mc.text)
              is MsgContent.MCUnknown ->
                if (ci.file == null) {
                  CIMarkdownText(ci, chatInfo, chatTTL, linkMode, uriHandler, onLinkLongClick, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
                } else {
                  ciFileView(ci, mc.text)
                }
              is MsgContent.MCLink -> {
                ChatItemLinkView(mc.preview, showMenu, onLongClick = { showMenu.value = true })
                Box(Modifier.widthIn(max = DEFAULT_MAX_IMAGE_WIDTH)) {
                  CIMarkdownText(ci, chatInfo, chatTTL, linkMode, uriHandler, onLinkLongClick, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
                }
              }
              is MsgContent.MCReport -> {
                val prefix = buildAnnotatedString {
                  withStyle(SpanStyle(color = Color.Red, fontStyle = FontStyle.Italic)) {
                    append(if (mc.text.isEmpty()) mc.reason.text else "${mc.reason.text}: ")
                  }
                }
                CIMarkdownText(ci, chatInfo, chatTTL, linkMode, uriHandler, onLinkLongClick, showViaProxy = showViaProxy, showTimestamp = showTimestamp, prefix = prefix)
              }
              else -> CIMarkdownText(ci, chatInfo, chatTTL, linkMode, uriHandler, onLinkLongClick, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
            }
          }
        }
      }
      Box(
        Modifier
          .padding(
            bottom = 6.dp,
            end = 12.dp + if (tailRendered && sent) msgTailWidthDp else 0.dp,
          )
      ) {
        CIMetaView(ci, chatTTL, metaColor, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
      }
    }
  }
}

@Composable
fun CIMarkdownText(
  ci: ChatItem,
  chatInfo: ChatInfo,
  chatTTL: Int?,
  linkMode: SimplexLinkMode,
  uriHandler: UriHandler?,
  onLinkLongClick: (link: String) -> Unit = {},
  showViaProxy: Boolean,
  showTimestamp: Boolean,
  prefix: AnnotatedString? = null
) {
  Box(Modifier.padding(vertical = 7.dp, horizontal = 12.dp)) {
    val text = if (ci.meta.isLive) ci.content.msgContent?.text ?: ci.text else ci.text
    MarkdownText(
      text, if (text.isEmpty()) emptyList() else ci.formattedText, toggleSecrets = true,
      meta = ci.meta, chatTTL = chatTTL, linkMode = linkMode,
      mentions = ci.mentions, userMemberId = when {
        chatInfo is ChatInfo.Group -> chatInfo.groupInfo.membership.memberId
        else -> null
      },
      uriHandler = uriHandler, senderBold = true, onLinkLongClick = onLinkLongClick, showViaProxy = showViaProxy, showTimestamp = showTimestamp, prefix = prefix
    )
  }
}

const val CHAT_IMAGE_LAYOUT_ID = "chatImage"
const val CHAT_BUBBLE_LAYOUT_ID = "chatBubble"
const val CHAT_COMPOSE_LAYOUT_ID = "chatCompose"
const val CONSOLE_COMPOSE_LAYOUT_ID = "consoleCompose"

/**
 * Compose shows "Can't represent a width of ... and height ... in Constraints" even when using built-in method for measuring max
 * available size. It seems like padding around such layout prevents showing them in parent layout when such child layouts are placed.
 * So calculating the expected padding here based on the values Compose printed in the exception (removing some pixels from
 * [Constraints.fitPrioritizingHeight] result makes it working well)
*/
private fun horizontalPaddingAroundCustomLayouts(density: Float): Int =
  // currently, it's 18. Doubling it just to cover possible changes in the future
  36 * ceil(density).toInt()

@Composable
fun PriorityLayout(
  modifier: Modifier = Modifier,
  priorityLayoutId: String,
  content: @Composable () -> Unit
) {
  Layout(
    content = content,
    modifier = modifier
  ) { measureable, constraints ->
    // Find important element which should tell what max width other elements can use
    // Expecting only one such element. Can be less than one but not more
    val imagePlaceable = measureable.firstOrNull { it.layoutId == priorityLayoutId }?.measure(constraints)
    val placeables: List<Placeable> = measureable.map {
      if (it.layoutId == priorityLayoutId)
        imagePlaceable!!
      else
        it.measure(constraints.copy(maxWidth = imagePlaceable?.width ?: constraints.maxWidth)) }
    // Limit width for every other element to width of important element and height for a sum of all elements.
    val width = imagePlaceable?.measuredWidth ?: placeables.maxOf { it.width }
    val height = placeables.sumOf { it.height }
    val adjustedConstraints = Constraints.fitPrioritizingHeight(constraints.minWidth, width, constraints.minHeight, height)
    layout(
      if (width > adjustedConstraints.maxWidth) adjustedConstraints.maxWidth - horizontalPaddingAroundCustomLayouts(density) else adjustedConstraints.maxWidth,
      adjustedConstraints.maxHeight
    ) {
      var y = 0
      placeables.forEach {
        it.place(0, y)
        y += it.measuredHeight
      }
    }
  }
}

@Composable
fun DependentLayout(
  modifier: Modifier = Modifier,
  mainLayoutId: String,
  content: @Composable () -> Unit
) {
  Layout(
    content = content,
    modifier = modifier
  ) { measureable, constraints ->
    // Find important element which should tell what min width it needs to draw itself.
    // Expecting only one such element. Can be less than one but not more
    val mainPlaceable = measureable.firstOrNull { it.layoutId == mainLayoutId }?.measure(constraints)
    val placeables: List<Placeable> = measureable.map {
      if (it.layoutId == mainLayoutId)
        mainPlaceable!!
      else
        it.measure(constraints.copy(minWidth = mainPlaceable?.width ?: 0, maxWidth = constraints.maxWidth)) }
    val width = mainPlaceable?.measuredWidth ?: placeables.maxOf { it.width }
    val height = placeables.sumOf { it.height }
    val adjustedConstraints = Constraints.fitPrioritizingHeight(constraints.minWidth, width, constraints.minHeight, height)
    layout(
      if (width > adjustedConstraints.maxWidth) adjustedConstraints.maxWidth - horizontalPaddingAroundCustomLayouts(density) else adjustedConstraints.maxWidth,
      adjustedConstraints.maxHeight
    ) {
      var y = 0
      placeables.forEach {
        it.place(0, y)
        y += it.measuredHeight
      }
    }
  }
}

// The purpose of this layout is to make measuring of bottom compose view and adapt top lazy column to its size in the same frame (not on the next frame as you would expect).
// So, steps are:
// - measuring the layout: measured height of compose view before this step is 0, it's added to content padding of lazy column (so it's == 0)
// - measured the layout: measured height of compose view now is correct, but it's not yet applied to lazy column content padding (so it's == 0) and lazy column is placed higher than compose view in view with respect to compose view's height
// - on next frame measured height is correct and content padding is the same, lazy column placed to occupy all parent view's size
// - every added/removed line in compose view goes through the same process.
@Composable
fun AdaptingBottomPaddingLayout(
  modifier: Modifier = Modifier,
  mainLayoutId: String,
  expectedHeight: MutableState<Dp>,
  content: @Composable () -> Unit
) {
  val expected = with(LocalDensity.current) { expectedHeight.value.roundToPx() }
  Layout(
    content = content,
    modifier = modifier
  ) { measureable, constraints ->
    require(measureable.size <= 2) { "Should be exactly one or two elements in this layout, you have ${measureable.size}" }
    val mainPlaceable = measureable.firstOrNull { it.layoutId == mainLayoutId }!!.measure(constraints)
    val placeables: List<Placeable> = measureable.map {
      if (it.layoutId == mainLayoutId)
        mainPlaceable
      else
        it.measure(constraints.copy(maxHeight = if (expected != mainPlaceable.measuredHeight) constraints.maxHeight - mainPlaceable.measuredHeight + expected else constraints.maxHeight)) }
    expectedHeight.value = mainPlaceable.measuredHeight.toDp()
    layout(constraints.maxWidth, constraints.maxHeight) {
      var y = 0
      placeables.forEach {
        if (it !== mainPlaceable) {
          it.place(0, y)
          y += it.measuredHeight
        } else {
          it.place(0, constraints.maxHeight - mainPlaceable.measuredHeight)
          y += it.measuredHeight
        }
      }
    }
  }
}

@Composable
fun CenteredRowLayout(
  modifier: Modifier = Modifier,
  content: @Composable () -> Unit
) {
  Layout(
    content = content,
    modifier = modifier
  ) { measureable, constraints ->
    require(measureable.size == 3) { "Should be exactly three elements in this layout, you have ${measureable.size}" }
    val first = measureable[0].measure(constraints.copy(minWidth = 0, minHeight = 0))
    val third = measureable[2].measure(constraints.copy(minWidth = first.measuredWidth, minHeight = 0))
    val second = measureable[1].measure(constraints.copy(minWidth = 0, minHeight = 0, maxWidth = (constraints.maxWidth - first.measuredWidth - third.measuredWidth).coerceAtLeast(0)))
    // Limit width for every other element to width of important element and height for a sum of all elements.
    layout(constraints.maxWidth, constraints.maxHeight) {
      first.place(0, ((constraints.maxHeight - first.measuredHeight) / 2).coerceAtLeast(0))
      second.place((constraints.maxWidth - second.measuredWidth) / 2, ((constraints.maxHeight - second.measuredHeight) / 2).coerceAtLeast(0))
      third.place(constraints.maxWidth - third.measuredWidth, ((constraints.maxHeight - third.measuredHeight) / 2).coerceAtLeast(0))
    }
  }
}

fun showQuotedItemDoesNotExistAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.message_deleted_or_not_received_error_title),
    text = generalGetString(MR.strings.message_deleted_or_not_received_error_desc)
  )
}

/*

class EditedProvider: PreviewParameterProvider<Boolean> {
  override val values = listOf(false, true).asSequence()
}

@Preview
@Composable
fun PreviewTextItemViewSnd(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello", itemEdited = edited,
      ),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewRcv(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectRcv(), Clock.System.now(), "hello", itemEdited = edited
      ),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewLong(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1,
        CIDirection.DirectSnd(),
        Clock.System.now(),
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
        itemEdited = edited
      ),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewQuote(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "https://simplex.chat",
        CIStatus.SndSent(),
        quotedItem = CIQuote.getSample(1, Clock.System.now(), "hi", chatDir = CIDirection.DirectRcv()),
        itemEdited = edited
      ),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewTextItemViewEmoji(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "👍",
        CIStatus.SndSent(),
        quotedItem = CIQuote.getSample(1, Clock.System.now(), "Lorem ipsum dolor sit amet", chatDir = CIDirection.DirectRcv()),
        itemEdited = edited
      ),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewQuoteWithTextAndImage(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val ciQuote = CIQuote(
    chatDir = CIDirection.DirectRcv(), itemId = 1, sentAt = Clock.System.now(),
    content = MsgContent.MCImage(
      text = "Hello there",
      image = "data:image/jpg;base64,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"
    )
  )
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "Hello there",
        CIStatus.SndSent(),
        quotedItem = ciQuote,
        itemEdited = edited
      ),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewQuoteWithLongTextAndImage(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val ciQuote = CIQuote(
    chatDir = CIDirection.DirectRcv(), itemId = 1, sentAt = Clock.System.now(),
    content = MsgContent.MCImage(
      text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
      image = "data:image/jpg;base64,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"
    )
  )
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "Hello there",
        CIStatus.SndSent(),
        quotedItem = ciQuote,
        itemEdited = edited
      ),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewQuoteWithLongTextAndFile(@PreviewParameter(EditedProvider::class) edited: Boolean) {
  val ciQuote = CIQuote(
    chatDir = CIDirection.DirectRcv(), itemId = 1, sentAt = Clock.System.now(),
    content = MsgContent.MCFile(
      text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    )
  )
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(),
        Clock.System.now(),
        "Hello there",
        CIStatus.SndSent(),
        quotedItem = ciQuote,
        itemEdited = edited
      ),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}
*/
