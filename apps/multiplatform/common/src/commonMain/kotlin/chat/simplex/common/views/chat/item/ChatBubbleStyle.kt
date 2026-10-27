package chat.simplex.common.views.chat.item

import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.CIDirection
import kotlinx.datetime.TimeZone
import kotlinx.datetime.toLocalDateTime
import kotlin.math.abs

data class ChatBubbleGroup(val joinedAbove: Boolean = false, val joinedBelow: Boolean = false)

val LocalChatBubbleGroup = staticCompositionLocalOf { ChatBubbleGroup() }
val LocalTelegramChatStyle = staticCompositionLocalOf { false }

val CHAT_TEXT_INSET = 11.dp
val CHAT_TEXT_TOP_INSET = 5.dp
val CHAT_TEXT_BOTTOM_INSET = 5.dp
val CHAT_META_BOTTOM_INSET = 4.5.dp
val CHAT_SCREEN_INSET = 6.dp
val CHAT_OPPOSITE_INSET = 49.dp
const val CHAT_GROUP_INTERVAL_SECONDS = 5 * 60

@Composable
fun TelegramChatTypography(group: ChatBubbleGroup = ChatBubbleGroup(), content: @Composable () -> Unit) {
  fun TextStyle.chatStyle() = copy(fontFamily = FontFamily.Default, lineHeight = TextUnit.Unspecified, letterSpacing = 0.sp)
  val typography = MaterialTheme.typography
  CompositionLocalProvider(LocalTelegramChatStyle provides true, LocalChatBubbleGroup provides group) {
    MaterialTheme(typography = typography.copy(
      h1 = typography.h1.chatStyle(), h2 = typography.h2.chatStyle(), h3 = typography.h3.chatStyle(),
      h4 = typography.h4.chatStyle(), h5 = typography.h5.chatStyle(), h6 = typography.h6.chatStyle(),
      subtitle1 = typography.subtitle1.chatStyle(), subtitle2 = typography.subtitle2.chatStyle(),
      body1 = typography.body1.chatStyle(), body2 = typography.body2.chatStyle(),
      button = typography.button.chatStyle(), caption = typography.caption.chatStyle(), overline = typography.overline.chatStyle()
    ), content = content)
  }
}

@Composable
fun chatMessageTextStyle(
  fontSize: TextUnit = MaterialTheme.typography.body1.fontSize,
  color: Color = MaterialTheme.colors.onSurface
): TextStyle = MaterialTheme.typography.body1.copy(fontSize = fontSize, color = color, lineHeight = TextUnit.Unspecified, letterSpacing = 0.sp)

@Composable
fun chatReplyFontSize(): TextUnit = ((2 * MaterialTheme.typography.body1.fontSize.value + 10) / 3).sp

fun chatMessagesJoined(item: ChatItem, adjacent: ChatItem?): Boolean {
  if (adjacent == null || item.content.msgContent == null || adjacent.content.msgContent == null ||
    item.meta.itemDeleted != null || adjacent.meta.itemDeleted != null || item.isDeletedContent || adjacent.isDeletedContent ||
    item.isReport != adjacent.isReport || item.chatDir.sent != adjacent.chatDir.sent) return false
  val sameSender = when (val direction = item.chatDir) {
    is CIDirection.GroupRcv -> adjacent.chatDir is CIDirection.GroupRcv && direction.groupMember.groupMemberId == adjacent.chatDir.groupMember.groupMemberId
    is CIDirection.ChannelRcv -> adjacent.chatDir is CIDirection.ChannelRcv
    else -> item.chatDir::class == adjacent.chatDir::class
  }
  val zone = TimeZone.currentSystemDefault()
  return sameSender && abs(item.meta.createdAt.epochSeconds - adjacent.meta.createdAt.epochSeconds) <= CHAT_GROUP_INTERVAL_SECONDS &&
    item.meta.itemTs.toLocalDateTime(zone).date == adjacent.meta.itemTs.toLocalDateTime(zone).date
}