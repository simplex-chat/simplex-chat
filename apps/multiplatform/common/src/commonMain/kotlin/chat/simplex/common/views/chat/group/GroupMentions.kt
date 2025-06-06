package chat.simplex.common.views.chat.group

import androidx.compose.animation.core.*
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chatlist.setGroupMembers
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.launch
import kotlin.text.CharCategory.*

val punctuation = setOf(
  DASH_PUNCTUATION, START_PUNCTUATION, END_PUNCTUATION,
  CONNECTOR_PUNCTUATION, OTHER_PUNCTUATION
)

private val PICKER_ROW_SIZE = MEMBER_ROW_AVATAR_SIZE + (MEMBER_ROW_VERTICAL_PADDING * 2f)
private val MAX_PICKER_HEIGHT = (PICKER_ROW_SIZE * 4) + (MEMBER_ROW_AVATAR_SIZE + MEMBER_ROW_VERTICAL_PADDING - 4.dp)

@Composable
fun GroupMentions(
  rhId: Long?,
  composeState: MutableState<ComposeState>,
  composeViewFocusRequester: FocusRequester?,
  chatInfo: ChatInfo.Group
) {
  val maxHeightInPx = with(LocalDensity.current) { windowHeight().toPx() }
  val isVisible = remember { mutableStateOf(false) }
  val offsetY = remember { Animatable(maxHeightInPx) }

  val currentMessage = remember { mutableStateOf(composeState.value.message) }
  val mentionName = remember { mutableStateOf("") }
  val mentionRange = remember { mutableStateOf<TextRange?>(null) }
  val mentionMemberId = remember { mutableStateOf<String?>(null) }
  val filteredMembers = remember {
    derivedStateOf {
      val members = chatModel.groupMembers.value
        .filter {
          val status = it.memberStatus
          status != GroupMemberStatus.MemLeft && status != GroupMemberStatus.MemRemoved && status != GroupMemberStatus.MemInvited
        }
        .sortedByDescending { it.memberRole }

      if (mentionName.value.isEmpty()) {
        members
      } else {
        members.filter { it.memberProfile.anyNameContains(mentionName.value) }
      }
    }
  }
  val scope = rememberCoroutineScope()

  suspend fun closeMembersPicker() {
    isVisible.value = false
    if (offsetY.value != 0f) {
      return
    }

    offsetY.animateTo(
      targetValue = maxHeightInPx,
      animationSpec = mentionPickerAnimSpec()
    )
    mentionName.value = ""
    mentionRange.value = null
    mentionMemberId.value = null
  }

  fun messageChanged(msg: ComposeMessage, parsedMsg: List<FormattedText>) {
    removeUnusedMentions(composeState, parsedMsg)
    val selected = selectedMarkdown(parsedMsg, msg.selection)

    if (selected != null) {
      val (ft, r) = selected

      when (ft.format) {
        is Format.Mention -> {
          isVisible.value = true
          mentionName.value = ft.format.memberName
          mentionRange.value = r
          mentionMemberId.value = composeState.value.mentions[mentionName.value]?.memberId
          if (!chatModel.membersLoaded.value) {
            scope.launch {
              setGroupMembers(rhId, chatInfo.groupInfo, chatModel)
            }
          }
          return
        }
        null -> {
          val pos = msg.selection.start
          if (msg.selection.length == 0 && getCharacter(msg.text, pos - 1)?.first == "@") {
            val prevChar = getCharacter(msg.text, pos - 2)?.first
            if (prevChar == null || prevChar == " " || prevChar == "\n") {
              isVisible.value = true
              mentionName.value = ""
              mentionRange.value = TextRange(pos - 1, pos)
              mentionMemberId.value = null
              scope.launch {
                setGroupMembers(rhId, chatInfo.groupInfo, chatModel)
              }
              return
            }
          }
        }
        else -> {}
      }
    }
    scope.launch {
      closeMembersPicker()
    }
  }

  fun addMemberMention(member: GroupMember, range: TextRange) {
    val mentions = composeState.value.mentions.toMutableMap()
    val existingMention = mentions.entries.firstOrNull {
      it.value.memberId == member.memberId
    }
    val newName = existingMention?.key ?: composeState.value.mentionMemberName(member.memberProfile.displayName)
    mentions[newName] = CIMention(member)
    var msgMention = if (newName.contains(" ") || (newName.lastOrNull()?.category in punctuation))
                      "@'$newName'"
                      else "@$newName"
    var newPos = range.start + msgMention.length
    val newMsgLength = composeState.value.message.text.length + msgMention.length - range.length
    if (newPos == newMsgLength) {
      msgMention += " "
      newPos += 1
    }

    val msg = composeState.value.message.text.replaceRange(
      range.start,
      range.end,
      msgMention
    )
    composeState.value = composeState.value.copy(
      message = ComposeMessage(msg, TextRange(newPos)),
      parsedMessage = parseToMarkdown(msg) ?: FormattedText.plain(msg),
      mentions = mentions
    )

    composeViewFocusRequester?.requestFocus()

    scope.launch {
      closeMembersPicker()
    }
  }

  LaunchedEffect(composeState.value.parsedMessage) {
    currentMessage.value = composeState.value.message
    messageChanged(currentMessage.value, composeState.value.parsedMessage)
  }

//  KeyChangeEffect(composeState.value.message.selection) {
//    // This condition is needed to prevent messageChanged called twice,
//    // because composeState.formattedText triggers later when message changes.
//    // The condition is only true if position changed without text change
//    if (currentMessage.value.text == composeState.value.message.text) {
//      messageChanged(currentMessage.value, composeState.value.parsedMessage)
//    }
//  }

  LaunchedEffect(isVisible.value) {
    if (isVisible.value) {
      offsetY.animateTo(
        targetValue = 0f,
        animationSpec = mentionPickerAnimSpec()
      )
    }
  }
  Box(
    modifier = Modifier
      .fillMaxSize()
      .offset { IntOffset(0, offsetY.value.toInt()) }
      .clickable(indication = null, interactionSource = remember { MutableInteractionSource() }) {
        scope.launch { closeMembersPicker() }
      },
    contentAlignment = Alignment.BottomStart
  ) {
    val showMaxReachedBox = composeState.value.mentions.size >= MAX_NUMBER_OF_MENTIONS && isVisible.value && composeState.value.mentions[mentionName.value] == null
    LazyColumnWithScrollBarNoAppBar(
      Modifier
        .heightIn(max = MAX_PICKER_HEIGHT)
        .background(MaterialTheme.colors.surface),
      maxHeight = remember { mutableStateOf(MAX_PICKER_HEIGHT) },
      containerAlignment = Alignment.BottomEnd
    ) {
      if (showMaxReachedBox) {
        stickyHeader {
          MaxMentionsReached()
        }
      }
      itemsIndexed(filteredMembers.value, key = { _, item -> item.groupMemberId }) { i, member ->
        if (i != 0 || !showMaxReachedBox) {
          Divider()
        }
        val mentioned = mentionMemberId.value == member.memberId
        val disabled = composeState.value.mentions.size >= MAX_NUMBER_OF_MENTIONS && !mentioned
        Row(
          Modifier
            .fillMaxWidth()
            .alpha(if (disabled) 0.6f else 1f)
            .clickable(enabled = !disabled) {
              val range = mentionRange.value ?: return@clickable
              val mentionMemberValue = mentionMemberId.value

              if (mentionMemberValue != null) {
                if (mentionMemberValue != member.memberId) {
                  addMemberMention(member, range)
                } else {
                  return@clickable
                }
              } else {
                addMemberMention(member, range)
              }
            }
            .padding(horizontal = DEFAULT_PADDING_HALF),
          verticalAlignment = Alignment.CenterVertically
        ) {
          MemberRow(
            member,
            infoPage = false,
            showlocalAliasAndFullName = true,
            selected = mentioned
          )
        }
      }
    }
  }
}

@Composable
private fun MaxMentionsReached() {
  Column(Modifier.background(MaterialTheme.colors.surface)) {
    Divider()
    Row(
      Modifier.fillMaxWidth(),
      verticalAlignment = Alignment.CenterVertically,
    ) {
      Text(
        String.format(generalGetString(MR.strings.max_group_mentions_per_message_reached), MAX_NUMBER_OF_MENTIONS),
        Modifier.padding(12.dp),
      )
    }
    Divider()
  }
}

private fun getCharacter(s: String, pos: Int): Pair<CharSequence, IntRange>? {
  return if (pos in s.indices) {
    val char = s.subSequence(pos, pos + 1)
    char to (pos until pos + 1)
  } else {
    null
  }
}

private fun selectedMarkdown(
  parsedMsg: List<FormattedText>,
  range: TextRange
): Pair<FormattedText, TextRange>? {
  if (parsedMsg.isEmpty()) return null

  var i = 0
  var pos = 0

  while (i < parsedMsg.size && pos + parsedMsg[i].text.length < range.start) {
    pos += parsedMsg[i].text.length
    i++
  }

  return if (i >= parsedMsg.size || range.end > pos + parsedMsg[i].text.length) {
    null
  } else {
    parsedMsg[i] to TextRange(pos, pos + parsedMsg[i].text.length)
  }
}

private fun removeUnusedMentions(composeState: MutableState<ComposeState>, parsedMsg: List<FormattedText>) {
  val usedMentions = parsedMsg.mapNotNull { ft ->
    when (ft.format) {
      is Format.Mention -> ft.format.memberName
      else -> null
    }
  }.toSet()

  if (usedMentions.size < composeState.value.mentions.size) {
    composeState.value = composeState.value.copy(
      mentions = composeState.value.mentions.filterKeys { it in usedMentions }
    )
  }
}
