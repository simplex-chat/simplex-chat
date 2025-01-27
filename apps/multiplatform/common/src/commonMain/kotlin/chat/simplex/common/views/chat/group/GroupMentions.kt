package chat.simplex.common.views.chat.group

import androidx.compose.animation.core.*
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
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
import chat.simplex.common.views.helpers.mentionPickerAnimSpec
import kotlinx.coroutines.flow.distinctUntilChanged

const val MENTION_START = '@'
const val QUOTE = '\''
val GROUP_MENTIONS_MAX_HEIGHT = DEFAULT_MIN_SECTION_ITEM_HEIGHT * 5f

private data class MentionRange(val start: Int, var name: String)
private data class MentionsState(
  val ranges: Map<Int, MentionRange>,
  val activeRange: MentionRange?,
  val mentionMemberOccurrences: Map<String, Int>
)

@Composable
fun GroupMentions(
  rhId: Long?,
  composeState: MutableState<ComposeState>,
  textSelection: MutableState<TextRange>,
  composeViewFocusRequester: FocusRequester?,
  chatInfo: ChatInfo.Group
) {
  val maxHeightInPx = with(LocalDensity.current) { GROUP_MENTIONS_MAX_HEIGHT.toPx() }
  val membersToMention = remember { mutableStateOf<List<GroupMember>>(emptyList()) }
  val mentionsState by remember { derivedStateOf { parseMentionRanges(composeState.value.message, textSelection.value.start) }}
  val showMembersPicker by remember { derivedStateOf { mentionsState.activeRange != null && membersToMention.value.isNotEmpty() }}
  val offsetY = remember { Animatable(maxHeightInPx) }

  LaunchedEffect(mentionsState.activeRange) {
    val activeMentionRange = mentionsState.activeRange
    val search = activeMentionRange?.name?.trim(QUOTE)

    if (search == null) {
      if (membersToMention.value.size == 1 && composeState.value.memberMentionsEnabled) {
        val member = membersToMention.value.first()

        if (composeState.value.mentions.none { it.member.memberId == member.memberId }) {
          val displayName = composeState.value.mentionMemberName(member.displayName)
          composeState.value = composeState.value.copy(
            mentions = composeState.value.mentions.toMutableList().apply {
              add(GroupMemberMention(displayName, member))
            }
          )
        }
      }
      if (membersToMention.value.isNotEmpty()) {
        offsetY.animateTo(
          targetValue = maxHeightInPx,
          animationSpec = mentionPickerAnimSpec(),
        )
        membersToMention.value = emptyList()
      }
      return@LaunchedEffect
    }

    val txtAsMention = composeState.value.mentions.firstOrNull {
      if (it.member.displayName == it.memberName) {
        mentionsState.mentionMemberOccurrences[search] == 1 && search == it.memberName
      } else {
        search == it.memberName
      }
    }
    if (txtAsMention != null) {
      membersToMention.value = listOf(txtAsMention.member)
      return@LaunchedEffect
    }
    // TODO - [MENTIONS] replace with real api
    val gms = chatModel.controller.apiListMembers(rhId, chatInfo.groupInfo.groupId)
    membersToMention.value = gms.filter { gm ->
      gm.displayName.contains(search, ignoreCase = true) && gm.memberStatus != GroupMemberStatus.MemLeft && gm.memberStatus != GroupMemberStatus.MemRemoved
    }
  }

  LaunchedEffect(Unit) {
    snapshotFlow { mentionsState.mentionMemberOccurrences }
      .distinctUntilChanged()
      .collect { mmo ->
        val filteredMentions = composeState.value.mentions.filter { mmo.contains(it.memberName) }

        if (filteredMentions.size != composeState.value.mentions.size) {
          composeState.value = composeState.value.copy(mentions = filteredMentions)
        }
      }
  }

  LaunchedEffect(showMembersPicker) {
    if (offsetY.value != 0f) {
      offsetY.animateTo(
        targetValue = 0f,
        animationSpec = mentionPickerAnimSpec()
      )
    }
  }

  LazyColumn(
    Modifier
      .offset { IntOffset(0, offsetY.value.toInt()) }
      .heightIn(max = DEFAULT_MIN_SECTION_ITEM_HEIGHT * 5f)
      .background(MaterialTheme.colors.surface)
  ) {
    itemsIndexed(membersToMention.value, key = { _, item -> item.groupMemberId }) { _, member ->
      Divider()
      val existingMention = composeState.value.mentions.find { it.member.memberId == member.memberId }
      val enabled = composeState.value.memberMentionsEnabled || existingMention != null
      Row(
        Modifier
          .fillMaxWidth()
          .alpha(if (enabled) 1f else 0.6f)
          .clickable(enabled = enabled) {
            val selection = mentionsState.activeRange ?: return@clickable
            val msg = composeState.value.message
            val displayName = existingMention?.memberName ?: composeState.value.mentionMemberName(member.displayName)
            val mentions = if (existingMention != null) composeState.value.mentions else composeState.value.mentions.toMutableList().apply {
              add(GroupMemberMention(displayName, member))
            }

            val endIndex = selection.start + selection.name.length
            var name = if (displayName.contains(" ")) "'$displayName'" else displayName
            if (endIndex == msg.length) {
              name += " "
            }

            composeState.value = composeState.value.copy(
              message = msg.replaceRange(
                selection.start,
                endIndex,
                name
              ),
              mentions = mentions
            )

            if (appPlatform.isDesktop) {
              // Desktop doesn't auto focus after click, we need to do it manually in here.
              composeViewFocusRequester?.requestFocus()
              textSelection.value = TextRange(composeState.value.message.length)
            }
          }
          .padding(horizontal = DEFAULT_PADDING_HALF),
        verticalAlignment = Alignment.CenterVertically
      ) {
        MemberRow(
          member,
          infoPage = false,
        )
      }
    }
  }
}

private fun parseMentionRanges(message: String, activeSelection: Int): MentionsState {
  val mentionByRange = mutableMapOf<Int, MentionRange>()
  val parsedMentions = mutableMapOf<String, Int>()
  var currentRange: MentionRange? = null

  val addToParseMentions = { n: String ->
    val name = n.trim(QUOTE)
    if (name.isNotEmpty()) {
      val existing = parsedMentions[name]
      if (existing != null) {
        parsedMentions[name] = existing + 1
      } else {
        parsedMentions[name] = 1
      }
    }
  }

  for (i in message.indices) {
    val char = message[i]
    val isInsideQuote = currentRange?.name?.count { it == QUOTE } == 1

    if (isInsideQuote && char == QUOTE && currentRange != null) {
      currentRange.name += char
      mentionByRange[i + 1] = currentRange
      addToParseMentions(currentRange.name)
      currentRange = null
      continue
    }

    if (!isInsideQuote && (char == ' ' || char == '\n')) {
      if (currentRange != null) {
        addToParseMentions(currentRange.name)
        currentRange = null
      }
      continue;
    }

    if (currentRange == null && char == MENTION_START) {
      currentRange = MentionRange(i + 1,"")
      mentionByRange[i + 1] = currentRange
    } else if (currentRange != null) {
      currentRange.name += char
      mentionByRange[i + 1] = currentRange
    }
  }

  if (currentRange != null) {
    mentionByRange[message.length] = currentRange
    addToParseMentions(currentRange.name)
  }

  return MentionsState(mentionByRange, mentionByRange[activeSelection], parsedMentions)
}