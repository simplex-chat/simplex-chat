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
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.mentionPickerAnimSpec
import kotlinx.coroutines.flow.distinctUntilChanged

const val MENTION_START = '@'
const val QUOTE = '\''
const val MAX_NUMBER_OF_MENTIONS = 3
val GROUP_MENTIONS_MAX_HEIGHT = DEFAULT_MIN_SECTION_ITEM_HEIGHT * 5f

private data class MentionRange(val start: Int, var name: String)

@Composable
fun GroupMentions(
  rhId: Long?,
  composeState: MutableState<ComposeState>,
  textSelection: MutableState<Pair<Int, Int>>,
  chatInfo: ChatInfo.Group
) {
  val maxHeightInPx = with(LocalDensity.current) { GROUP_MENTIONS_MAX_HEIGHT.toPx() }
  val membersToMention = remember { mutableStateOf<List<GroupMember>>(emptyList()) }
  val allMentionRanges by remember { derivedStateOf { parseMentionRanges(composeState.value.message) }}
  val activeMentionRange by remember { derivedStateOf { allMentionRanges.first[textSelection.value.first] }}
  val showMembersPicker by remember { derivedStateOf { activeMentionRange != null && membersToMention.value.isNotEmpty() }}
  val offsetY = remember { Animatable(maxHeightInPx) }

  LaunchedEffect(activeMentionRange) {
    val search = activeMentionRange?.name?.trim(QUOTE)
    if (search == null) {
      if (membersToMention.value.isNotEmpty()) {
        offsetY.animateTo(
          targetValue = maxHeightInPx,
          animationSpec = mentionPickerAnimSpec(),
        )
        membersToMention.value = emptyList()
      }
      return@LaunchedEffect
    }

    println("ranges: ${activeMentionRange}")
    val txtAsMention = composeState.value.mentions.firstOrNull {
      if (it.member.displayName == it.memberName) {
        allMentionRanges.second[search] == 1 && search == it.memberName
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
    snapshotFlow { allMentionRanges }
      .distinctUntilChanged()
      .collect { (_, m) ->
        // TODO - [MENTIONS] review this, checks if mention was removed
        val filteredMentions = composeState.value.mentions.filter { m.contains(it.memberName) }

        if (filteredMentions.size != composeState.value.mentions.size) {
          composeState.value = composeState.value.copy(mentions = filteredMentions.toMutableList())
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
      val enabled = composeState.value.mentions.size < MAX_NUMBER_OF_MENTIONS || existingMention != null
      Row(
        Modifier
          .fillMaxWidth()
          .alpha(if (enabled) 1f else 0.6f)
          .clickable(enabled = enabled) {
            val selection = activeMentionRange ?: return@clickable
            val msg = composeState.value.message
            val displayName = existingMention?.memberName ?: uniqueMentionName(0, member.displayName, composeState.value.mentions)
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

private fun parseMentionRanges(message: String): Pair<Map<Int, MentionRange>, Map<String, Int>> {
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

  return mentionByRange to parsedMentions
}

private fun uniqueMentionName(n: Int, name: String, mentions: List<GroupMemberMention>): String {
  val tryName = if (n == 0) name else "${name}_$n"
  val used = mentions.any { it.memberName == tryName }
  return if (used) uniqueMentionName(n + 1, name, mentions) else tryName
}