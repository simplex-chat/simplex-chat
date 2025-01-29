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
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.views.helpers.mentionPickerAnimSpec
import chat.simplex.res.MR
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch

const val MENTION_START = '@'
const val QUOTE = '\''
private val MAX_PICKER_HEIGHT = DEFAULT_MIN_SECTION_ITEM_HEIGHT * 5f

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
  val maxHeightInPx = with(LocalDensity.current) { windowHeight().toPx() }
  val membersToMention = remember { mutableStateOf<List<GroupMember>>(emptyList()) }
  val showMembersPicker = remember { mutableStateOf(false) }
  val offsetY = remember { Animatable(maxHeightInPx) }
  val mentionsState by remember { derivedStateOf { parseMentionRanges(composeState.value.message, textSelection.value.start) } }
  val scope = rememberCoroutineScope()

  suspend fun closeMembersPicker() {
    showMembersPicker.value = false
    if (offsetY.value != 0f) {
      return
    }

    offsetY.animateTo(
      targetValue = maxHeightInPx,
      animationSpec = mentionPickerAnimSpec()
    )
  }

  LaunchedEffect(mentionsState.activeRange) {
    val activeMentionRange = mentionsState.activeRange
    val search = activeMentionRange?.name?.trim(QUOTE)

    if (search == null) {
      // If we don't call it sync in here the panel will close due to no results before showMembersPicker animation.
      closeMembersPicker()
      if (membersToMention.value.size == 1 && !composeState.value.maxMemberMentionsReached) {
        val member = membersToMention.value.first()

        if (composeState.value.mentions.none { it.member.memberId == member.memberId }) {
          val displayName = composeState.value.mentionMemberName(member.memberProfile.displayName)
          composeState.value = composeState.value.copy(
            mentions = composeState.value.mentions.toMutableList().apply {
              add(MemberMention(displayName, member))
            }
          )
        }
      }
      if (membersToMention.value.isNotEmpty()) {
        membersToMention.value = emptyList()
      }
      return@LaunchedEffect
    }
    val txtAsMention = composeState.value.mentions.firstOrNull {
      if (it.member.memberProfile.displayName == it.memberName) {
        mentionsState.mentionMemberOccurrences[search] == 1 && search == it.memberName
      } else {
        search == it.memberName
      }
    }
    if (txtAsMention != null) {
      showMembersPicker.value = true
      membersToMention.value = listOf(txtAsMention.member)
      return@LaunchedEffect
    }
    // TODO - [MENTIONS] replace with real api
    val gms = chatModel.controller.apiListMembers(rhId, chatInfo.groupInfo.groupId)
    membersToMention.value = gms.filter { gm ->
      gm.memberProfile.displayName.contains(search, ignoreCase = true) && gm.memberStatus != GroupMemberStatus.MemLeft && gm.memberStatus != GroupMemberStatus.MemRemoved
    }
    if (membersToMention.value.isNotEmpty()) {
      showMembersPicker.value = true
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

  LaunchedEffect(showMembersPicker.value) {
    if (showMembersPicker.value) {
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
    val mentionsLookup = composeState.value.mentions.associateBy { it.member.memberId }
    val showMaxReachedBox = composeState.value.maxMemberMentionsReached &&
        showMembersPicker.value &&
        membersToMention.value.any { it.memberId !in mentionsLookup }

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
      itemsIndexed(membersToMention.value, key = { _, item -> item.groupMemberId }) { i, member ->
        if (i != 0 || !showMaxReachedBox) {
          Divider()
        }
        val existingMention = composeState.value.mentions.find { it.member.memberId == member.memberId }
        val enabled = !composeState.value.maxMemberMentionsReached || existingMention != null
        Row(
          Modifier
            .fillMaxWidth()
            .alpha(if (enabled) 1f else 0.6f)
            .clickable(enabled = enabled) {
              val selection = mentionsState.activeRange ?: return@clickable
              showMembersPicker.value = false
              val msg = composeState.value.message
              val displayName = existingMention?.memberName ?: composeState.value.mentionMemberName(member.memberProfile.displayName)
              val mentions = if (existingMention != null) composeState.value.mentions else composeState.value.mentions.toMutableList().apply {
                add(MemberMention(displayName, member))
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
      continue
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
