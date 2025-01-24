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
import kotlinx.coroutines.flow.distinctUntilChanged

const val QUOTE_END = "' "
const val QUOTED_MENTION_START = "@'"
const val MENTION_START = "@"
const val QUOTE = '\''
const val MAX_NUMBER_OF_MENTIONS = 3
val GROUP_MENTIONS_MAX_HEIGHT = DEFAULT_MIN_SECTION_ITEM_HEIGHT * 5f

@Composable
fun GroupMentions(
  rhId: Long?,
  composeState: MutableState<ComposeState>,
  textSelection: MutableState<Pair<Int, Int>>,
  chatInfo: ChatInfo.Group) {
  val mentionSelection = remember {
    derivedStateOf {
      val text = composeState.value.message
      parseActiveMentionRange(textSelection.value, text)
    }
  }
  val membersToMention = remember { mutableStateOf<List<GroupMember>>(emptyList()) }
  val mentionSelectionText by remember {
    derivedStateOf {
      val selection = mentionSelection.value
      if (selection != null) {
        if (selection.first == selection.second) {
          ""
        } else {
          composeState.value.message.substring(selection.first, selection.second + 1).trim()
        }
      } else {
        null
      }
    }
  }

  LaunchedEffect(chatInfo.groupInfo.groupId, composeState.value.mentions) {
    snapshotFlow { mentionSelectionText }
      .distinctUntilChanged()
      .collect { txt ->
        if (txt == null) {
          membersToMention.value = emptyList()
          return@collect
        }

        val txtAsMention = composeState.value.mentions.firstOrNull {
          txt == if (it.memberName.contains(" ")) {
            "${QUOTED_MENTION_START}${it.memberName}${QUOTE}"
          } else {
            "${MENTION_START}${it.memberName}"
          }
        }
        if (txtAsMention != null) {
          membersToMention.value = listOf(txtAsMention.member)
          return@collect
        }

        // TODO - [MENTIONS] replace with real api
        val gms = chatModel.controller.apiListMembers(rhId, chatInfo.groupInfo.groupId)
        val search = txt.trim().removePrefix(QUOTED_MENTION_START).removePrefix(MENTION_START).removeSuffix(QUOTE.toString())

        membersToMention.value = gms.filter { gm ->
          gm.displayName.contains(search, ignoreCase = true) && gm.memberStatus != GroupMemberStatus.MemLeft && gm.memberStatus != GroupMemberStatus.MemRemoved
        }
      }
  }

  LaunchedEffect(Unit) {
    snapshotFlow { composeState.value.message }
      .distinctUntilChanged()
      .collect { txt ->
        // TODO - [MENTIONS] review this, checks if mention was removed
        val paddedText = " $txt "
        val filteredMentions = composeState.value.mentions.filter {
          paddedText.contains(" ${MENTION_START}${it.memberName} ") || paddedText.contains(" ${QUOTED_MENTION_START}${it.memberName}${QUOTE_END}")
        }

        if (filteredMentions.size != composeState.value.mentions.size) {
          composeState.value = composeState.value.copy(mentions = filteredMentions.toMutableList())
        }
      }
  }
  val maxHeightInPx = with(LocalDensity.current) { GROUP_MENTIONS_MAX_HEIGHT.toPx() }
  val offsetY = remember { Animatable(maxHeightInPx) }

  LaunchedEffect(mentionSelection.value) {
    if (mentionSelection.value != null) {
      offsetY.animateTo(
        targetValue = 0f,
        animationSpec = tween(durationMillis = 500, easing = FastOutSlowInEasing)
      )
    } else {
      offsetY.animateTo(
        targetValue = maxHeightInPx,
        animationSpec = tween(durationMillis = 500, easing = FastOutSlowInEasing)
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
            val selection = mentionSelection.value ?: return@clickable
            val msg = composeState.value.message
            val nameHasSpaces = member.displayName.contains(' ')
            val displayName = existingMention?.memberName ?: uniqueMentionName(0, member.displayName, composeState.value.mentions)
            val mentions = if (existingMention != null) composeState.value.mentions else composeState.value.mentions.toMutableList().apply {
              add(GroupMemberMention(displayName, member))
            }

            var name =  if (nameHasSpaces) {
              " ${QUOTED_MENTION_START}${displayName}${QUOTE_END}"
            } else " ${MENTION_START}${displayName} "
            if (selection.first == 0) {
              name = name.removePrefix(" ")
            }

            composeState.value = composeState.value.copy(
              message = msg.replaceRange(
                selection.first,
                selection.second + 1,
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

private fun parseActiveMentionRange(textSelection: Pair<Int, Int>, text: String): Pair<Int, Int>? {
  val (start, end) = textSelection
  // Prevents empty spaces, and possible race conditions between text and selection changes.
  if (text.isEmpty() || start > text.length || end > text.length || start == 0 || end == 0) {
    return null
  }

  val leftSide = text.substring(0, start)
  val startOfQuotedMention = leftSide.lastIndexOf(QUOTED_MENTION_START).takeIf { it != -1 }
  val hasOpenQuote = leftSide.count { it == QUOTE } % 2 == 1
  var isQuotedMention = startOfQuotedMention != null && hasOpenQuote

  val startM: Int? = if (isQuotedMention) {
    startOfQuotedMention
  } else {
    val leftSidePrefixed = " $leftSide"
    val lastMention = leftSidePrefixed
      .lastIndexOf(" $MENTION_START")
      .takeIf { it != -1 }?.let { if (it > 0) it - 1 else it }

    if (lastMention == null || leftSide.lastIndexOf(" ") > lastMention) {
      null
    } else {
      lastMention
    }
  }

  if (startM == null) {
    return null
  }

  val rightSide = text.substring(start)

  if (rightSide.startsWith(QUOTE)) {
    isQuotedMention = true
  }

  val rangeMax = if (start == end) text.lastIndex else end
  val endM = minOf(
    start + (rightSide.indexOf(if (isQuotedMention) QUOTE_END else " ").takeIf { it != -1 } ?: rightSide.lastIndex),
    rangeMax
  )

  return startM to endM
}

private fun uniqueMentionName(n: Int, name: String, mentions: List<GroupMemberMention>): String {
  val tryName = if (n == 0) name else "${name}_$n"
  val used = mentions.any { it.memberName == tryName }
  return if (used) uniqueMentionName(n + 1, name, mentions) else tryName
}