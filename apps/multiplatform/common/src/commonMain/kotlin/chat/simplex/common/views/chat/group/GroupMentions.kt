package chat.simplex.common.views.chat.group

import SectionDivider
import SectionItemView
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.ui.theme.DEFAULT_SPACE_AFTER_ICON
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chatlist.setGroupMembers
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.flow.filterNotNull

const val WORD_SEPARATOR = " "
const val QUOTE_END = "' "
const val QUOTED_MENTION_START = "@'"
const val MENTION_START = "@"
const val QUOTE = '\''
const val MAX_NUMBER_OF_MENTIONS = 3

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
          composeState.value.message.substring(selection.first, selection.second + 1)
        }
      } else {
        null
      }
    }
  }

  LaunchedEffect(chatInfo.groupInfo.groupId) {
    snapshotFlow { mentionSelectionText }
      .distinctUntilChanged()
      .collect { txt ->
        if (txt != null) {
          // TODO - [MENTIONS] replace with real api
          val gms = chatModel.controller.apiListMembers(rhId, chatInfo.groupInfo.groupId)
          val search = txt.trim().removePrefix(QUOTED_MENTION_START).removePrefix(MENTION_START).removeSuffix(QUOTE.toString())

          membersToMention.value = gms.filter { gm ->
            gm.displayName.contains(search, ignoreCase = true) && gm.memberStatus != GroupMemberStatus.MemLeft && gm.memberStatus != GroupMemberStatus.MemRemoved
          }
        } else if (membersToMention.value.isNotEmpty()) {
          membersToMention.value = emptyList()
        }
      }
  }

  LaunchedEffect(Unit) {
    snapshotFlow { composeState.value.message }
      .distinctUntilChanged()
      .collect { txt ->
        // TODO - [MENTIONS] review this, checks if mention was removed
        val filteredMentions = composeState.value.mentions.filter { txt.contains("@${it.usedName}") }

        if (filteredMentions.size != composeState.value.mentions.size) {
          composeState.value = composeState.value.copy(mentions = filteredMentions.toMutableList())
        }
      }
  }

  val selection = mentionSelection.value
  if (selection != null && composeState.value.mentions.size < MAX_NUMBER_OF_MENTIONS) {
    LazyColumn(
      Modifier.background(MaterialTheme.colors.surface),
    ) {
      itemsIndexed(membersToMention.value, key = { _, item -> item.groupMemberId }) { _, member ->
        Divider()
        Row(
          Modifier
            .fillMaxWidth()
            .clickable {
              val msg = composeState.value.message
              val nameHasSpaces = member.displayName.contains(' ')
              val existingMention = composeState.value.mentions.find { it.member.groupMemberId == member.groupMemberId }
              val displayName = existingMention?.usedName ?: uniqueMentionName(0, member.displayName, composeState.value.mentions)

              composeState.value = composeState.value.copy(
                message = msg.replaceRange(
                  selection.first,
                  selection.second + 1,
                  if (nameHasSpaces) "@'${displayName}' " else "@${displayName} "
                ),
                mentions = if (existingMention != null) composeState.value.mentions else composeState.value.mentions.toMutableList().apply {
                  add(MentionMember(displayName, member))
                }
              )
            }
            .padding(horizontal = DEFAULT_PADDING_HALF)
          ,
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
    val lastMention = leftSide.lastIndexOf(MENTION_START).takeIf { it != -1 }
    if (lastMention == null || leftSide.lastIndexOf(WORD_SEPARATOR) > lastMention) {
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
    start + (rightSide.indexOf(if (isQuotedMention) QUOTE_END else WORD_SEPARATOR).takeIf { it != -1 } ?: rightSide.lastIndex),
    rangeMax
  )

  return startM to endM
}

private fun uniqueMentionName(n: Int, name: String, mentions: List<MentionMember>): String {
  val tryName = if (n == 0) name else "${name}_$n"
  val used = mentions.any { it.usedName == tryName }
  return if (used) uniqueMentionName(n + 1, name, mentions) else tryName
}