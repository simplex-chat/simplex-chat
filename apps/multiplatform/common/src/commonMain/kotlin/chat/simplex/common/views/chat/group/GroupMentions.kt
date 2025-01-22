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
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.chatlist.setGroupMembers
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.flow.filterNotNull

@Composable
fun GroupMentions(
  rhId: Long?,
  composeState: MutableState<ComposeState>,
  textSelection: MutableState<Pair<Int, Int>>,
  chatInfo: ChatInfo.Group) {
  val mentionSelection = remember {
    derivedStateOf {
      val (start, end) = textSelection.value
      val text = composeState.value.message
      if (text.isEmpty() || start > text.length || end > text.length || start == 0 || end == 0) {
        null
      } else {
        val range = if (start == end) {
          val lastSpaceIndex = text.substring(0, start).lastIndexOf(' ')
          val rangeStart = if (lastSpaceIndex != -1) minOf(lastSpaceIndex + 1, text.lastIndex) else 0
          rangeStart to start
        } else {
          start to end
        }

        if (text[range.first] == '@') {
          range
        } else {
          null
        }
      }
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
          composeState.value.message.substring(selection.first + 1, selection.second)
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
          membersToMention.value = gms.filter { gm ->
            gm.displayName.contains(txt, ignoreCase = true) && composeState.value.mentions.none { m -> m.groupMemberId == gm.groupMemberId }
          }
        } else {
          membersToMention.value = emptyList()
        }
      }
  }

  LaunchedEffect(Unit) {
    snapshotFlow { composeState.value.message }
      .distinctUntilChanged()
      .collect { txt ->
        // TODO - [MENTIONS] review this, checks if mention was removed
        val filteredMentions = composeState.value.mentions.filter { txt.contains("@${it.displayName}") }

        if (filteredMentions.size != composeState.value.mentions.size) {
          composeState.value = composeState.value.copy(mentions = filteredMentions.toMutableList())
        }
      }
  }
  val selection = mentionSelection.value
  if (membersToMention.value.isNotEmpty() && selection != null) {
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
              val lastSpaceIndex = msg.indexOf(' ', selection.first).takeIf { it != -1 } ?: msg.length

              composeState.value = composeState.value.copy(
                message = msg.replaceRange(
                  selection.first,
                  lastSpaceIndex,
                  "@${member.displayName} "
                ),
                mentions = composeState.value.mentions.toMutableList().apply {
                  add(member)
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
