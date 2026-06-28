package chat.simplex.common.views.chatlist

import SectionItemViewLongClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.MessageReminder
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.DefaultDropdownMenu
import chat.simplex.common.views.helpers.withBGApi
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import kotlin.time.Duration.Companion.days
import kotlin.time.Duration.Companion.hours
import kotlin.time.Duration.Companion.minutes

@Composable
fun LaterRemindersSection(reminders: List<MessageReminder>) {
  if (reminders.isEmpty()) return
  val scope = rememberCoroutineScope()
  Column(Modifier.fillMaxWidth()) {
    Text(
      stringResource(MR.strings.later_section),
      Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
      fontSize = 14.sp,
      fontWeight = FontWeight.SemiBold,
      color = MaterialTheme.colors.secondary,
    )
    reminders.forEach { reminder ->
      LaterReminderRow(
        reminder = reminder,
        onOpen = {
          scope.launch {
            val chat = chatModel.getChat(reminder.chatId)
            if (chat != null) {
              chatModel.openAroundItemId.value = reminder.itemId
              openChat(
                secondaryChatsCtx = null,
                rhId = chat.remoteHostId,
                chat.chatInfo.chatType,
                chat.chatInfo.apiId,
                openAroundItemId = reminder.itemId,
              )
            }
          }
        },
        onComplete = {
          withBGApi { chatModel.reminderRepository.completeReminder(reminder.id) }
        },
      )
    }
    Divider(Modifier.padding(horizontal = DEFAULT_PADDING))
  }
}

@Composable
private fun LaterReminderRow(
  reminder: MessageReminder,
  onOpen: () -> Unit,
  onComplete: () -> Unit,
) {
  val showMenu = remember { mutableStateOf(false) }
  val dueColor = if (reminder.isOverdue) MaterialTheme.colors.error else MaterialTheme.colors.secondary
  SectionItemViewLongClickable(
    click = onOpen,
    longClick = { showMenu.value = true },
    padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = 8.dp),
  ) {
    Icon(
      painterResource(MR.images.ic_timer),
      contentDescription = null,
      tint = dueColor,
      modifier = Modifier.size(22.dp).padding(end = 10.dp),
    )
    Column(Modifier.weight(1f)) {
      Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
        Text(
          reminder.chatDisplayName.ifEmpty { reminder.chatId },
          Modifier.weight(1f),
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.body1,
          fontWeight = FontWeight.Medium,
        )
        Text(
          reminderDueLabel(reminder),
          fontSize = 12.sp,
          color = dueColor,
        )
      }
      if (reminder.messagePreview.isNotEmpty()) {
        Text(
          reminder.messagePreview,
          maxLines = 2,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.body2,
          color = MaterialTheme.colors.secondary,
        )
      }
    }
  }
  DefaultDropdownMenu(showMenu) {
    ItemAction(stringResource(MR.strings.mark_reminder_complete)) {
      onComplete()
      showMenu.value = false
    }
  }
}

@Composable
private fun reminderDueLabel(reminder: MessageReminder): String {
  if (reminder.isOverdue) {
    return stringResource(MR.strings.reminder_overdue)
  }
  return formatRelativeDue(reminder.dueAt)
}

private fun formatRelativeDue(dueAt: Instant): String {
  val now = Clock.System.now()
  val diff = dueAt - now
  return when {
    diff < 1.minutes -> "now"
    diff < 1.hours -> "${diff.inWholeMinutes}m"
    diff < 1.days -> "${diff.inWholeHours}h"
    else -> "${diff.inWholeDays}d"
  }
}