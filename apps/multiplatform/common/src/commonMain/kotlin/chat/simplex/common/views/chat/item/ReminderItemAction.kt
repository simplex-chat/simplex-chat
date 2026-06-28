package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.padding
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.ReminderPreset
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.DefaultDropdownMenu
import chat.simplex.common.views.helpers.withBGApi
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun RemindMessageItemActions(
  showMenu: MutableState<Boolean>,
  cInfo: ChatInfo,
  cItem: ChatItem,
  live: Boolean,
) {
  val showRemindPresets = remember { mutableStateOf(false) }
  val canRemind =
    cItem.meta.itemDeleted == null &&
      !live &&
      !cItem.isLiveDummy &&
      cItem.id >= 0

  if (canRemind) {
    ItemAction(
      stringResource(MR.strings.remind_me),
      painterResource(MR.images.ic_timer),
      onClick = {
        showRemindPresets.value = true
        showMenu.value = false
      }
    )
  }

  RemindMessagePresetsMenu(
    showMenu = showRemindPresets,
    cInfo = cInfo,
    cItem = cItem,
  )
}

@Composable
private fun RemindMessagePresetsMenu(
  showMenu: MutableState<Boolean>,
  cInfo: ChatInfo,
  cItem: ChatItem,
) {
  DefaultDropdownMenu(showMenu) {
    Text(
      stringResource(MR.strings.later_section),
      Modifier.padding(vertical = DEFAULT_PADDING_HALF, horizontal = DEFAULT_PADDING * 1.5f),
      fontSize = 16.sp,
      color = MaterialTheme.colors.secondary
    )
    ItemAction(stringResource(MR.strings.remind_in_1_hour)) {
      createMessageReminder(cInfo, cItem, ReminderPreset.In1Hour)
      showMenu.value = false
    }
    ItemAction(stringResource(MR.strings.remind_in_3_hours)) {
      createMessageReminder(cInfo, cItem, ReminderPreset.In3Hours)
      showMenu.value = false
    }
    ItemAction(stringResource(MR.strings.remind_tomorrow)) {
      createMessageReminder(cInfo, cItem, ReminderPreset.Tomorrow)
      showMenu.value = false
    }
    ItemAction(stringResource(MR.strings.remind_next_week)) {
      createMessageReminder(cInfo, cItem, ReminderPreset.NextWeek)
      showMenu.value = false
    }
  }
}

private fun createMessageReminder(cInfo: ChatInfo, cItem: ChatItem, preset: ReminderPreset) {
  withBGApi {
    chatModel.reminderRepository.createReminder(
      cInfo.id,
      cItem.id,
      preset,
      cItem.text(cInfo.isChannel),
      cInfo.displayName,
    )
  }
}