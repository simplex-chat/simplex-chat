package chat.simplex.common.model

import chat.simplex.common.platform.MessageReminderScheduler
import chat.simplex.common.platform.chatController
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.datetime.Clock
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json

class MessageReminderRepository {
  private val json = Json { ignoreUnknownKeys = true }
  private val _reminders = MutableStateFlow<List<MessageReminder>>(emptyList())
  val reminders: StateFlow<List<MessageReminder>> = _reminders.asStateFlow()

  val activeLaterReminders: List<MessageReminder>
    get() = _reminders.value.filter { !it.isComplete }.sortedBy { it.dueAt }

  fun load() {
    val raw = chatController.appPrefs.messageReminders.get() ?: "[]"
    _reminders.value = runCatching {
      json.decodeFromString(ListSerializer(MessageReminder.serializer()), raw)
    }.getOrElse { emptyList() }
    MessageReminderScheduler.rescheduleAll(_reminders.value.filter { !it.isComplete })
  }

  private fun persist() {
    val encoded = json.encodeToString(ListSerializer(MessageReminder.serializer()), _reminders.value)
    chatController.appPrefs.messageReminders.set(encoded)
  }

  fun createReminder(
    chatId: ChatId,
    itemId: Long,
    preset: ReminderPreset,
    messagePreview: String,
    chatDisplayName: String,
    timeZone: kotlinx.datetime.TimeZone = kotlinx.datetime.TimeZone.currentSystemDefault(),
  ): MessageReminder? {
    val userId = ChatModel.currentUser.value?.userId ?: return null
    val now = Clock.System.now()
    val reminder = MessageReminder(
      id = newMessageReminderId(),
      userId = userId,
      chatId = chatId,
      itemId = itemId,
      dueAt = preset.dueAt(now, timeZone),
      createdAt = now,
      messagePreview = messagePreview.take(200),
      chatDisplayName = chatDisplayName,
    )
    _reminders.update { it + reminder }
    persist()
    MessageReminderScheduler.schedule(reminder)
    return reminder
  }

  fun completeReminder(reminderId: String) {
    val now = Clock.System.now()
    _reminders.update { list ->
      list.map { if (it.id == reminderId && !it.isComplete) it.copy(completedAt = now) else it }
    }
    persist()
    MessageReminderScheduler.cancel(reminderId)
    MessageReminderScheduler.cancelNotification(reminderId)
  }

  fun deleteReminder(reminderId: String) {
    _reminders.update { list -> list.filter { it.id != reminderId } }
    persist()
    MessageReminderScheduler.cancel(reminderId)
    MessageReminderScheduler.cancelNotification(reminderId)
  }

  fun reminderById(id: String): MessageReminder? = _reminders.value.firstOrNull { it.id == id }
}