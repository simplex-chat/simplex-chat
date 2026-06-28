//
//  ReminderStore.swift
//  SimpleX
//
//  Client-local message reminders (issue #7135).
//

import Foundation
import Combine
import SimpleXChat

final class ReminderStore: ObservableObject {
    static let shared = ReminderStore()

    @Published private(set) var reminders: [MessageReminder] = []

    var activeLaterReminders: [MessageReminder] {
        reminders.filter { !$0.isComplete }.sorted { $0.dueAt < $1.dueAt }
    }

    private init() {}

    func load() {
        let raw = groupDefaults.string(forKey: GROUP_DEFAULT_MESSAGE_REMINDERS) ?? "[]"
        reminders = (try? messageReminderJsonDecoder.decode([MessageReminder].self, from: Data(raw.utf8))) ?? []
        NtfManager.shared.rescheduleMessageReminders(activeLaterReminders)
    }

    private func persist() {
        guard let data = try? messageReminderJsonEncoder.encode(reminders),
              let encoded = String(data: data, encoding: .utf8) else { return }
        groupDefaults.set(encoded, forKey: GROUP_DEFAULT_MESSAGE_REMINDERS)
        groupDefaults.synchronize()
    }

    @discardableResult
    func createReminder(
        chatId: ChatId,
        itemId: Int64,
        preset: ReminderPreset,
        messagePreview: String,
        chatDisplayName: String
    ) -> MessageReminder? {
        guard let userId = ChatModel.shared.currentUser?.userId else { return nil }
        let now = Date.now
        let reminder = MessageReminder(
            id: newMessageReminderId(),
            userId: userId,
            chatId: chatId,
            itemId: itemId,
            dueAt: preset.dueAt(from: now),
            createdAt: now,
            completedAt: nil,
            messagePreview: String(messagePreview.prefix(200)),
            chatDisplayName: chatDisplayName
        )
        reminders.append(reminder)
        persist()
        NtfManager.shared.scheduleMessageReminder(reminder)
        return reminder
    }

    func completeReminder(reminderId: String) {
        let now = Date.now
        var changed = false
        reminders = reminders.map {
            guard $0.id == reminderId, !$0.isComplete else { return $0 }
            changed = true
            var copy = $0
            copy.completedAt = now
            return copy
        }
        if changed {
            persist()
            NtfManager.shared.cancelMessageReminder(reminderId)
        }
    }

    func reminderById(_ id: String) -> MessageReminder? {
        reminders.first { $0.id == id }
    }

    func remindersForCurrentUser() -> [MessageReminder] {
        guard let userId = ChatModel.shared.currentUser?.userId else { return [] }
        return activeLaterReminders.filter { $0.userId == userId }
    }
}
