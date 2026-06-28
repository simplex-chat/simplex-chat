//
//  LaterRemindersSection.swift
//  SimpleX
//
//  Message reminders list section (issue #7135).
//

import SwiftUI
import SimpleXChat

struct LaterRemindersSection: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var reminderStore: ReminderStore
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true
    let reminders: [MessageReminder]

    var body: some View {
        Section {
            ForEach(reminders) { reminder in
                LaterReminderRow(reminder: reminder, oneHandUI: oneHandUI)
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    .listRowBackground(Color.clear)
                    .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                        Button {
                            reminderStore.completeReminder(reminderId: reminder.id)
                        } label: {
                            SwipeLabel(
                                NSLocalizedString("Mark complete", comment: "swipe action"),
                                systemImage: "checkmark.circle.fill",
                                inverted: oneHandUI
                            )
                        }
                        .tint(theme.colors.primary)
                    }
            }
        } header: {
            Text(NSLocalizedString("Later", comment: "chat list section"))
                .font(.subheadline.weight(.semibold))
                .foregroundColor(theme.colors.secondary)
                .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                .textCase(nil)
        }
    }
}

private struct LaterReminderRow: View {
    @EnvironmentObject var theme: AppTheme
    let reminder: MessageReminder
    let oneHandUI: Bool

    var body: some View {
        Button {
            ChatModel.shared.openAroundItemId = reminder.itemId
            ItemsModel.shared.loadOpenChatNoWait(reminder.chatId, reminder.itemId)
        } label: {
            HStack(alignment: .top, spacing: 10) {
                Image(systemName: "bell.fill")
                    .foregroundColor(reminder.isOverdue ? .orange : theme.colors.secondary)
                    .frame(width: 22)
                VStack(alignment: .leading, spacing: 4) {
                    HStack {
                        Text(reminder.chatDisplayName.isEmpty ? reminder.chatId : reminder.chatDisplayName)
                            .font(.headline)
                            .foregroundColor(theme.colors.onBackground)
                            .lineLimit(1)
                        Spacer(minLength: 8)
                        Text(reminderDueLabel(reminder))
                            .font(.caption)
                            .foregroundColor(reminder.isOverdue ? .orange : theme.colors.secondary)
                    }
                    if !reminder.messagePreview.isEmpty {
                        Text(reminder.messagePreview)
                            .font(.subheadline)
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(2)
                    }
                }
            }
            .padding(.vertical, 6)
        }
        .buttonStyle(.plain)
    }

    private func reminderDueLabel(_ reminder: MessageReminder) -> String {
        if reminder.isOverdue {
            return NSLocalizedString("Overdue", comment: "message reminder status")
        }
        let formatter = RelativeDateTimeFormatter()
        formatter.unitsStyle = .abbreviated
        return formatter.localizedString(for: reminder.dueAt, relativeTo: .now)
    }
}
