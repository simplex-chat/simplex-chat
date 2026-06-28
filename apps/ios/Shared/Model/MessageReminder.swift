//
//  MessageReminder.swift
//  SimpleX
//
//  Client-local message reminder (issue #7135).
//

import Foundation
import SimpleXChat

let GROUP_DEFAULT_MESSAGE_REMINDERS = "MessageReminders"

enum ReminderPreset: String, CaseIterable {
    case in1Hour
    case in3Hours
    case tomorrow
    case nextWeek

    var menuTitle: String {
        switch self {
        case .in1Hour:
            return NSLocalizedString("In 1 hour", comment: "message reminder preset")
        case .in3Hours:
            return NSLocalizedString("In 3 hours", comment: "message reminder preset")
        case .tomorrow:
            return NSLocalizedString("Tomorrow", comment: "message reminder preset")
        case .nextWeek:
            return NSLocalizedString("Next week", comment: "message reminder preset")
        }
    }

    func dueAt(from now: Date = .now, calendar: Calendar = .current) -> Date {
        switch self {
        case .in1Hour:
            return now.addingTimeInterval(3600)
        case .in3Hours:
            return now.addingTimeInterval(3 * 3600)
        case .tomorrow:
            let startOfToday = calendar.startOfDay(for: now)
            guard let tomorrow = calendar.date(byAdding: .day, value: 1, to: startOfToday) else {
                return now.addingTimeInterval(24 * 3600)
            }
            return calendar.date(bySettingHour: 9, minute: 0, second: 0, of: tomorrow) ?? tomorrow
        case .nextWeek:
            let today = calendar.startOfDay(for: now)
            let weekday = calendar.component(.weekday, from: today)
            let daysUntilMonday = (9 - weekday) % 7
            let addDays = daysUntilMonday == 0 ? 7 : daysUntilMonday
            guard let monday = calendar.date(byAdding: .day, value: addDays, to: today) else {
                return now.addingTimeInterval(7 * 24 * 3600)
            }
            return calendar.date(bySettingHour: 9, minute: 0, second: 0, of: monday) ?? monday
        }
    }
}

struct MessageReminder: Codable, Equatable, Identifiable {
    let id: String
    let userId: Int64
    let chatId: ChatId
    let itemId: Int64
    let dueAt: Date
    let createdAt: Date
    var completedAt: Date?
    var messagePreview: String
    var chatDisplayName: String

    var isComplete: Bool { completedAt != nil }
    var isOverdue: Bool { !isComplete && dueAt < .now }
}

func newMessageReminderId() -> String { UUID().uuidString }

let messageReminderJsonEncoder: JSONEncoder = {
    let e = JSONEncoder()
    e.dateEncodingStrategy = .iso8601
    return e
}()

let messageReminderJsonDecoder: JSONDecoder = {
    let d = JSONDecoder()
    d.dateDecodingStrategy = .iso8601
    return d
}()

func canSetMessageReminder(_ ci: ChatItem, _ chat: Chat, live: Bool) -> Bool {
    guard chat.chatInfo.sendMsgEnabled else { return false }
    guard ci.meta.itemDeleted == nil, !ci.isReport, !ci.localNote, !ci.isLiveDummy, !live, !ci.meta.isLive else { return false }
    guard ci.content.msgContent != nil || !ci.content.text.isEmpty else { return false }
    switch ci.chatDir {
    case .channelRcv: return false
    default: return true
    }
}

func messageReminderPreview(_ ci: ChatItem, _ chat: Chat) -> String {
    let isChannel = chat.chatInfo.isChannel
    let text = ci.text(isChannel: isChannel)
    return String(text.prefix(200))
}
