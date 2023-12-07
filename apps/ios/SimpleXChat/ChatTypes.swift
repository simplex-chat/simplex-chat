//
//  ChatModel.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

public struct User: Identifiable, Decodable, UserLike, NamedChat {
    public var userId: Int64
    var userContactId: Int64
    var localDisplayName: ContactName
    public var profile: LocalProfile
    public var fullPreferences: FullPreferences
    public var activeUser: Bool

    public var displayName: String { get { profile.displayName } }
    public var fullName: String { get { profile.fullName } }
    public var image: String? { get { profile.image } }
    public var localAlias: String { get { "" } }

    public var showNtfs: Bool
    public var sendRcptsContacts: Bool
    public var sendRcptsSmallGroups: Bool
    public var viewPwdHash: UserPwdHash?

    public var id: Int64 { userId }

    public var hidden: Bool { viewPwdHash != nil }

    public var showNotifications: Bool {
        activeUser || showNtfs
    }

    public var addressShared: Bool {
        profile.contactLink != nil
    }

    public static let sampleData = User(
        userId: 1,
        userContactId: 1,
        localDisplayName: "alice",
        profile: LocalProfile.sampleData,
        fullPreferences: FullPreferences.sampleData,
        activeUser: true,
        showNtfs: true,
        sendRcptsContacts: true,
        sendRcptsSmallGroups: false
    )
}

public struct UserRef: Identifiable, Decodable, UserLike {
    public var userId: Int64
    public var localDisplayName: ContactName

    public var id: Int64 { userId }
}

public protocol UserLike: Identifiable {
    var userId: Int64  { get }
}

public struct UserPwdHash: Decodable {
    public var hash: String
    public var salt: String
}

public struct UserInfo: Decodable, Identifiable {
    public var user: User
    public var unreadCount: Int

    public init(user: User, unreadCount: Int) {
        self.user = user
        self.unreadCount = unreadCount
    }

    public var id: Int64 { user.userId }

    public static let sampleData = UserInfo(
        user: User.sampleData,
        unreadCount: 1
    )
}

public typealias ContactName = String

public typealias GroupName = String

public struct Profile: Codable, NamedChat {
    public init(
        displayName: String,
        fullName: String,
        image: String? = nil,
        contactLink: String? = nil,
        preferences: Preferences? = nil
    ) {
        self.displayName = displayName
        self.fullName = fullName
        self.image = image
        self.contactLink = contactLink
        self.preferences = preferences
    }

    public var displayName: String
    public var fullName: String
    public var image: String?
    public var contactLink: String?
    public var preferences: Preferences?
    public var localAlias: String { get { "" } }

    var profileViewName: String {
        (fullName == "" || displayName == fullName) ? displayName : "\(displayName) (\(fullName))"
    }

    public static let sampleData = Profile(
        displayName: "alice",
        fullName: "Alice"
    )
}

public struct LocalProfile: Codable, NamedChat {
    public init(
        profileId: Int64,
        displayName: String,
        fullName: String,
        image: String? = nil,
        contactLink: String? = nil,
        preferences: Preferences? = nil,
        localAlias: String
    ) {
        self.profileId = profileId
        self.displayName = displayName
        self.fullName = fullName
        self.image = image
        self.contactLink = contactLink
        self.preferences = preferences
        self.localAlias = localAlias
    }

    public var profileId: Int64
    public var displayName: String
    public var fullName: String
    public var image: String?
    public var contactLink: String?
    public var preferences: Preferences?
    public var localAlias: String

    var profileViewName: String {
        localAlias == ""
        ? (fullName == "" || displayName == fullName) ? displayName : "\(displayName) (\(fullName))"
        : localAlias
    }

    static let sampleData = LocalProfile(
        profileId: 1,
        displayName: "alice",
        fullName: "Alice",
        preferences: Preferences.sampleData,
        localAlias: ""
    )
}

public func toLocalProfile (_ profileId: Int64, _ profile: Profile, _ localAlias: String) -> LocalProfile {
    LocalProfile(profileId: profileId, displayName: profile.displayName, fullName: profile.fullName, image: profile.image, contactLink: profile.contactLink, preferences: profile.preferences, localAlias: localAlias)
}

public func fromLocalProfile (_ profile: LocalProfile) -> Profile {
    Profile(displayName: profile.displayName, fullName: profile.fullName, image: profile.image, contactLink: profile.contactLink, preferences: profile.preferences)
}

public struct UserProfileUpdateSummary: Decodable {
    public var notChanged: Int
    public var updateSuccesses: Int
    public var updateFailures: Int
    public var changedContacts: [Contact]
}

public enum ChatType: String {
    case direct = "@"
    case group = "#"
    case contactRequest = "<@"
    case contactConnection = ":"
}

public protocol NamedChat {
    var displayName: String { get }
    var fullName: String { get }
    var image: String? { get }
    var localAlias: String { get }
}

extension NamedChat {
    public var chatViewName: String {
        localAlias == ""
        ? displayName + (fullName == "" || fullName == displayName ? "" : " / \(fullName)")
        : localAlias
    }
}

public typealias ChatId = String

public struct FullPreferences: Decodable, Equatable {
    public var timedMessages: TimedMessagesPreference
    public var fullDelete: SimplePreference
    public var reactions: SimplePreference
    public var voice: SimplePreference
    public var calls: SimplePreference

    public init(
        timedMessages: TimedMessagesPreference,
        fullDelete: SimplePreference,
        reactions: SimplePreference,
        voice: SimplePreference,
        calls: SimplePreference
    ) {
        self.timedMessages = timedMessages
        self.fullDelete = fullDelete
        self.reactions = reactions
        self.voice = voice
        self.calls = calls
    }

    public static let sampleData = FullPreferences(
        timedMessages:  TimedMessagesPreference(allow: .no),
        fullDelete: SimplePreference(allow: .no),
        reactions: SimplePreference(allow: .yes),
        voice: SimplePreference(allow: .yes),
        calls: SimplePreference(allow: .yes)
    )
}

public struct Preferences: Codable {
    public var timedMessages: TimedMessagesPreference?
    public var fullDelete: SimplePreference?
    public var reactions: SimplePreference?
    public var voice: SimplePreference?
    public var calls: SimplePreference?

    public init(
        timedMessages: TimedMessagesPreference?,
        fullDelete: SimplePreference?,
        reactions: SimplePreference?,
        voice: SimplePreference?,
        calls: SimplePreference?
    ) {
        self.timedMessages = timedMessages
        self.fullDelete = fullDelete
        self.reactions = reactions
        self.voice = voice
        self.calls = calls
    }

    func copy(
        timedMessages: TimedMessagesPreference? = nil,
        fullDelete: SimplePreference? = nil,
        reactions: SimplePreference? = nil,
        voice: SimplePreference? = nil,
        calls: SimplePreference? = nil
    ) -> Preferences {
        Preferences(
            timedMessages: timedMessages ?? self.timedMessages,
            fullDelete: fullDelete ?? self.fullDelete,
            reactions: reactions ?? self.reactions,
            voice: voice ?? self.voice,
            calls: calls ?? self.calls
        )
    }

    public func setAllowed(_ feature: ChatFeature, allowed: FeatureAllowed = .yes, param: Int? = nil) -> Preferences {
        switch feature {
        case .timedMessages: return copy(timedMessages: TimedMessagesPreference(allow: allowed, ttl: param ?? timedMessages?.ttl))
        case .fullDelete: return copy(fullDelete: SimplePreference(allow: allowed))
        case .reactions: return copy(reactions: SimplePreference(allow: allowed))
        case .voice: return copy(voice: SimplePreference(allow: allowed))
        case .calls: return copy(calls: SimplePreference(allow: allowed))
        }
    }

    public static let sampleData = Preferences(
        timedMessages: TimedMessagesPreference(allow: .no),
        fullDelete: SimplePreference(allow: .no),
        reactions: SimplePreference(allow: .yes),
        voice: SimplePreference(allow: .yes),
        calls: SimplePreference(allow: .yes)
    )
}

public func fullPreferencesToPreferences(_ fullPreferences: FullPreferences) -> Preferences {
    Preferences(
        timedMessages: fullPreferences.timedMessages,
        fullDelete: fullPreferences.fullDelete,
        reactions: fullPreferences.reactions,
        voice: fullPreferences.voice,
        calls: fullPreferences.calls
    )
}

public func contactUserPreferencesToPreferences(_ contactUserPreferences: ContactUserPreferences) -> Preferences {
    Preferences(
        timedMessages: contactUserPreferences.timedMessages.userPreference.preference,
        fullDelete: contactUserPreferences.fullDelete.userPreference.preference,
        reactions: contactUserPreferences.reactions.userPreference.preference,
        voice: contactUserPreferences.voice.userPreference.preference,
        calls: contactUserPreferences.calls.userPreference.preference
    )
}

public protocol Preference: Codable, Equatable {
    var allow: FeatureAllowed { get set }
}

public struct SimplePreference: Preference {
    public var allow: FeatureAllowed

    public init(allow: FeatureAllowed) {
        self.allow = allow
    }
}

public struct TimedMessagesPreference: Preference {
    public var allow: FeatureAllowed
    public var ttl: Int?

    public init(allow: FeatureAllowed, ttl: Int? = nil) {
        self.allow = allow
        self.ttl = ttl
    }

    public static var ttlValues: [Int?] {
        [600, 3600, 86400, 7 * 86400, 30 * 86400, 3 * 30 * 86400, nil]
    }
}

public enum CustomTimeUnit {
    case second
    case minute
    case hour
    case day
    case week
    case month

    public var toSeconds: Int {
        switch self {
        case .second: return 1
        case .minute: return 60
        case .hour: return 3600
        case .day: return 86400
        case .week: return 7 * 86400
        case .month: return 30 * 86400
        }
    }

    public var text: String {
        switch self {
        case .second: return NSLocalizedString("seconds", comment: "time unit")
        case .minute: return NSLocalizedString("minutes", comment: "time unit")
        case .hour: return NSLocalizedString("hours", comment: "time unit")
        case .day: return NSLocalizedString("days", comment: "time unit")
        case .week: return NSLocalizedString("weeks", comment: "time unit")
        case .month: return NSLocalizedString("months", comment: "time unit")
        }
    }

    public static func toTimeUnit(seconds: Int) -> (CustomTimeUnit, Int) {
        let tryUnits = [month, week, day, hour, minute]
        var selectedUnit: (CustomTimeUnit, Int)? = nil
        for unit in tryUnits {
            let (v, r) = divMod(seconds, by: unit.toSeconds)
            if r == 0 {
                selectedUnit = (unit, v)
                break
            }
        }
        return selectedUnit ?? (CustomTimeUnit.second, seconds)
    }

    private static func divMod(_ n: Int, by d: Int) -> (Int, Int) {
        (n / d, n % d)
    }

    public static func toText(seconds: Int) -> String {
        let (unit, value) = toTimeUnit(seconds: seconds)
        switch unit {
        case .second:
            return String.localizedStringWithFormat(NSLocalizedString("%d sec", comment: "time interval"), value)
        case .minute:
            return String.localizedStringWithFormat(NSLocalizedString("%d min", comment: "time interval"), value)
        case .hour:
            return value == 1
            ? NSLocalizedString("1 hour", comment: "time interval")
            : String.localizedStringWithFormat(NSLocalizedString("%d hours", comment: "time interval"), value)
        case .day:
            return value == 1
            ? NSLocalizedString("1 day", comment: "time interval")
            : String.localizedStringWithFormat(NSLocalizedString("%d days", comment: "time interval"), value)
        case .week:
            return value == 1
            ? NSLocalizedString("1 week", comment: "time interval")
            : String.localizedStringWithFormat(NSLocalizedString("%d weeks", comment: "time interval"), value)
        case .month:
            return value == 1
            ? NSLocalizedString("1 month", comment: "time interval")
            : String.localizedStringWithFormat(NSLocalizedString("%d months", comment: "time interval"), value)
        }
    }

    public static func toShortText(seconds: Int) -> LocalizedStringKey {
        let (unit, value) = toTimeUnit(seconds: seconds)
        switch unit {
        case .second: return "\(value)s"
        case .minute: return "\(value)m"
        case .hour: return "\(value)h"
        case .day: return "\(value)d"
        case .week: return "\(value)w"
        case .month: return "\(value)mth"
        }
    }
}


public func timeText(_ seconds: Int?) -> String {
    guard let seconds = seconds else { return NSLocalizedString("off", comment: "time to disappear") }
    if seconds == 0 { return NSLocalizedString("0 sec", comment: "time to disappear") }
    return CustomTimeUnit.toText(seconds: seconds)
}

public func shortTimeText(_ seconds: Int?) -> LocalizedStringKey {
    guard let seconds = seconds else { return "off" }
    if seconds == 0 { return "0s" }
    return CustomTimeUnit.toShortText(seconds: seconds)
}

public struct ContactUserPreferences: Decodable {
    public var timedMessages: ContactUserPreference<TimedMessagesPreference>
    public var fullDelete: ContactUserPreference<SimplePreference>
    public var reactions: ContactUserPreference<SimplePreference>
    public var voice: ContactUserPreference<SimplePreference>
    public var calls: ContactUserPreference<SimplePreference>

    public init(
        timedMessages: ContactUserPreference<TimedMessagesPreference>,
        fullDelete: ContactUserPreference<SimplePreference>,
        reactions: ContactUserPreference<SimplePreference>,
        voice: ContactUserPreference<SimplePreference>,
        calls: ContactUserPreference<SimplePreference>
    ) {
        self.timedMessages = timedMessages
        self.fullDelete = fullDelete
        self.reactions = reactions
        self.voice = voice
        self.calls = calls
    }

    public static let sampleData = ContactUserPreferences(
        timedMessages: ContactUserPreference<TimedMessagesPreference>(
            enabled: FeatureEnabled(forUser: false, forContact: false),
            userPreference: ContactUserPref<TimedMessagesPreference>.user(preference: TimedMessagesPreference(allow: .yes)),
            contactPreference: TimedMessagesPreference(allow: .no)
        ),
        fullDelete: ContactUserPreference<SimplePreference>(
            enabled: FeatureEnabled(forUser: false, forContact: false),
            userPreference: ContactUserPref<SimplePreference>.user(preference: SimplePreference(allow: .no)),
            contactPreference: SimplePreference(allow: .no)
        ),
        reactions: ContactUserPreference<SimplePreference>(
            enabled: FeatureEnabled(forUser: true, forContact: true),
            userPreference: ContactUserPref<SimplePreference>.user(preference: SimplePreference(allow: .yes)),
            contactPreference: SimplePreference(allow: .yes)
        ),
        voice: ContactUserPreference<SimplePreference>(
            enabled: FeatureEnabled(forUser: true, forContact: true),
            userPreference: ContactUserPref<SimplePreference>.user(preference: SimplePreference(allow: .yes)),
            contactPreference: SimplePreference(allow: .yes)
        ),
        calls: ContactUserPreference<SimplePreference>(
            enabled: FeatureEnabled(forUser: true, forContact: true),
            userPreference: ContactUserPref<SimplePreference>.user(preference: SimplePreference(allow: .yes)),
            contactPreference: SimplePreference(allow: .yes)
        )
    )
}

public struct ContactUserPreference<P: Preference>: Decodable {
    public var enabled: FeatureEnabled
    public var userPreference: ContactUserPref<P>
    public var contactPreference: P

    public init(enabled: FeatureEnabled, userPreference: ContactUserPref<P>, contactPreference: P) {
        self.enabled = enabled
        self.userPreference = userPreference
        self.contactPreference = contactPreference
    }
}

public struct FeatureEnabled: Decodable {
    public var forUser: Bool
    public var forContact: Bool

    public init(forUser: Bool, forContact: Bool) {
        self.forUser = forUser
        self.forContact = forContact
    }

    public static func enabled(asymmetric: Bool, user: any Preference, contact: any Preference) -> FeatureEnabled {
        switch (user.allow, contact.allow) {
        case (.always, .no): return FeatureEnabled(forUser: false, forContact: asymmetric)
        case (.no, .always): return FeatureEnabled(forUser: asymmetric, forContact: false)
        case (_, .no): return FeatureEnabled(forUser: false, forContact: false)
        case (.no, _): return FeatureEnabled(forUser: false, forContact: false)
        default: return FeatureEnabled(forUser: true, forContact: true)
        }
    }

    public var text: String {
        (forUser && forContact) ? NSLocalizedString("enabled", comment: "enabled status")
        : forUser ? NSLocalizedString("enabled for you", comment: "enabled status")
        : forContact ? NSLocalizedString("enabled for contact", comment: "enabled status")
        : NSLocalizedString("off", comment: "enabled status")
    }

    public var iconColor: Color {
        forUser ? .green : forContact ? .yellow : .secondary
    }
}

public enum ContactUserPref<P: Preference>: Decodable {
    case contact(preference: P) // contact override is set
    case user(preference: P) // global user default is used

    public var preference: P {
        switch self {
        case let .contact(preference): return preference
        case let .user(preference): return preference
        }
    }
}

public protocol Feature {
    var icon: String { get }
    var iconFilled: String { get }
    var iconScale: CGFloat { get }
    var hasParam: Bool { get }
    var text: String { get }
}

public enum ChatFeature: String, Decodable, Feature {
    case timedMessages
    case fullDelete
    case reactions
    case voice
    case calls

    public var id: Self { self }

    public var asymmetric: Bool {
        switch self {
        case .timedMessages: return false
        default: return true
        }
    }

    public var hasParam: Bool {
        switch self {
        case .timedMessages: return true
        default: return false
        }
    }

    public var text: String {
        switch self {
        case .timedMessages: return NSLocalizedString("Disappearing messages", comment: "chat feature")
        case .fullDelete: return NSLocalizedString("Delete for everyone", comment: "chat feature")
        case .reactions: return NSLocalizedString("Message reactions", comment: "chat feature")
        case .voice: return NSLocalizedString("Voice messages", comment: "chat feature")
        case .calls: return NSLocalizedString("Audio/video calls", comment: "chat feature")
        }
    }

    public var icon: String {
        switch self {
        case .timedMessages: return "stopwatch"
        case .fullDelete: return "trash.slash"
        case .reactions: return "face.smiling"
        case .voice: return "mic"
        case .calls: return "phone"
        }
    }

    public var iconFilled: String {
        switch self {
        case .timedMessages: return "stopwatch.fill"
        case .fullDelete: return "trash.slash.fill"
        case .reactions: return "face.smiling.fill"
        case .voice: return "mic.fill"
        case .calls: return "phone.fill"
        }
    }

    public var iconScale: CGFloat {
        switch self {
        case .timedMessages: return 0.9
        default: return 1
        }
    }

    public func allowDescription(_ allowed: FeatureAllowed) -> LocalizedStringKey {
        switch self {
        case .timedMessages:
            switch allowed {
            case .always: return "Allow your contacts to send disappearing messages."
            case .yes: return "Allow disappearing messages only if your contact allows it to you."
            case .no: return "Prohibit sending disappearing messages."
            }
        case .fullDelete:
            switch allowed {
            case .always: return "Allow your contacts to irreversibly delete sent messages."
            case .yes: return "Allow irreversible message deletion only if your contact allows it to you."
            case .no: return "Contacts can mark messages for deletion; you will be able to view them."
            }
        case .reactions:
            switch allowed {
            case .always: return "Allow your contacts adding message reactions."
            case .yes: return "Allow message reactions only if your contact allows them."
            case .no: return "Prohibit message reactions."
            }
        case .voice:
            switch allowed {
            case .always: return "Allow your contacts to send voice messages."
            case .yes: return "Allow voice messages only if your contact allows them."
            case .no: return "Prohibit sending voice messages."
            }
        case .calls:
            switch allowed {
            case .always: return "Allow your contacts to call you."
            case .yes: return "Allow calls only if your contact allows them."
            case .no: return "Prohibit audio/video calls."
            }
        }
    }

    public func enabledDescription(_ enabled: FeatureEnabled) -> LocalizedStringKey {
        switch self {
        case .timedMessages:
            return enabled.forUser && enabled.forContact
                    ? "Both you and your contact can send disappearing messages."
                    : enabled.forUser
                    ? "Only you can send disappearing messages."
                    : enabled.forContact
                    ? "Only your contact can send disappearing messages."
                    : "Disappearing messages are prohibited in this chat."
        case .fullDelete:
            return enabled.forUser && enabled.forContact
                    ? "Both you and your contact can irreversibly delete sent messages."
                    : enabled.forUser
                    ? "Only you can irreversibly delete messages (your contact can mark them for deletion)."
                    : enabled.forContact
                    ? "Only your contact can irreversibly delete messages (you can mark them for deletion)."
                    : "Irreversible message deletion is prohibited in this chat."
        case .reactions:
            return enabled.forUser && enabled.forContact
                    ? "Both you and your contact can add message reactions."
                    : enabled.forUser
                    ? "Only you can add message reactions."
                    : enabled.forContact
                    ? "Only your contact can add message reactions."
                    : "Message reactions are prohibited in this chat."
        case .voice:
            return enabled.forUser && enabled.forContact
                    ? "Both you and your contact can send voice messages."
                    : enabled.forUser
                    ? "Only you can send voice messages."
                    : enabled.forContact
                    ? "Only your contact can send voice messages."
                    : "Voice messages are prohibited in this chat."
        case .calls:
            return enabled.forUser && enabled.forContact
                    ? "Both you and your contact can make calls."
                    : enabled.forUser
                    ? "Only you can make calls."
                    : enabled.forContact
                    ? "Only your contact can make calls."
                    : "Audio/video calls are prohibited."
        }
    }
}

public enum GroupFeature: String, Decodable, Feature {
    case timedMessages
    case directMessages
    case fullDelete
    case reactions
    case voice
    case files

    public var id: Self { self }

    public var hasParam: Bool {
        switch self {
        case .timedMessages: return true
        default: return false
        }
    }

    public var text: String {
        switch self {
        case .timedMessages: return NSLocalizedString("Disappearing messages", comment: "chat feature")
        case .directMessages: return NSLocalizedString("Direct messages", comment: "chat feature")
        case .fullDelete: return NSLocalizedString("Delete for everyone", comment: "chat feature")
        case .reactions: return NSLocalizedString("Message reactions", comment: "chat feature")
        case .voice: return NSLocalizedString("Voice messages", comment: "chat feature")
        case .files: return NSLocalizedString("Files and media", comment: "chat feature")
        }
    }

    public var icon: String {
        switch self {
        case .timedMessages: return "stopwatch"
        case .directMessages: return "arrow.left.and.right.circle"
        case .fullDelete: return "trash.slash"
        case .reactions: return "face.smiling"
        case .voice: return "mic"
        case .files: return "doc"
        }
    }

    public var iconFilled: String {
        switch self {
        case .timedMessages: return "stopwatch.fill"
        case .directMessages: return "arrow.left.and.right.circle.fill"
        case .fullDelete: return "trash.slash.fill"
        case .reactions: return "face.smiling.fill"
        case .voice: return "mic.fill"
        case .files: return "doc.fill"
        }
    }

    public var iconScale: CGFloat {
        switch self {
        case .timedMessages: return 0.9
        default: return 1
        }
    }

    public func enableDescription(_ enabled: GroupFeatureEnabled, _ canEdit: Bool) -> LocalizedStringKey {
        if canEdit {
            switch self {
            case .timedMessages:
                switch enabled {
                case .on: return "Allow sending disappearing messages."
                case .off: return "Prohibit sending disappearing messages."
                }
            case .directMessages:
                switch enabled {
                case .on: return "Allow sending direct messages to members."
                case .off: return "Prohibit sending direct messages to members."
                }
            case .fullDelete:
                switch enabled {
                case .on: return "Allow to irreversibly delete sent messages."
                case .off: return "Prohibit irreversible message deletion."
                }
            case .reactions:
                switch enabled {
                case .on: return "Allow message reactions."
                case .off: return "Prohibit messages reactions."
                }
            case .voice:
                switch enabled {
                case .on: return "Allow to send voice messages."
                case .off: return "Prohibit sending voice messages."
                }
            case .files:
                switch enabled {
                case .on: return "Allow to send files and media."
                case .off: return "Prohibit sending files and media."
                }
            }
        } else {
            switch self {
            case .timedMessages:
                switch enabled {
                case .on: return "Group members can send disappearing messages."
                case .off: return "Disappearing messages are prohibited in this group."
                }
            case .directMessages:
                switch enabled {
                case .on: return "Group members can send direct messages."
                case .off: return "Direct messages between members are prohibited in this group."
                }
            case .fullDelete:
                switch enabled {
                case .on: return "Group members can irreversibly delete sent messages."
                case .off: return "Irreversible message deletion is prohibited in this group."
                }
            case .reactions:
                switch enabled {
                case .on: return "Group members can add message reactions."
                case .off: return "Message reactions are prohibited in this group."
                }
            case .voice:
                switch enabled {
                case .on: return "Group members can send voice messages."
                case .off: return "Voice messages are prohibited in this group."
                }
            case .files:
                switch enabled {
                case .on: return "Group members can send files and media."
                case .off: return "Files and media are prohibited in this group."
                }
            }
        }
    }
}

public enum ContactFeatureAllowed: Identifiable, Hashable {
    case userDefault(FeatureAllowed)
    case always
    case yes
    case no

    public static func values(_ def: FeatureAllowed) -> [ContactFeatureAllowed] {
        [.userDefault(def) , .always, .yes, .no]
    }

    public var id: Self { self }

    public var allowed: FeatureAllowed {
        switch self {
        case let .userDefault(def): return def
        case .always: return .always
        case .yes: return .yes
        case .no: return .no
        }
    }

    public var text: String {
        switch self {
        case let .userDefault(def): return String.localizedStringWithFormat(NSLocalizedString("default (%@)", comment: "pref value"), def.text)
        case .always: return NSLocalizedString("always", comment: "pref value")
        case .yes: return NSLocalizedString("yes", comment: "pref value")
        case .no: return NSLocalizedString("no", comment: "pref value")
        }
    }
}

public struct ContactFeaturesAllowed: Equatable {
    public var timedMessagesAllowed: Bool
    public var timedMessagesTTL: Int?
    public var fullDelete: ContactFeatureAllowed
    public var reactions: ContactFeatureAllowed
    public var voice: ContactFeatureAllowed
    public var calls: ContactFeatureAllowed

    public init(
        timedMessagesAllowed: Bool,
        timedMessagesTTL: Int?,
        fullDelete: ContactFeatureAllowed,
        reactions: ContactFeatureAllowed,
        voice: ContactFeatureAllowed,
        calls: ContactFeatureAllowed
    ) {
        self.timedMessagesAllowed = timedMessagesAllowed
        self.timedMessagesTTL = timedMessagesTTL
        self.fullDelete = fullDelete
        self.reactions = reactions
        self.voice = voice
        self.calls = calls
    }

    public static let sampleData = ContactFeaturesAllowed(
        timedMessagesAllowed: false,
        timedMessagesTTL: nil,
        fullDelete: ContactFeatureAllowed.userDefault(.no),
        reactions: ContactFeatureAllowed.userDefault(.yes),
        voice: ContactFeatureAllowed.userDefault(.yes),
        calls: ContactFeatureAllowed.userDefault(.yes)
    )
}

public func contactUserPrefsToFeaturesAllowed(_ contactUserPreferences: ContactUserPreferences) -> ContactFeaturesAllowed {
    let pref = contactUserPreferences.timedMessages.userPreference
    let allow = pref.preference.allow
    return ContactFeaturesAllowed(
        timedMessagesAllowed: allow == .yes || allow == .always,
        timedMessagesTTL: pref.preference.ttl,
        fullDelete: contactUserPrefToFeatureAllowed(contactUserPreferences.fullDelete),
        reactions: contactUserPrefToFeatureAllowed(contactUserPreferences.reactions),
        voice: contactUserPrefToFeatureAllowed(contactUserPreferences.voice),
        calls: contactUserPrefToFeatureAllowed(contactUserPreferences.calls)
    )
}

public func contactUserPrefToFeatureAllowed(_ contactUserPreference: ContactUserPreference<SimplePreference>) -> ContactFeatureAllowed {
    switch contactUserPreference.userPreference {
    case let .user(preference): return .userDefault(preference.allow)
    case let .contact(preference):
        switch preference.allow {
        case .always: return .always
        case .yes: return .yes
        case .no: return .no
        }
    }
}

public func contactFeaturesAllowedToPrefs(_ contactFeaturesAllowed: ContactFeaturesAllowed) -> Preferences {
    Preferences(
        timedMessages: TimedMessagesPreference(allow: contactFeaturesAllowed.timedMessagesAllowed ? .yes : .no, ttl: contactFeaturesAllowed.timedMessagesTTL),
        fullDelete: contactFeatureAllowedToPref(contactFeaturesAllowed.fullDelete),
        reactions: contactFeatureAllowedToPref(contactFeaturesAllowed.reactions),
        voice: contactFeatureAllowedToPref(contactFeaturesAllowed.voice),
        calls: contactFeatureAllowedToPref(contactFeaturesAllowed.calls)
    )
}

public func contactFeatureAllowedToPref(_ contactFeatureAllowed: ContactFeatureAllowed) -> SimplePreference? {
    switch contactFeatureAllowed {
    case .userDefault: return nil
    case .always: return SimplePreference(allow: .always)
    case .yes: return SimplePreference(allow: .yes)
    case .no: return SimplePreference(allow: .no)
    }
}

public enum FeatureAllowed: String, Codable, Identifiable {
    case always
    case yes
    case no

    public static var values: [FeatureAllowed] { [.always, .yes, .no] }

    public var id: Self { self }

    public var text: String {
        switch self {
        case .always: return NSLocalizedString("always", comment: "pref value")
        case .yes: return NSLocalizedString("yes", comment: "pref value")
        case .no: return NSLocalizedString("no", comment: "pref value")
        }
    }
}

public struct FullGroupPreferences: Decodable, Equatable {
    public var timedMessages: TimedMessagesGroupPreference
    public var directMessages: GroupPreference
    public var fullDelete: GroupPreference
    public var reactions: GroupPreference
    public var voice: GroupPreference
    public var files: GroupPreference

    public init(
        timedMessages: TimedMessagesGroupPreference,
        directMessages: GroupPreference,
        fullDelete: GroupPreference,
        reactions: GroupPreference,
        voice: GroupPreference,
        files: GroupPreference
    ) {
        self.timedMessages = timedMessages
        self.directMessages = directMessages
        self.fullDelete = fullDelete
        self.reactions = reactions
        self.voice = voice
        self.files = files
    }

    public static let sampleData = FullGroupPreferences(
        timedMessages: TimedMessagesGroupPreference(enable: .off),
        directMessages: GroupPreference(enable: .off),
        fullDelete: GroupPreference(enable: .off),
        reactions: GroupPreference(enable: .on),
        voice: GroupPreference(enable: .on),
        files: GroupPreference(enable: .on)
    )
}

public struct GroupPreferences: Codable {
    public var timedMessages: TimedMessagesGroupPreference?
    public var directMessages: GroupPreference?
    public var fullDelete: GroupPreference?
    public var reactions: GroupPreference?
    public var voice: GroupPreference?
    public var files: GroupPreference?

    public init(
        timedMessages: TimedMessagesGroupPreference?,
        directMessages: GroupPreference?,
        fullDelete: GroupPreference?,
        reactions: GroupPreference?,
        voice: GroupPreference?,
        files: GroupPreference?
    ) {
        self.timedMessages = timedMessages
        self.directMessages = directMessages
        self.fullDelete = fullDelete
        self.reactions = reactions
        self.voice = voice
        self.files = files
    }

    public static let sampleData = GroupPreferences(
        timedMessages: TimedMessagesGroupPreference(enable: .off),
        directMessages: GroupPreference(enable: .off),
        fullDelete: GroupPreference(enable: .off),
        reactions: GroupPreference(enable: .on),
        voice: GroupPreference(enable: .on),
        files: GroupPreference(enable: .on)
    )
}

public func toGroupPreferences(_ fullPreferences: FullGroupPreferences) -> GroupPreferences {
    GroupPreferences(
        timedMessages: fullPreferences.timedMessages,
        directMessages: fullPreferences.directMessages,
        fullDelete: fullPreferences.fullDelete,
        reactions: fullPreferences.reactions,
        voice: fullPreferences.voice,
        files: fullPreferences.files
    )
}

public struct GroupPreference: Codable, Equatable {
    public var enable: GroupFeatureEnabled

    public var on: Bool {
        enable == .on
    }

    public init(enable: GroupFeatureEnabled) {
        self.enable = enable
    }
}

public struct TimedMessagesGroupPreference: Codable, Equatable {
    public var enable: GroupFeatureEnabled
    public var ttl: Int?

    public var on: Bool {
        enable == .on
    }

    public init(enable: GroupFeatureEnabled, ttl: Int? = nil) {
        self.enable = enable
        self.ttl = ttl
    }
}

public enum GroupFeatureEnabled: String, Codable, Identifiable {
    case on
    case off

    public static var values: [GroupFeatureEnabled] { [.on, .off] }

    public var id: Self { self }

    public var text: String {
        switch self {
        case .on: return NSLocalizedString("on", comment: "group pref value")
        case .off: return NSLocalizedString("off", comment: "group pref value")
        }
    }

    public var iconColor: Color {
        switch self {
        case .on: return .green
        case .off: return .secondary
        }
    }
}

public enum ChatInfo: Identifiable, Decodable, NamedChat {
    case direct(contact: Contact)
    case group(groupInfo: GroupInfo)
    case contactRequest(contactRequest: UserContactRequest)
    case contactConnection(contactConnection: PendingContactConnection)
    case invalidJSON(json: String)

    private static let invalidChatName = NSLocalizedString("invalid chat", comment: "invalid chat data")

    public var localDisplayName: String {
        get {
            switch self {
            case let .direct(contact): return contact.localDisplayName
            case let .group(groupInfo): return groupInfo.localDisplayName
            case let .contactRequest(contactRequest): return contactRequest.localDisplayName
            case let .contactConnection(contactConnection): return contactConnection.localDisplayName
            case .invalidJSON: return ChatInfo.invalidChatName
            }
        }
    }

    public var displayName: String {
        get {
            switch self {
            case let .direct(contact): return contact.displayName
            case let .group(groupInfo): return groupInfo.displayName
            case let .contactRequest(contactRequest): return contactRequest.displayName
            case let .contactConnection(contactConnection): return contactConnection.displayName
            case .invalidJSON: return ChatInfo.invalidChatName
            }
        }
    }

    public var fullName: String {
        get {
            switch self {
            case let .direct(contact): return contact.fullName
            case let .group(groupInfo): return groupInfo.fullName
            case let .contactRequest(contactRequest): return contactRequest.fullName
            case let .contactConnection(contactConnection): return contactConnection.fullName
            case .invalidJSON: return ChatInfo.invalidChatName
            }
        }
    }

    public var image: String? {
        get {
            switch self {
            case let .direct(contact): return contact.image
            case let .group(groupInfo): return groupInfo.image
            case let .contactRequest(contactRequest): return contactRequest.image
            case let .contactConnection(contactConnection): return contactConnection.image
            case .invalidJSON: return nil
            }
        }
    }

    public var localAlias: String {
        get {
            switch self {
            case let .direct(contact): return contact.localAlias
            case let .group(groupInfo): return groupInfo.localAlias
            case let .contactRequest(contactRequest): return contactRequest.localAlias
            case let .contactConnection(contactConnection): return contactConnection.localAlias
            case .invalidJSON: return ""
            }
        }
    }

    public var id: ChatId {
        get {
            switch self {
            case let .direct(contact): return contact.id
            case let .group(groupInfo): return groupInfo.id
            case let .contactRequest(contactRequest): return contactRequest.id
            case let .contactConnection(contactConnection): return contactConnection.id
            case .invalidJSON: return ""
            }
        }
    }

    public var chatType: ChatType {
        get {
            switch self {
            case .direct: return .direct
            case .group: return .group
            case .contactRequest: return .contactRequest
            case .contactConnection: return .contactConnection
            case .invalidJSON: return .direct
            }
        }
    }

    public var apiId: Int64 {
        get {
            switch self {
            case let .direct(contact): return contact.apiId
            case let .group(groupInfo): return groupInfo.apiId
            case let .contactRequest(contactRequest): return contactRequest.apiId
            case let .contactConnection(contactConnection): return contactConnection.apiId
            case .invalidJSON: return 0
            }
        }
    }

    public var ready: Bool {
        get {
            switch self {
            case let .direct(contact): return contact.ready
            case let .group(groupInfo): return groupInfo.ready
            case let .contactRequest(contactRequest): return contactRequest.ready
            case let .contactConnection(contactConnection): return contactConnection.ready
            case .invalidJSON: return false
            }
        }
    }

    public var sendMsgEnabled: Bool {
        get {
            switch self {
            case let .direct(contact): return contact.sendMsgEnabled
            case let .group(groupInfo): return groupInfo.sendMsgEnabled
            case let .contactRequest(contactRequest): return contactRequest.sendMsgEnabled
            case let .contactConnection(contactConnection): return contactConnection.sendMsgEnabled
            case .invalidJSON: return false
            }
        }
    }

    public var incognito: Bool {
        get {
            switch self {
            case let .direct(contact): return contact.contactConnIncognito
            case let .group(groupInfo): return groupInfo.membership.memberIncognito
            case .contactRequest: return false
            case let .contactConnection(contactConnection): return contactConnection.incognito
            case .invalidJSON: return false
            }
        }
    }

    public var contact: Contact? {
        switch self {
        case let .direct(contact): return contact
        default: return nil
        }
    }

    public var groupInfo: GroupInfo? {
        switch self {
        case let .group(groupInfo): return groupInfo
        default: return nil
        }
    }

    // this works for features that are common for contacts and groups
    public func featureEnabled(_ feature: ChatFeature) -> Bool {
        switch self {
        case let .direct(contact):
            let cups = contact.mergedPreferences
            switch feature {
            case .timedMessages: return cups.timedMessages.enabled.forUser
            case .fullDelete: return cups.fullDelete.enabled.forUser
            case .reactions: return cups.reactions.enabled.forUser
            case .voice: return cups.voice.enabled.forUser
            case .calls: return cups.calls.enabled.forUser
            }
        case let .group(groupInfo):
            let prefs = groupInfo.fullGroupPreferences
            switch feature {
            case .timedMessages: return prefs.timedMessages.on
            case .fullDelete: return prefs.fullDelete.on
            case .reactions: return prefs.reactions.on
            case .voice: return prefs.voice.on
            case .calls: return false
            }
        default: return false
        }
    }

    public var timedMessagesTTL: Int? {
        switch self {
        case let .direct(contact):
            let pref = contact.mergedPreferences.timedMessages
            return pref.enabled.forUser ? pref.userPreference.preference.ttl : nil
        case let .group(groupInfo):
            let pref = groupInfo.fullGroupPreferences.timedMessages
            return pref.on ? pref.ttl : nil
        default:
            return nil
        }
    }

    public enum ShowEnableVoiceMessagesAlert {
        case userEnable
        case askContact
        case groupOwnerCan
        case other
    }

    public var showEnableVoiceMessagesAlert: ShowEnableVoiceMessagesAlert {
        switch self {
        case let .direct(contact):
            if contact.mergedPreferences.voice.userPreference.preference.allow == .no {
                return .userEnable
            } else if contact.mergedPreferences.voice.contactPreference.allow == .no {
                return .askContact
            } else {
                return .other
            }
        case let .group(groupInfo):
            if !groupInfo.fullGroupPreferences.voice.on {
                return .groupOwnerCan
            } else {
                return .other
            }
        default:
            return .other
        }
    }

    public var ntfsEnabled: Bool {
        self.chatSettings?.enableNtfs == .all
    }

    public var chatSettings: ChatSettings? {
        switch self {
        case let .direct(contact): return contact.chatSettings
        case let .group(groupInfo): return groupInfo.chatSettings
        default: return nil
        }
    }

    var createdAt: Date {
        switch self {
        case let .direct(contact): return contact.createdAt
        case let .group(groupInfo): return groupInfo.createdAt
        case let .contactRequest(contactRequest): return contactRequest.createdAt
        case let .contactConnection(contactConnection): return contactConnection.createdAt
        case .invalidJSON: return .now
        }
    }

    public var updatedAt: Date {
        switch self {
        case let .direct(contact): return contact.updatedAt
        case let .group(groupInfo): return groupInfo.updatedAt
        case let .contactRequest(contactRequest): return contactRequest.updatedAt
        case let .contactConnection(contactConnection): return contactConnection.updatedAt
        case .invalidJSON: return .now
        }
    }

    public struct SampleData {
        public var direct: ChatInfo
        public var group: ChatInfo
        public var contactRequest: ChatInfo
        public var contactConnection: ChatInfo
    }

    public static var sampleData: ChatInfo.SampleData = SampleData(
        direct: ChatInfo.direct(contact: Contact.sampleData),
        group: ChatInfo.group(groupInfo: GroupInfo.sampleData),
        contactRequest: ChatInfo.contactRequest(contactRequest: UserContactRequest.sampleData),
        contactConnection: ChatInfo.contactConnection(contactConnection: PendingContactConnection.getSampleData())
    )
}

public struct ChatData: Decodable, Identifiable {
    public var chatInfo: ChatInfo
    public var chatItems: [ChatItem]
    public var chatStats: ChatStats

    public var id: ChatId { get { chatInfo.id } }

    public static func invalidJSON(_ json: String) -> ChatData {
        ChatData(
            chatInfo: .invalidJSON(json: json),
            chatItems: [],
            chatStats: ChatStats()
        )
    }
}

public struct ChatStats: Decodable {
    public init(unreadCount: Int = 0, minUnreadItemId: Int64 = 0, unreadChat: Bool = false) {
        self.unreadCount = unreadCount
        self.minUnreadItemId = minUnreadItemId
        self.unreadChat = unreadChat
    }

    public var unreadCount: Int = 0
    public var minUnreadItemId: Int64 = 0
    public var unreadChat: Bool = false
}

public struct Contact: Identifiable, Decodable, NamedChat {
    public var contactId: Int64
    var localDisplayName: ContactName
    public var profile: LocalProfile
    public var activeConn: Connection?
    public var viaGroup: Int64?
    public var contactUsed: Bool
    public var contactStatus: ContactStatus
    public var chatSettings: ChatSettings
    public var userPreferences: Preferences
    public var mergedPreferences: ContactUserPreferences
    var createdAt: Date
    var updatedAt: Date
    var contactGroupMemberId: Int64?
    var contactGrpInvSent: Bool

    public var id: ChatId { get { "@\(contactId)" } }
    public var apiId: Int64 { get { contactId } }
    public var ready: Bool { get { activeConn?.connStatus == .ready } }
    public var active: Bool { get { contactStatus == .active } }
    public var sendMsgEnabled: Bool { get {
        (ready && active && !(activeConn?.connectionStats?.ratchetSyncSendProhibited ?? false))
        || nextSendGrpInv
    } }
    public var nextSendGrpInv: Bool { get { contactGroupMemberId != nil && !contactGrpInvSent } }
    public var displayName: String { localAlias == "" ? profile.displayName : localAlias }
    public var fullName: String { get { profile.fullName } }
    public var image: String? { get { profile.image } }
    public var contactLink: String? { get { profile.contactLink } }
    public var localAlias: String { profile.localAlias }
    public var verified: Bool { activeConn?.connectionCode != nil }

    public var directOrUsed: Bool {
        if let activeConn = activeConn {
            (activeConn.connLevel == 0 && !activeConn.viaGroupLink) || contactUsed
        } else {
            true
        }
    }

    public var contactConnIncognito: Bool {
        activeConn?.customUserProfileId != nil
    }

    public func allowsFeature(_ feature: ChatFeature) -> Bool {
        switch feature {
        case .timedMessages: return mergedPreferences.timedMessages.contactPreference.allow != .no
        case .fullDelete: return mergedPreferences.fullDelete.contactPreference.allow != .no
        case .reactions: return mergedPreferences.reactions.contactPreference.allow != .no
        case .voice: return mergedPreferences.voice.contactPreference.allow != .no
        case .calls: return mergedPreferences.calls.contactPreference.allow != .no
        }
    }

    public func userAllowsFeature(_ feature: ChatFeature) -> Bool {
        switch feature {
        case .timedMessages: return mergedPreferences.timedMessages.userPreference.preference.allow != .no
        case .fullDelete: return mergedPreferences.fullDelete.userPreference.preference.allow != .no
        case .reactions: return mergedPreferences.reactions.userPreference.preference.allow != .no
        case .voice: return mergedPreferences.voice.userPreference.preference.allow != .no
        case .calls: return mergedPreferences.calls.userPreference.preference.allow != .no
        }
    }

    public static let sampleData = Contact(
        contactId: 1,
        localDisplayName: "alice",
        profile: LocalProfile.sampleData,
        activeConn: Connection.sampleData,
        contactUsed: true,
        contactStatus: .active,
        chatSettings: ChatSettings.defaults,
        userPreferences: Preferences.sampleData,
        mergedPreferences: ContactUserPreferences.sampleData,
        createdAt: .now,
        updatedAt: .now,
        contactGrpInvSent: false
    )
}

public enum ContactStatus: String, Decodable {
    case active = "active"
    case deleted = "deleted"
}

public struct ContactRef: Decodable, Equatable {
    var contactId: Int64
    public var agentConnId: String
    var connId: Int64
    var localDisplayName: ContactName

    public var id: ChatId { get { "@\(contactId)" } }
}

public struct ContactSubStatus: Decodable {
    public var contact: Contact
    public var contactError: ChatError?
}

public struct Connection: Decodable {
    public var connId: Int64
    public var agentConnId: String
    public var peerChatVRange: VersionRange
    var connStatus: ConnStatus
    public var connLevel: Int
    public var viaGroupLink: Bool
    public var customUserProfileId: Int64?
    public var connectionCode: SecurityCode?

    public var connectionStats: ConnectionStats? = nil

    private enum CodingKeys: String, CodingKey {
        case connId, agentConnId, peerChatVRange, connStatus, connLevel, viaGroupLink, customUserProfileId, connectionCode
    }

    public var id: ChatId { get { ":\(connId)" } }

    static let sampleData = Connection(
        connId: 1,
        agentConnId: "abc",
        peerChatVRange: VersionRange(minVersion: 1, maxVersion: 1),
        connStatus: .ready,
        connLevel: 0,
        viaGroupLink: false
    )
}

public struct VersionRange: Decodable {
    public init(minVersion: Int, maxVersion: Int) {
        self.minVersion = minVersion
        self.maxVersion = maxVersion
    }

    public var minVersion: Int
    public var maxVersion: Int

    public func isCompatibleRange(_ vRange: VersionRange) -> Bool {
        self.minVersion <= vRange.maxVersion && vRange.minVersion <= self.maxVersion
    }
}

public struct SecurityCode: Decodable, Equatable {
    public init(securityCode: String, verifiedAt: Date) {
        self.securityCode = securityCode
        self.verifiedAt = verifiedAt
    }

    public var securityCode: String
    public var verifiedAt: Date
}

public struct UserContact: Decodable {
    public var userContactLinkId: Int64
//    public var connReqContact: String
    public var groupId: Int64?

    public init(userContactLinkId: Int64) {
        self.userContactLinkId = userContactLinkId
    }

    public init(contactRequest: UserContactRequest) {
        self.userContactLinkId = contactRequest.userContactLinkId
    }

    public var id: String {
        "@>\(userContactLinkId)"
    }
}

public struct UserContactRequest: Decodable, NamedChat {
    var contactRequestId: Int64
    public var userContactLinkId: Int64
    public var cReqChatVRange: VersionRange
    var localDisplayName: ContactName
    var profile: Profile
    var createdAt: Date
    public var updatedAt: Date

    public var id: ChatId { get { "<@\(contactRequestId)" } }
    public var apiId: Int64 { get { contactRequestId } }
    var ready: Bool { get { true } }
    public var sendMsgEnabled: Bool { get { false } }
    public var displayName: String { get { profile.displayName } }
    public var fullName: String { get { profile.fullName } }
    public var image: String? { get { profile.image } }
    public var localAlias: String { "" }

    public static let sampleData = UserContactRequest(
        contactRequestId: 1,
        userContactLinkId: 1,
        cReqChatVRange: VersionRange(minVersion: 1, maxVersion: 1),
        localDisplayName: "alice",
        profile: Profile.sampleData,
        createdAt: .now,
        updatedAt: .now
    )
}

public struct PendingContactConnection: Decodable, NamedChat {
    public var pccConnId: Int64
    var pccAgentConnId: String
    var pccConnStatus: ConnStatus
    public var viaContactUri: Bool
    public var groupLinkId: String?
    public var customUserProfileId: Int64?
    public var connReqInv: String?
    public var localAlias: String
    var createdAt: Date
    public var updatedAt: Date

    public var id: ChatId { get { ":\(pccConnId)" } }
    public var apiId: Int64 { get { pccConnId } }
    var ready: Bool { get { false } }
    public var sendMsgEnabled: Bool { get { false } }
    var localDisplayName: String {
        get { String.localizedStringWithFormat(NSLocalizedString("connection:%@", comment: "connection information"), pccConnId) }
    }
    public var displayName: String {
        get {
            if let initiated = pccConnStatus.initiated {
                return initiated && !viaContactUri
                ? NSLocalizedString("invited to connect", comment: "chat list item title")
                : NSLocalizedString("connecting…", comment: "chat list item title")
            } else {
                // this should not be in the list
                return NSLocalizedString("connection established", comment: "chat list item title (it should not be shown")
            }
        }
    }
    public var fullName: String { get { "" } }
    public var image: String? { get { nil } }
    public var initiated: Bool { get { (pccConnStatus.initiated ?? false) && !viaContactUri } }

    public var incognito: Bool {
        customUserProfileId != nil
    }

    public var description: String {
        get {
            if let initiated = pccConnStatus.initiated {
                var desc: String
                if initiated && !viaContactUri {
                    if incognito {
                        desc = NSLocalizedString("you shared one-time link incognito", comment: "chat list item description")
                    } else {
                        desc = NSLocalizedString("you shared one-time link", comment: "chat list item description")
                    }
                } else if viaContactUri {
                    if groupLinkId != nil {
                        if incognito {
                            desc = NSLocalizedString("incognito via group link", comment: "chat list item description")
                        } else {
                            desc = NSLocalizedString("via group link", comment: "chat list item description")
                        }
                    } else {
                        if incognito {
                            desc = NSLocalizedString("incognito via contact address link", comment: "chat list item description")
                        } else {
                            desc = NSLocalizedString("via contact address link", comment: "chat list item description")
                        }
                    }
                } else {
                    if incognito {
                        desc = NSLocalizedString("incognito via one-time link", comment: "chat list item description")
                    } else {
                        desc = NSLocalizedString("via one-time link", comment: "chat list item description")
                    }
                }
                return desc
            } else {
                return ""
            }
        }
    }

    public static func getSampleData(_ status: ConnStatus = .new, viaContactUri: Bool = false) -> PendingContactConnection {
        PendingContactConnection(
            pccConnId: 1,
            pccAgentConnId: "abcd",
            pccConnStatus: status,
            viaContactUri: viaContactUri,
            localAlias: "",
            createdAt: .now,
            updatedAt: .now
        )
    }
}

public enum ConnStatus: String, Decodable {
    case new = "new"
    case joined = "joined"
    case requested = "requested"
    case accepted = "accepted"
    case sndReady = "snd-ready"
    case ready = "ready"
    case deleted = "deleted"

    var initiated: Bool? {
        get {
            switch self {
            case .new: return true
            case .joined: return false
            case .requested: return true
            case .accepted: return true
            case .sndReady: return false
            case .ready: return nil
            case .deleted: return nil
            }
        }
    }
}

public struct Group: Decodable {
    public var groupInfo: GroupInfo
    public var members: [GroupMember]

    public init(groupInfo: GroupInfo, members: [GroupMember]) {
        self.groupInfo = groupInfo
        self.members = members
    }
}

public struct GroupInfo: Identifiable, Decodable, NamedChat {
    public var groupId: Int64
    var localDisplayName: GroupName
    public var groupProfile: GroupProfile
    public var fullGroupPreferences: FullGroupPreferences
    public var membership: GroupMember
    public var hostConnCustomUserProfileId: Int64?
    public var chatSettings: ChatSettings
    var createdAt: Date
    var updatedAt: Date

    public var id: ChatId { get { "#\(groupId)" } }
    public var apiId: Int64 { get { groupId } }
    public var ready: Bool { get { true } }
    public var sendMsgEnabled: Bool { get { membership.memberActive } }
    public var displayName: String { get { groupProfile.displayName } }
    public var fullName: String { get { groupProfile.fullName } }
    public var image: String? { get { groupProfile.image } }
    public var localAlias: String { "" }

    public var canEdit: Bool {
        return membership.memberRole == .owner && membership.memberCurrent
    }

    public var canDelete: Bool {
        return membership.memberRole == .owner || !membership.memberCurrent
    }

    public var canAddMembers: Bool {
        return membership.memberRole >= .admin && membership.memberActive
    }

    public static let sampleData = GroupInfo(
        groupId: 1,
        localDisplayName: "team",
        groupProfile: GroupProfile.sampleData,
        fullGroupPreferences: FullGroupPreferences.sampleData,
        membership: GroupMember.sampleData,
        hostConnCustomUserProfileId: nil,
        chatSettings: ChatSettings.defaults,
        createdAt: .now,
        updatedAt: .now
    )
}

public struct GroupRef: Decodable {
    public var groupId: Int64
    var localDisplayName: GroupName
}

public struct GroupProfile: Codable, NamedChat {
    public init(displayName: String, fullName: String, description: String? = nil, image: String? = nil, groupPreferences: GroupPreferences? = nil) {
        self.displayName = displayName
        self.fullName = fullName
        self.description = description
        self.image = image
        self.groupPreferences = groupPreferences
    }

    public var displayName: String
    public var fullName: String
    public var description: String?
    public var image: String?
    public var groupPreferences: GroupPreferences?
    public var localAlias: String { "" }

    public static let sampleData = GroupProfile(
        displayName: "team",
        fullName: "My Team"
    )
}

public struct GroupMember: Identifiable, Decodable {
    public var groupMemberId: Int64
    public var groupId: Int64
    public var memberId: String
    public var memberRole: GroupMemberRole
    public var memberCategory: GroupMemberCategory
    public var memberStatus: GroupMemberStatus
    public var memberSettings: GroupMemberSettings
    public var invitedBy: InvitedBy
    public var localDisplayName: ContactName
    public var memberProfile: LocalProfile
    public var memberContactId: Int64?
    public var memberContactProfileId: Int64
    public var activeConn: Connection?

    public var id: String { "#\(groupId) @\(groupMemberId)" }
    public var displayName: String {
        get {
            let p = memberProfile
            return p.localAlias == "" ? p.displayName : p.localAlias
        }
    }
    public var fullName: String { get { memberProfile.fullName } }
    public var image: String? { get { memberProfile.image } }
    public var contactLink: String? { get { memberProfile.contactLink } }
    public var verified: Bool { activeConn?.connectionCode != nil }

    var directChatId: ChatId? {
        get {
            if let chatId = memberContactId {
                return "@\(chatId)"
            } else {
                return nil
            }
        }
    }

    public var chatViewName: String {
        get {
            let p = memberProfile
            return p.localAlias == ""
            ? p.displayName + (p.fullName == "" || p.fullName == p.displayName ? "" : " / \(p.fullName)")
            : p.localAlias
        }
    }

    public var memberActive: Bool {
        switch memberStatus {
        case .memRemoved: return false
        case .memLeft: return false
        case .memGroupDeleted: return false
        case .memInvited: return false
        case .memIntroduced: return false
        case .memIntroInvited: return false
        case .memAccepted: return false
        case .memAnnounced: return false
        case .memConnected: return true
        case .memComplete: return true
        case .memCreator: return true
        }
    }

    public var memberCurrent: Bool {
        switch memberStatus {
        case .memRemoved: return false
        case .memLeft: return false
        case .memGroupDeleted: return false
        case .memInvited: return false
        case .memIntroduced: return true
        case .memIntroInvited: return true
        case .memAccepted: return true
        case .memAnnounced: return true
        case .memConnected: return true
        case .memComplete: return true
        case .memCreator: return true
        }
    }

    public func canBeRemoved(groupInfo: GroupInfo) -> Bool {
        let userRole = groupInfo.membership.memberRole
        return memberStatus != .memRemoved && memberStatus != .memLeft
            && userRole >= .admin && userRole >= memberRole && groupInfo.membership.memberCurrent
    }

    public func canChangeRoleTo(groupInfo: GroupInfo) -> [GroupMemberRole]? {
        if !canBeRemoved(groupInfo: groupInfo) { return nil }
        let userRole = groupInfo.membership.memberRole
        return GroupMemberRole.allCases.filter { $0 <= userRole && $0 != .author }
    }

    public var memberIncognito: Bool {
        memberProfile.profileId != memberContactProfileId
    }

    public static let sampleData = GroupMember(
        groupMemberId: 1,
        groupId: 1,
        memberId: "abcd",
        memberRole: .admin,
        memberCategory: .inviteeMember,
        memberStatus: .memComplete,
        memberSettings: GroupMemberSettings(showMessages: true),
        invitedBy: .user,
        localDisplayName: "alice",
        memberProfile: LocalProfile.sampleData,
        memberContactId: 1,
        memberContactProfileId: 1,
        activeConn: Connection.sampleData
    )
}

public struct GroupMemberSettings: Codable {
    public var showMessages: Bool
}

public struct GroupMemberRef: Decodable {
    var groupMemberId: Int64
    var profile: Profile
}

public struct GroupMemberIds: Decodable {
    var groupMemberId: Int64
    var groupId: Int64
}

public enum GroupMemberRole: String, Identifiable, CaseIterable, Comparable, Decodable {
    case observer = "observer"
    case author = "author"
    case member = "member"
    case admin = "admin"
    case owner = "owner"

    public var id: Self { self }

    public var text: String {
        switch self {
        case .observer: return NSLocalizedString("observer", comment: "member role")
        case .author: return NSLocalizedString("author", comment: "member role")
        case .member: return NSLocalizedString("member", comment: "member role")
        case .admin: return NSLocalizedString("admin", comment: "member role")
        case .owner: return NSLocalizedString("owner", comment: "member role")
        }
    }

    private var comparisonValue: Int {
        switch self {
        case .observer: return 0
        case .author: return 1
        case .member: return 2
        case .admin: return 3
        case .owner: return 4
        }
    }

    public static func < (lhs: Self, rhs: Self) -> Bool {
        return lhs.comparisonValue < rhs.comparisonValue
    }
}

public enum GroupMemberCategory: String, Decodable {
    case userMember = "user"
    case inviteeMember = "invitee"
    case hostMember = "host"
    case preMember = "pre"
    case postMember = "post"
}

public enum GroupMemberStatus: String, Decodable {
    case memRemoved = "removed"
    case memLeft = "left"
    case memGroupDeleted = "deleted"
    case memInvited = "invited"
    case memIntroduced = "introduced"
    case memIntroInvited = "intro-inv"
    case memAccepted = "accepted"
    case memAnnounced = "announced"
    case memConnected = "connected"
    case memComplete = "complete"
    case memCreator = "creator"

    public var text: LocalizedStringKey {
        switch self {
        case .memRemoved: return "removed"
        case .memLeft: return "left"
        case .memGroupDeleted: return "group deleted"
        case .memInvited: return "invited"
        case .memIntroduced: return "connecting (introduced)"
        case .memIntroInvited: return "connecting (introduction invitation)"
        case .memAccepted: return "connecting (accepted)"
        case .memAnnounced: return "connecting (announced)"
        case .memConnected: return "member connected"
        case .memComplete: return "complete"
        case .memCreator: return "creator"
        }
    }

    public var shortText: LocalizedStringKey {
        switch self {
        case .memRemoved: return "removed"
        case .memLeft: return "left"
        case .memGroupDeleted: return "group deleted"
        case .memInvited: return "invited"
        case .memIntroduced: return "connecting"
        case .memIntroInvited: return "connecting"
        case .memAccepted: return "connecting"
        case .memAnnounced: return "connecting"
        case .memConnected: return "member connected"
        case .memComplete: return "complete"
        case .memCreator: return "creator"
        }
    }
}

public enum InvitedBy: Decodable {
    case contact(byContactId: Int64)
    case user
    case unknown
}

public struct MemberSubError: Decodable {
    var member: GroupMemberIds
    var memberError: ChatError
}

public enum ConnectionEntity: Decodable {
    case rcvDirectMsgConnection(contact: Contact?)
    case rcvGroupMsgConnection(groupInfo: GroupInfo, groupMember: GroupMember)
    case sndFileConnection(sndFileTransfer: SndFileTransfer)
    case rcvFileConnection(rcvFileTransfer: RcvFileTransfer)
    case userContactConnection(userContact: UserContact)

    public var id: String? {
        switch self {
        case let .rcvDirectMsgConnection(contact):
            return contact?.id ?? nil
        case let .rcvGroupMsgConnection(_, groupMember):
            return groupMember.id
        case let .userContactConnection(userContact):
            return userContact.id
        default:
            return nil
        }
    }

    public var ntfsEnabled: Bool {
        switch self {
        case let .rcvDirectMsgConnection(contact): return contact?.chatSettings.enableNtfs == .all
        case let .rcvGroupMsgConnection(groupInfo, _): return groupInfo.chatSettings.enableNtfs == .all
        case .sndFileConnection: return false
        case .rcvFileConnection: return false
        case let .userContactConnection(userContact): return userContact.groupId == nil
        }
    }
}

public struct NtfMsgInfo: Decodable {
    public var msgId: String
    public var msgTs: Date
}

public struct AChatItem: Decodable {
    public var chatInfo: ChatInfo
    public var chatItem: ChatItem

    public var chatId: String {
        if case let .groupRcv(groupMember) = chatItem.chatDir {
            return groupMember.id
        }
        return chatInfo.id
    }
}

public struct ACIReaction: Decodable {
    public var chatInfo: ChatInfo
    public var chatReaction: CIReaction
}

public struct CIReaction: Decodable {
    public var chatDir: CIDirection
    public var chatItem: ChatItem
    public var sentAt: Date
    public var reaction: MsgReaction
}

public struct ChatItem: Identifiable, Decodable {
    public init(chatDir: CIDirection, meta: CIMeta, content: CIContent, formattedText: [FormattedText]? = nil, quotedItem: CIQuote? = nil, reactions: [CIReactionCount] = [], file: CIFile? = nil) {
        self.chatDir = chatDir
        self.meta = meta
        self.content = content
        self.formattedText = formattedText
        self.quotedItem = quotedItem
        self.reactions = reactions
        self.file = file
    }

    public var chatDir: CIDirection
    public var meta: CIMeta
    public var content: CIContent
    public var formattedText: [FormattedText]?
    public var quotedItem: CIQuote?
    public var reactions: [CIReactionCount]
    public var file: CIFile?

    public var viewTimestamp = Date.now
    public var isLiveDummy: Bool = false

    private enum CodingKeys: String, CodingKey {
        case chatDir, meta, content, formattedText, quotedItem, reactions, file
    }

    public var id: Int64 { meta.itemId }

    public var viewId: String { "\(meta.itemId) \(viewTimestamp.timeIntervalSince1970)" }

    public var timestampText: Text { meta.timestampText }

    public var text: String {
        switch (content.text, content.msgContent, file) {
        case let ("", .some(.voice(_, duration)), _): return "Voice message (\(durationText(duration)))"
        case let ("", _, .some(file)): return file.fileName
        default: return content.text
        }
    }

    public var isRcvNew: Bool { meta.isRcvNew }

    public var isDeletedContent: Bool {
        switch content {
        case .sndDeleted: return true
        case .rcvDeleted: return true
        case .sndModerated: return true
        case .rcvModerated: return true
        default: return false
        }
    }

    public var memberConnected: GroupMember? {
        switch chatDir {
        case let .groupRcv(groupMember):
            switch content {
            case .rcvGroupEvent(rcvGroupEvent: .memberConnected): return groupMember
            default: return nil
            }
        default: return nil
        }
    }

    public var mergeCategory: CIMergeCategory? {
        switch content {
        case .rcvChatFeature: .chatFeature
        case .sndChatFeature: .chatFeature
        case .rcvGroupFeature: .chatFeature
        case .sndGroupFeature: .chatFeature
        case let.rcvGroupEvent(event):
            switch event {
            case .userRole: nil
            case .userDeleted: nil
            case .groupDeleted: nil
            case .memberCreatedContact: nil
            default: .rcvGroupEvent
            }
        case let .sndGroupEvent(event):
            switch event {
            case .userRole: nil
            case .userLeft: nil
            default: .sndGroupEvent
            }
        default:
            if meta.itemDeleted == nil {
                nil
            } else {
                chatDir.sent ? .sndItemDeleted : .rcvItemDeleted
            }
        }
    }

    private var showNtfDir: Bool {
        return !chatDir.sent
    }

    public var showNotification: Bool {
        switch content {
        case .sndMsgContent: return showNtfDir
        case .rcvMsgContent: return showNtfDir
        case .sndDeleted: return showNtfDir
        case .rcvDeleted: return showNtfDir
        case .sndCall: return showNtfDir
        case .rcvCall: return false // notification is shown on .callInvitation instead
        case .rcvIntegrityError: return showNtfDir
        case .rcvDecryptionError: return showNtfDir
        case .rcvGroupInvitation: return showNtfDir
        case .sndGroupInvitation: return showNtfDir
        case .rcvDirectEvent: return false
        case .rcvGroupEvent(rcvGroupEvent: let rcvGroupEvent):
            switch rcvGroupEvent {
            case .groupUpdated: return false
            case .memberConnected: return false
            case .memberRole: return false
            case .userRole: return showNtfDir
            case .userDeleted: return showNtfDir
            case .groupDeleted: return showNtfDir
            case .memberAdded: return false
            case .memberLeft: return false
            case .memberDeleted: return false
            case .invitedViaGroupLink: return false
            case .memberCreatedContact: return false
            }
        case .sndGroupEvent: return showNtfDir
        case .rcvConnEvent: return false
        case .sndConnEvent: return showNtfDir
        case .rcvChatFeature: return false
        case .sndChatFeature: return showNtfDir
        case .rcvChatPreference: return false
        case .sndChatPreference: return showNtfDir
        case .rcvGroupFeature: return false
        case .sndGroupFeature: return showNtfDir
        case .rcvChatFeatureRejected: return showNtfDir
        case .rcvGroupFeatureRejected: return showNtfDir
        case .sndModerated: return true
        case .rcvModerated: return true
        case .invalidJSON: return false
        }
    }

    public var allowAddReaction: Bool {
        meta.itemDeleted == nil && !isLiveDummy && reactions.filter({ $0.userReacted }).count < 3
    }

    public func autoReceiveFile() -> CIFile? {
        if let file = file,
           let mc = content.msgContent,
           privacyAcceptImagesGroupDefault.get(),
           (mc.isImage && file.fileSize <= MAX_IMAGE_SIZE_AUTO_RCV)
            || (mc.isVideo && file.fileSize <= MAX_VIDEO_SIZE_AUTO_RCV)
            || (mc.isVoice && file.fileSize <= MAX_VOICE_SIZE_AUTO_RCV && file.fileStatus != .rcvAccepted) {
            return file
        }
        return nil
    }

    public var encryptedFile: Bool? {
        guard let fileSource = file?.fileSource else { return nil }
        return fileSource.cryptoArgs != nil
    }

    public var encryptLocalFile: Bool {
        content.msgContent?.isVideo == false &&
        privacyEncryptLocalFilesGroupDefault.get()
    }

    public var memberDisplayName: String? {
        get {
            if case let .groupRcv(groupMember) = chatDir {
                return groupMember.chatViewName
            } else {
                return nil
            }
        }
    }

    public func memberToModerate(_ chatInfo: ChatInfo) -> (GroupInfo, GroupMember)? {
        switch (chatInfo, chatDir) {
        case let (.group(groupInfo), .groupRcv(groupMember)):
            let m = groupInfo.membership
            return m.memberRole >= .admin && m.memberRole >= groupMember.memberRole && meta.itemDeleted == nil
                    ? (groupInfo, groupMember)
                    : nil
        default: return nil
        }
    }

    public static func getSample (_ id: Int64, _ dir: CIDirection, _ ts: Date, _ text: String, _ status: CIStatus = .sndNew, quotedItem: CIQuote? = nil, file: CIFile? = nil, itemDeleted: CIDeleted? = nil, itemEdited: Bool = false, itemLive: Bool = false, editable: Bool = true) -> ChatItem {
        ChatItem(
            chatDir: dir,
            meta: CIMeta.getSample(id, ts, text, status, itemDeleted: itemDeleted, itemEdited: itemEdited, itemLive: itemLive, editable: editable),
            content: .sndMsgContent(msgContent: .text(text)),
            quotedItem: quotedItem,
            file: file
        )
    }

    public static func getVoiceMsgContentSample (id: Int64 = 1, text: String = "", fileName: String = "voice.m4a", fileSize: Int64 = 65536, fileStatus: CIFileStatus = .rcvComplete) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(id, .now, text, .rcvRead),
            content: .rcvMsgContent(msgContent: .voice(text: text, duration: 30)),
            quotedItem: nil,
            file: CIFile.getSample(fileName: fileName, fileSize: fileSize, fileStatus: fileStatus)
        )
    }

    public static func getFileMsgContentSample (id: Int64 = 1, text: String = "", fileName: String = "test.txt", fileSize: Int64 = 100, fileStatus: CIFileStatus = .rcvComplete) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(id, .now, text, .rcvRead),
            content: .rcvMsgContent(msgContent: .file(text)),
            quotedItem: nil,
            file: CIFile.getSample(fileName: fileName, fileSize: fileSize, fileStatus: fileStatus)
        )
    }

    public static func getDeletedContentSample (_ id: Int64 = 1, dir: CIDirection = .directRcv, _ ts: Date = .now, _ text: String = "this item is deleted", _ status: CIStatus = .rcvRead) -> ChatItem {
        ChatItem(
            chatDir: dir,
            meta: CIMeta.getSample(id, ts, text, status),
            content: .rcvDeleted(deleteMode: .cidmBroadcast),
            quotedItem: nil,
            file: nil
        )
    }

    public static func getIntegrityErrorSample (_ status: CIStatus = .rcvRead, fromMsgId: Int64 = 1, toMsgId: Int64 = 2) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "1 skipped message", status),
            content: .rcvIntegrityError(msgError: .msgSkipped(fromMsgId: fromMsgId, toMsgId: toMsgId)),
            quotedItem: nil,
            file: nil
        )
    }

    public static func getGroupInvitationSample (_ status: CIGroupInvitationStatus = .pending) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "received invitation to join group team as admin", .rcvRead),
            content: .rcvGroupInvitation(groupInvitation: CIGroupInvitation.getSample(status: status), memberRole: .admin),
            quotedItem: nil,
            file: nil
        )
    }

    public static func getGroupEventSample () -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "group event text", .rcvRead),
            content: .rcvGroupEvent(rcvGroupEvent: .memberAdded(groupMemberId: 1, profile: Profile.sampleData)),
            quotedItem: nil,
            file: nil
        )
    }

    public static func getChatFeatureSample(_ feature: ChatFeature, _ enabled: FeatureEnabled) -> ChatItem {
        let content = CIContent.rcvChatFeature(feature: feature, enabled: enabled, param: nil)
        return ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, content.text, .rcvRead),
            content: content,
            quotedItem: nil,
            file: nil
        )
    }

    public static func deletedItemDummy() -> ChatItem {
        ChatItem(
            chatDir: CIDirection.directRcv,
            meta: CIMeta(
                itemId: -1,
                itemTs: .now,
                itemText: NSLocalizedString("deleted", comment: "deleted chat item"),
                itemStatus: .rcvRead,
                createdAt: .now,
                updatedAt: .now,
                itemDeleted: nil,
                itemEdited: false,
                itemLive: false,
                editable: false
            ),
            content: .rcvDeleted(deleteMode: .cidmBroadcast),
            quotedItem: nil,
            file: nil
        )
    }

    public static func liveDummy(_ chatType: ChatType) -> ChatItem {
        var item = ChatItem(
            chatDir: chatType == ChatType.direct ? CIDirection.directSnd : CIDirection.groupSnd,
            meta: CIMeta(
                itemId: -2,
                itemTs: .now,
                itemText: "",
                itemStatus: .rcvRead,
                createdAt: .now,
                updatedAt: .now,
                itemDeleted: nil,
                itemEdited: false,
                itemLive: true,
                editable: false
            ),
            content: .sndMsgContent(msgContent: .text("")),
            quotedItem: nil,
            file: nil
        )
        item.isLiveDummy = true
        return item
    }

    public static func invalidJSON(chatDir: CIDirection?, meta: CIMeta?, json: String) -> ChatItem {
        ChatItem(
            chatDir: chatDir ?? .directSnd,
            meta: meta ?? .invalidJSON,
            content: .invalidJSON(json: json),
            quotedItem: nil,
            file: nil
        )
    }
}

public enum CIMergeCategory {
    case memberConnected
    case rcvGroupEvent
    case sndGroupEvent
    case sndItemDeleted
    case rcvItemDeleted
    case chatFeature
}

public enum CIDirection: Decodable {
    case directSnd
    case directRcv
    case groupSnd
    case groupRcv(groupMember: GroupMember)

    public var sent: Bool {
        get {
            switch self {
            case .directSnd: return true
            case .directRcv: return false
            case .groupSnd: return true
            case .groupRcv: return false
            }
        }
    }
}

public struct CIMeta: Decodable {
    public var itemId: Int64
    public var itemTs: Date
    var itemText: String
    public var itemStatus: CIStatus
    public var createdAt: Date
    public var updatedAt: Date
    public var itemDeleted: CIDeleted?
    public var itemEdited: Bool
    public var itemTimed: CITimed?
    public var itemLive: Bool?
    public var editable: Bool

    public var timestampText: Text { get { formatTimestampText(itemTs) } }
    public var recent: Bool { updatedAt + 10 > .now }
    public var isLive: Bool { itemLive == true }
    public var disappearing: Bool { !isRcvNew && itemTimed?.deleteAt != nil }

    public var isRcvNew: Bool {
        if case .rcvNew = itemStatus { return true }
        return false
    }

    public func statusIcon(_ metaColor: Color = .secondary) -> (String, Color)? {
        itemStatus.statusIcon(metaColor)
    }

    public static func getSample(_ id: Int64, _ ts: Date, _ text: String, _ status: CIStatus = .sndNew, itemDeleted: CIDeleted? = nil, itemEdited: Bool = false, itemLive: Bool = false, editable: Bool = true) -> CIMeta {
        CIMeta(
            itemId: id,
            itemTs: ts,
            itemText: text,
            itemStatus: status,
            createdAt: ts,
            updatedAt: ts,
            itemDeleted: itemDeleted,
            itemEdited: itemEdited,
            itemLive: itemLive,
            editable: editable
        )
    }

    public static var invalidJSON: CIMeta {
        CIMeta(
            itemId: 0,
            itemTs: .now,
            itemText: "invalid JSON",
            itemStatus: .sndNew,
            createdAt: .now,
            updatedAt: .now,
            itemDeleted: nil,
            itemEdited: false,
            itemLive: false,
            editable: false
        )
    }
}

public struct CITimed: Decodable {
    public var ttl: Int
    public var deleteAt: Date?
}

let msgTimeFormat = Date.FormatStyle.dateTime.hour().minute()
let msgDateFormat = Date.FormatStyle.dateTime.day(.twoDigits).month(.twoDigits)

public func formatTimestampText(_ date: Date) -> Text {
    return Text(date, format: recent(date) ? msgTimeFormat : msgDateFormat)
}

private func recent(_ date: Date) -> Bool {
    let now = Date()
    let calendar = Calendar.current

    guard let previousDay = calendar.date(byAdding: DateComponents(day: -1), to: now),
          let previousDay18 = calendar.date(bySettingHour: 18, minute: 0, second: 0, of: previousDay),
          let currentDay00 = calendar.date(bySettingHour: 0, minute: 0, second: 0, of: now),
          let currentDay12 = calendar.date(bySettingHour: 12, minute: 0, second: 0, of: now) else {
        return false
    }

    let isSameDay = calendar.isDate(date, inSameDayAs: now)
    return isSameDay || (now < currentDay12 && date >= previousDay18 && date < currentDay00)
}

public enum CIStatus: Decodable {
    case sndNew
    case sndSent(sndProgress: SndCIStatusProgress)
    case sndRcvd(msgRcptStatus: MsgReceiptStatus, sndProgress: SndCIStatusProgress)
    case sndErrorAuth
    case sndError(agentError: String)
    case rcvNew
    case rcvRead
    case invalid(text: String)

    var id: String {
        switch self {
        case .sndNew: return "sndNew"
        case .sndSent: return "sndSent"
        case .sndRcvd: return "sndRcvd"
        case .sndErrorAuth: return "sndErrorAuth"
        case .sndError: return "sndError"
        case .rcvNew: return "rcvNew"
        case .rcvRead: return "rcvRead"
        case .invalid: return "invalid"
        }
    }

    public func statusIcon(_ metaColor: Color = .secondary) -> (String, Color)? {
        switch self {
        case .sndNew: return nil
        case .sndSent: return ("checkmark", metaColor)
        case let .sndRcvd(msgRcptStatus, _):
            switch msgRcptStatus {
            case .ok: return ("checkmark", metaColor)
            case .badMsgHash: return ("checkmark", .red)
            }
        case .sndErrorAuth: return ("multiply", .red)
        case .sndError: return ("exclamationmark.triangle.fill", .yellow)
        case .rcvNew: return ("circlebadge.fill", Color.accentColor)
        case .rcvRead: return nil
        case .invalid: return ("questionmark", metaColor)
        }
    }

    public var statusInfo: (String, String)? {
        switch self {
        case .sndNew: return nil
        case .sndSent: return nil
        case .sndRcvd: return nil
        case .sndErrorAuth: return (
                NSLocalizedString("Message delivery error", comment: "item status text"),
                NSLocalizedString("Most likely this connection is deleted.", comment: "item status description")
            )
        case let .sndError(agentError): return (
                NSLocalizedString("Message delivery error", comment: "item status text"),
                String.localizedStringWithFormat(NSLocalizedString("Unexpected error: %@", comment: "item status description"), agentError)
            )
        case .rcvNew: return nil
        case .rcvRead: return nil
        case let .invalid(text): return (
                NSLocalizedString("Invalid status", comment: "item status text"),
                text
            )
        }
    }
}

public enum MsgReceiptStatus: String, Decodable {
    case ok
    case badMsgHash
}

public enum SndCIStatusProgress: String, Decodable {
    case partial
    case complete
}

public enum CIDeleted: Decodable {
    case deleted(deletedTs: Date?)
    case blocked(deletedTs: Date?)
    case moderated(deletedTs: Date?, byGroupMember: GroupMember)

    var id: String {
        switch self {
        case .deleted: return  "deleted"
        case .blocked: return  "blocked"
        case .moderated: return "moderated"
        }
    }
}

public enum CIDeleteMode: String, Decodable {
    case cidmBroadcast = "broadcast"
    case cidmInternal = "internal"
}

protocol ItemContent {
    var text: String { get }
}

public enum CIContent: Decodable, ItemContent {
    case sndMsgContent(msgContent: MsgContent)
    case rcvMsgContent(msgContent: MsgContent)
    case sndDeleted(deleteMode: CIDeleteMode) // legacy - since v4.3.0 itemDeleted field is used
    case rcvDeleted(deleteMode: CIDeleteMode) // legacy - since v4.3.0 itemDeleted field is used
    case sndCall(status: CICallStatus, duration: Int)
    case rcvCall(status: CICallStatus, duration: Int)
    case rcvIntegrityError(msgError: MsgErrorType)
    case rcvDecryptionError(msgDecryptError: MsgDecryptError, msgCount: UInt32)
    case rcvGroupInvitation(groupInvitation: CIGroupInvitation, memberRole: GroupMemberRole)
    case sndGroupInvitation(groupInvitation: CIGroupInvitation, memberRole: GroupMemberRole)
    case rcvDirectEvent(rcvDirectEvent: RcvDirectEvent)
    case rcvGroupEvent(rcvGroupEvent: RcvGroupEvent)
    case sndGroupEvent(sndGroupEvent: SndGroupEvent)
    case rcvConnEvent(rcvConnEvent: RcvConnEvent)
    case sndConnEvent(sndConnEvent: SndConnEvent)
    case rcvChatFeature(feature: ChatFeature, enabled: FeatureEnabled, param: Int?)
    case sndChatFeature(feature: ChatFeature, enabled: FeatureEnabled, param: Int?)
    case rcvChatPreference(feature: ChatFeature, allowed: FeatureAllowed, param: Int?)
    case sndChatPreference(feature: ChatFeature, allowed: FeatureAllowed, param: Int?)
    case rcvGroupFeature(groupFeature: GroupFeature, preference: GroupPreference, param: Int?)
    case sndGroupFeature(groupFeature: GroupFeature, preference: GroupPreference, param: Int?)
    case rcvChatFeatureRejected(feature: ChatFeature)
    case rcvGroupFeatureRejected(groupFeature: GroupFeature)
    case sndModerated
    case rcvModerated
    case invalidJSON(json: String)

    public var text: String {
        get {
            switch self {
            case let .sndMsgContent(mc): return mc.text
            case let .rcvMsgContent(mc): return mc.text
            case .sndDeleted: return NSLocalizedString("deleted", comment: "deleted chat item")
            case .rcvDeleted: return NSLocalizedString("deleted", comment: "deleted chat item")
            case let .sndCall(status, duration): return status.text(duration)
            case let .rcvCall(status, duration): return status.text(duration)
            case let .rcvIntegrityError(msgError): return msgError.text
            case let .rcvDecryptionError(msgDecryptError, _): return msgDecryptError.text
            case let .rcvGroupInvitation(groupInvitation, _): return groupInvitation.text
            case let .sndGroupInvitation(groupInvitation, _): return groupInvitation.text
            case let .rcvDirectEvent(rcvDirectEvent): return rcvDirectEvent.text
            case let .rcvGroupEvent(rcvGroupEvent): return rcvGroupEvent.text
            case let .sndGroupEvent(sndGroupEvent): return sndGroupEvent.text
            case let .rcvConnEvent(rcvConnEvent): return rcvConnEvent.text
            case let .sndConnEvent(sndConnEvent): return sndConnEvent.text
            case let .rcvChatFeature(feature, enabled, param): return CIContent.featureText(feature, enabled.text, param)
            case let .sndChatFeature(feature, enabled, param): return CIContent.featureText(feature, enabled.text, param)
            case let .rcvChatPreference(feature, allowed, param): return CIContent.preferenceText(feature, allowed, param)
            case let .sndChatPreference(feature, allowed, param): return CIContent.preferenceText(feature, allowed, param)
            case let .rcvGroupFeature(feature, preference, param): return CIContent.featureText(feature, preference.enable.text, param)
            case let .sndGroupFeature(feature, preference, param): return CIContent.featureText(feature, preference.enable.text, param)
            case let .rcvChatFeatureRejected(feature): return String.localizedStringWithFormat("%@: received, prohibited", feature.text)
            case let .rcvGroupFeatureRejected(groupFeature): return String.localizedStringWithFormat("%@: received, prohibited", groupFeature.text)
            case .sndModerated: return NSLocalizedString("moderated", comment: "moderated chat item")
            case .rcvModerated: return NSLocalizedString("moderated", comment: "moderated chat item")
            case .invalidJSON: return NSLocalizedString("invalid data", comment: "invalid chat item")
            }
        }
    }

    static func featureText(_ feature: Feature, _ enabled: String, _ param: Int?) -> String {
        feature.hasParam
        ? "\(feature.text): \(timeText(param))"
        : "\(feature.text): \(enabled)"
    }

    public static func preferenceText(_ feature: Feature, _ allowed: FeatureAllowed, _ param: Int?) -> String {
        allowed != .no && feature.hasParam && param != nil
        ? String.localizedStringWithFormat(NSLocalizedString("offered %@: %@", comment: "feature offered item"), feature.text, timeText(param))
        : allowed != .no
        ? String.localizedStringWithFormat(NSLocalizedString("offered %@", comment: "feature offered item"), feature.text)
        : String.localizedStringWithFormat(NSLocalizedString("cancelled %@", comment: "feature offered item"), feature.text)
    }

    public var msgContent: MsgContent? {
        get {
            switch self {
            case let .sndMsgContent(mc): return mc
            case let .rcvMsgContent(mc): return mc
            default: return nil
            }
        }
    }

    public var showMemberName: Bool {
        switch self {
        case .rcvMsgContent: return true
        case .rcvDeleted: return true
        case .rcvCall: return true
        case .rcvIntegrityError: return true
        case .rcvDecryptionError: return true
        case .rcvGroupInvitation: return true
        case .rcvModerated: return true
        case .invalidJSON: return true
        default: return false
        }
    }
}

public enum MsgDecryptError: String, Decodable {
    case ratchetHeader
    case tooManySkipped
    case ratchetEarlier
    case other
    case ratchetSync

    var text: String {
        switch self {
        case .ratchetHeader: return NSLocalizedString("Permanent decryption error", comment: "message decrypt error item")
        case .tooManySkipped: return NSLocalizedString("Permanent decryption error", comment: "message decrypt error item")
        case .ratchetEarlier: return NSLocalizedString("Decryption error", comment: "message decrypt error item")
        case .other: return NSLocalizedString("Decryption error", comment: "message decrypt error item")
        case .ratchetSync: return NSLocalizedString("Encryption re-negotiation error", comment: "message decrypt error item")
        }
    }
}

public struct CIQuote: Decodable, ItemContent {
    public var chatDir: CIDirection?
    public var itemId: Int64?
    var sharedMsgId: String? = nil
    public var sentAt: Date
    public var content: MsgContent
    public var formattedText: [FormattedText]?

    public var text: String {
        switch (content.text, content) {
        case let ("", .voice(_, duration)): return durationText(duration)
        default: return content.text
        }
    }

    public func getSender(_ membership: GroupMember?) -> String? {
        switch (chatDir) {
        case .directSnd: return "you"
        case .directRcv: return nil
        case .groupSnd: return membership?.displayName ?? "you"
        case let .groupRcv(member): return member.displayName
        case nil: return nil
        }
    }

    public static func getSample(_ itemId: Int64?, _ sentAt: Date, _ text: String, chatDir: CIDirection?, image: String? = nil) -> CIQuote {
        let mc: MsgContent
        if let image = image {
            mc = .image(text: text, image: image)
        } else {
            mc = .text(text)
        }
        return CIQuote(chatDir: chatDir, itemId: itemId, sentAt: sentAt, content: mc)
    }
}

public struct CIReactionCount: Decodable {
    public var reaction: MsgReaction
    public var userReacted: Bool
    public var totalReacted: Int
}

public enum MsgReaction: Hashable {
    case emoji(emoji: MREmojiChar)
    case unknown(type: String)

    public var text: String {
        switch self {
        case let .emoji(emoji):
            switch emoji {
            case .heart: return "❤️"
            default: return emoji.rawValue
            }
        case .unknown: return "?"
        }
    }

    public static var values: [MsgReaction] = MREmojiChar.allCases.map { .emoji(emoji: $0) }

    enum CodingKeys: String, CodingKey {
        case type
        case emoji
    }
}

public enum MREmojiChar: String, Codable, CaseIterable {
    case thumbsup = "👍"
    case thumbsdown = "👎"
    case smile = "😀"
    case sad = "😢"
    case heart = "❤"
    case launch = "🚀"
}

extension MsgReaction: Decodable {
    public init(from decoder: Decoder) throws {
        do {
            let container = try decoder.container(keyedBy: CodingKeys.self)
            let type = try container.decode(String.self, forKey: CodingKeys.type)
            switch type {
            case "emoji":
                let emoji = try container.decode(MREmojiChar.self, forKey: CodingKeys.emoji)
                self = .emoji(emoji: emoji)
            default:
                self = .unknown(type: type)
            }
        } catch {
            self = .unknown(type: "")
        }
    }
}

extension MsgReaction: Encodable {
    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case let .emoji(emoji):
            try container.encode("emoji", forKey: .type)
            try container.encode(emoji, forKey: .emoji)
        // TODO use original JSON and type
        case let .unknown(type):
            try container.encode(type, forKey: .type)
        }
    }
}

public struct CIFile: Decodable {
    public var fileId: Int64
    public var fileName: String
    public var fileSize: Int64
    public var fileSource: CryptoFile?
    public var fileStatus: CIFileStatus
    public var fileProtocol: FileProtocol

    public static func getSample(fileId: Int64 = 1, fileName: String = "test.txt", fileSize: Int64 = 100, filePath: String? = "test.txt", fileStatus: CIFileStatus = .rcvComplete) -> CIFile {
        let f: CryptoFile?
        if let filePath = filePath {
            f = CryptoFile.plain(filePath)
        } else {
            f = nil
        }
        return CIFile(fileId: fileId, fileName: fileName, fileSize: fileSize, fileSource: f, fileStatus: fileStatus, fileProtocol: .xftp)
    }

    public var loaded: Bool {
        get {
            switch self.fileStatus {
            case .sndStored: return true
            case .sndTransfer: return true
            case .sndComplete: return true
            case .sndCancelled: return true
            case .sndError: return true
            case .rcvInvitation: return false
            case .rcvAccepted: return false
            case .rcvTransfer: return false
            case .rcvCancelled: return false
            case .rcvComplete: return true
            case .rcvError: return false
            case .invalid: return false
            }
        }
    }

    public var cancelAction: CancelAction? {
        get {
            switch self.fileStatus {
            case .sndStored: return sndCancelAction
            case .sndTransfer: return sndCancelAction
            case .sndComplete:
                if self.fileProtocol == .xftp {
                    return revokeCancelAction
                } else {
                    return nil
                }
            case .sndCancelled: return nil
            case .sndError: return nil
            case .rcvInvitation: return nil
            case .rcvAccepted: return rcvCancelAction
            case .rcvTransfer: return rcvCancelAction
            case .rcvCancelled: return nil
            case .rcvComplete: return nil
            case .rcvError: return nil
            case .invalid: return nil
            }
        }
    }
}

public struct CryptoFile: Codable {
    public var filePath: String // the name of the file, not a full path
    public var cryptoArgs: CryptoFileArgs?

    public init(filePath: String, cryptoArgs: CryptoFileArgs?) {
        self.filePath = filePath
        self.cryptoArgs = cryptoArgs
    }

    public static func plain(_ f: String) -> CryptoFile {
        CryptoFile(filePath: f, cryptoArgs: nil)
    }
}

public struct CryptoFileArgs: Codable {
    public var fileKey: String
    public var fileNonce: String
}

public struct CancelAction {
    public var uiAction: String
    public var alert: AlertInfo
}

public struct AlertInfo {
    public var title: LocalizedStringKey
    public var message: LocalizedStringKey
    public var confirm: LocalizedStringKey
}

private var sndCancelAction = CancelAction(
    uiAction: NSLocalizedString("Stop file", comment: "cancel file action"),
    alert: AlertInfo(
        title: "Stop sending file?",
        message: "Sending file will be stopped.",
        confirm: "Stop"
    )
)

private var revokeCancelAction = CancelAction(
    uiAction: NSLocalizedString("Revoke file", comment: "cancel file action"),
    alert: AlertInfo(
        title: "Revoke file?",
        message: "File will be deleted from servers.",
        confirm: "Revoke"
    )
)

private var rcvCancelAction = CancelAction(
    uiAction: NSLocalizedString("Stop file", comment: "cancel file action"),
    alert: AlertInfo(
        title: "Stop receiving file?",
        message: "Receiving file will be stopped.",
        confirm: "Stop"
    )
)

public enum FileProtocol: String, Decodable {
    case smp = "smp"
    case xftp = "xftp"
}

public enum CIFileStatus: Decodable, Equatable {
    case sndStored
    case sndTransfer(sndProgress: Int64, sndTotal: Int64)
    case sndComplete
    case sndCancelled
    case sndError
    case rcvInvitation
    case rcvAccepted
    case rcvTransfer(rcvProgress: Int64, rcvTotal: Int64)
    case rcvComplete
    case rcvCancelled
    case rcvError
    case invalid(text: String)

    var id: String {
        switch self {
        case .sndStored: return "sndStored"
        case let .sndTransfer(sndProgress, sndTotal): return "sndTransfer \(sndProgress) \(sndTotal)"
        case .sndComplete: return "sndComplete"
        case .sndCancelled: return "sndCancelled"
        case .sndError: return "sndError"
        case .rcvInvitation: return "rcvInvitation"
        case .rcvAccepted: return "rcvAccepted"
        case let .rcvTransfer(rcvProgress, rcvTotal): return "rcvTransfer \(rcvProgress) \(rcvTotal)"
        case .rcvComplete: return "rcvComplete"
        case .rcvCancelled: return "rcvCancelled"
        case .rcvError: return "rcvError"
        case .invalid: return "invalid"
        }
    }
}

public enum MsgContent: Equatable {
    case text(String)
    case link(text: String, preview: LinkPreview)
    case image(text: String, image: String)
    case video(text: String, image: String, duration: Int)
    case voice(text: String, duration: Int)
    case file(String)
    // TODO include original JSON, possibly using https://github.com/zoul/generic-json-swift
    case unknown(type: String, text: String)

    public var text: String {
        switch self {
        case let .text(text): return text
        case let .link(text, _): return text
        case let .image(text, _): return text
        case let .video(text, _, _): return text
        case let .voice(text, _): return text
        case let .file(text): return text
        case let .unknown(_, text): return text
        }
    }

    public var isText: Bool {
        switch self {
        case .text: return true
        default: return false
        }
    }

    public var isVoice: Bool {
        switch self {
        case .voice: return true
        default: return false
        }
    }

    public var isImage: Bool {
        switch self {
        case .image: return true
        default: return false
        }
    }

    public var isVideo: Bool {
        switch self {
        case .video: return true
        default: return false
        }
    }

    var cmdString: String {
        "json \(encodeJSON(self))"
    }

    enum CodingKeys: String, CodingKey {
        case type
        case text
        case preview
        case image
        case duration
    }

    public static func == (lhs: MsgContent, rhs: MsgContent) -> Bool {
        switch (lhs, rhs) {
        case let (.text(lt), .text(rt)): return lt == rt
        case let (.link(lt, lp), .link(rt, rp)): return lt == rt && lp == rp
        case let (.image(lt, li), .image(rt, ri)): return lt == rt && li == ri
        case let (.video(lt, li, ld), .video(rt, ri, rd)): return lt == rt && li == ri && ld == rd
        case let (.voice(lt, ld), .voice(rt, rd)): return lt == rt && ld == rd
        case let (.file(lf), .file(rf)): return lf == rf
        case let (.unknown(lType, lt), .unknown(rType, rt)): return lType == rType && lt == rt
        default: return false
        }
    }
}

extension MsgContent: Decodable {
    public init(from decoder: Decoder) throws {
        do {
            let container = try decoder.container(keyedBy: CodingKeys.self)
            let type = try container.decode(String.self, forKey: CodingKeys.type)
            switch type {
            case "text":
                let text = try container.decode(String.self, forKey: CodingKeys.text)
                self = .text(text)
            case "link":
                let text = try container.decode(String.self, forKey: CodingKeys.text)
                let preview = try container.decode(LinkPreview.self, forKey: CodingKeys.preview)
                self = .link(text: text, preview: preview)
            case "image":
                let text = try container.decode(String.self, forKey: CodingKeys.text)
                let image = try container.decode(String.self, forKey: CodingKeys.image)
                self = .image(text: text, image: image)
            case "video":
                let text = try container.decode(String.self, forKey: CodingKeys.text)
                let image = try container.decode(String.self, forKey: CodingKeys.image)
                let duration = try container.decode(Int.self, forKey: CodingKeys.duration)
                self = .video(text: text, image: image, duration: duration)
            case "voice":
                let text = try container.decode(String.self, forKey: CodingKeys.text)
                let duration = try container.decode(Int.self, forKey: CodingKeys.duration)
                self = .voice(text: text, duration: duration)
            case "file":
                let text = try container.decode(String.self, forKey: CodingKeys.text)
                self = .file(text)
            default:
                let text = try? container.decode(String.self, forKey: CodingKeys.text)
                self = .unknown(type: type, text: text ?? "unknown message format")
            }
        } catch {
            self = .unknown(type: "unknown", text: "invalid message format")
        }
    }
}

extension MsgContent: Encodable {
    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case let .text(text):
            try container.encode("text", forKey: .type)
            try container.encode(text, forKey: .text)
        case let .link(text, preview):
            try container.encode("link", forKey: .type)
            try container.encode(text, forKey: .text)
            try container.encode(preview, forKey: .preview)
        case let .image(text, image):
            try container.encode("image", forKey: .type)
            try container.encode(text, forKey: .text)
            try container.encode(image, forKey: .image)
        case let .video(text, image, duration):
            try container.encode("video", forKey: .type)
            try container.encode(text, forKey: .text)
            try container.encode(image, forKey: .image)
            try container.encode(duration, forKey: .duration)
        case let .voice(text, duration):
            try container.encode("voice", forKey: .type)
            try container.encode(text, forKey: .text)
            try container.encode(duration, forKey: .duration)
        case let .file(text):
            try container.encode("file", forKey: .type)
            try container.encode(text, forKey: .text)
        // TODO use original JSON and type
        case let .unknown(_, text):
            try container.encode("text", forKey: .type)
            try container.encode(text, forKey: .text)
        }
    }
}

public struct FormattedText: Decodable {
    public var text: String
    public var format: Format?
}

public enum Format: Decodable, Equatable {
    case bold
    case italic
    case strikeThrough
    case snippet
    case secret
    case colored(color: FormatColor)
    case uri
    case simplexLink(linkType: SimplexLinkType, simplexUri: String, smpHosts: [String])
    case email
    case phone
}

public enum SimplexLinkType: String, Decodable {
    case contact
    case invitation
    case group

    public var description: String {
        switch self {
        case .contact: return NSLocalizedString("SimpleX contact address", comment: "simplex link type")
        case .invitation: return NSLocalizedString("SimpleX one-time invitation", comment: "simplex link type")
        case .group: return NSLocalizedString("SimpleX group link", comment: "simplex link type")
        }
    }
}

public enum FormatColor: String, Decodable {
    case red = "red"
    case green = "green"
    case blue = "blue"
    case yellow = "yellow"
    case cyan = "cyan"
    case magenta = "magenta"
    case black = "black"
    case white = "white"

    public var uiColor: Color {
        get {
            switch (self) {
            case .red: return .red
            case .green: return .green
            case .blue: return .blue
            case .yellow: return .yellow
            case .cyan: return .cyan
            case .magenta: return .purple
            case .black: return .primary
            case .white: return .primary
            }
        }
    }
}

// Struct to use with simplex API
public struct LinkPreview: Codable, Equatable {
    public init(uri: URL, title: String, description: String = "", image: String) {
        self.uri = uri
        self.title = title
        self.description = description
        self.image = image
    }
    
    public var uri: URL
    public var title: String
    // TODO remove once optional in haskell
    public var description: String = ""
    public var image: String
}

public enum NtfTknStatus: String, Decodable {
    case new = "NEW"
    case registered = "REGISTERED"
    case invalid = "INVALID"
    case confirmed = "CONFIRMED"
    case active = "ACTIVE"
    case expired = "EXPIRED"
}

public struct SndFileTransfer: Decodable {

}

public struct RcvFileTransfer: Decodable {

}

public struct FileTransferMeta: Decodable {
    
}

public enum CICallStatus: String, Decodable {
    case pending
    case missed
    case rejected
    case accepted
    case negotiated
    case progress
    case ended
    case error

    func text(_ sec: Int) -> String {
        switch self {
        case .pending: return NSLocalizedString("calling…", comment: "call status")
        case .missed: return NSLocalizedString("missed call", comment: "call status")
        case .rejected: return NSLocalizedString("rejected call", comment: "call status")
        case .accepted: return NSLocalizedString("accepted call", comment: "call status")
        case .negotiated: return NSLocalizedString("connecting call", comment: "call status")
        case .progress: return NSLocalizedString("call in progress", comment: "call status")
        case .ended: return String.localizedStringWithFormat(NSLocalizedString("ended call %@", comment: "call status"), durationText(sec))
        case .error: return NSLocalizedString("call error", comment: "call status")
        }
    }
}

public func durationText(_ sec: Int) -> String {
    let s = sec % 60
    let m = sec / 60
    return m < 60
        ? String(format: "%02d:%02d", m, s)
        : String(format: "%02d:%02d:%02d", m / 60, m % 60, s)
}

public enum MsgErrorType: Decodable {
    case msgSkipped(fromMsgId: Int64, toMsgId: Int64)
    case msgBadId(msgId: Int64)
    case msgBadHash
    case msgDuplicate

    var text: String {
        switch self {
        case let .msgSkipped(fromMsgId, toMsgId):
            return String.localizedStringWithFormat(NSLocalizedString("%d skipped message(s)", comment: "integrity error chat item"), toMsgId - fromMsgId + 1)
        case .msgBadHash: return NSLocalizedString("bad message hash", comment: "integrity error chat item") // not used now
        case .msgBadId: return NSLocalizedString("bad message ID", comment: "integrity error chat item") // not used now
        case .msgDuplicate: return NSLocalizedString("duplicate message", comment: "integrity error chat item") // not used now
        }
    }
}

public struct CIGroupInvitation: Decodable {
    public var groupId: Int64
    public var groupMemberId: Int64
    public var localDisplayName: GroupName
    public var groupProfile: GroupProfile
    public var status: CIGroupInvitationStatus

    var text: String {
        String.localizedStringWithFormat(NSLocalizedString("invitation to group %@", comment: "group name"), groupProfile.displayName)
    }

    public static func getSample(groupId: Int64 = 1, groupMemberId: Int64 = 1, localDisplayName: GroupName = "team", groupProfile: GroupProfile = GroupProfile.sampleData, status: CIGroupInvitationStatus = .pending) -> CIGroupInvitation {
        CIGroupInvitation(groupId: groupId, groupMemberId: groupMemberId, localDisplayName: localDisplayName, groupProfile: groupProfile, status: status)
    }
}

public enum CIGroupInvitationStatus: String, Decodable {
    case pending
    case accepted
    case rejected
    case expired
}

public enum RcvDirectEvent: Decodable {
    case contactDeleted

    var text: String {
        switch self {
        case .contactDeleted: return NSLocalizedString("deleted contact", comment: "rcv direct event chat item")
        }
    }
}

public enum RcvGroupEvent: Decodable {
    case memberAdded(groupMemberId: Int64, profile: Profile)
    case memberConnected
    case memberLeft
    case memberRole(groupMemberId: Int64, profile: Profile, role: GroupMemberRole)
    case userRole(role: GroupMemberRole)
    case memberDeleted(groupMemberId: Int64, profile: Profile)
    case userDeleted
    case groupDeleted
    case groupUpdated(groupProfile: GroupProfile)
    case invitedViaGroupLink
    case memberCreatedContact

    var text: String {
        switch self {
        case let .memberAdded(_, profile):
            return String.localizedStringWithFormat(NSLocalizedString("invited %@", comment: "rcv group event chat item"), profile.profileViewName)
        case .memberConnected: return NSLocalizedString("member connected", comment: "rcv group event chat item")
        case .memberLeft: return NSLocalizedString("left", comment: "rcv group event chat item")
        case let .memberRole(_, profile, role):
            return  String.localizedStringWithFormat(NSLocalizedString("changed role of %@ to %@", comment: "rcv group event chat item"), profile.profileViewName, role.text)
        case let .userRole(role):
            return String.localizedStringWithFormat(NSLocalizedString("changed your role to %@", comment: "rcv group event chat item"), role.text)
        case let .memberDeleted(_, profile):
            return String.localizedStringWithFormat(NSLocalizedString("removed %@", comment: "rcv group event chat item"), profile.profileViewName)
        case .userDeleted: return NSLocalizedString("removed you", comment: "rcv group event chat item")
        case .groupDeleted: return NSLocalizedString("deleted group", comment: "rcv group event chat item")
        case .groupUpdated: return NSLocalizedString("updated group profile", comment: "rcv group event chat item")
        case .invitedViaGroupLink: return NSLocalizedString("invited via your group link", comment: "rcv group event chat item")
        case .memberCreatedContact: return NSLocalizedString("connected directly", comment: "rcv group event chat item")
        }
    }
}

public enum SndGroupEvent: Decodable {
    case memberRole(groupMemberId: Int64, profile: Profile, role: GroupMemberRole)
    case userRole(role: GroupMemberRole)
    case memberDeleted(groupMemberId: Int64, profile: Profile)
    case userLeft
    case groupUpdated(groupProfile: GroupProfile)

    var text: String {
        switch self {
        case let .memberRole(_, profile, role):
            return  String.localizedStringWithFormat(NSLocalizedString("you changed role of %@ to %@", comment: "snd group event chat item"), profile.profileViewName, role.text)
        case let .userRole(role):
            return String.localizedStringWithFormat(NSLocalizedString("you changed role for yourself to %@", comment: "snd group event chat item"), role.text)
        case let .memberDeleted(_, profile):
            return String.localizedStringWithFormat(NSLocalizedString("you removed %@", comment: "snd group event chat item"), profile.profileViewName)
        case .userLeft: return NSLocalizedString("you left", comment: "snd group event chat item")
        case .groupUpdated: return NSLocalizedString("group profile updated", comment: "snd group event chat item")
        }
    }
}

public enum RcvConnEvent: Decodable {
    case switchQueue(phase: SwitchPhase)
    case ratchetSync(syncStatus: RatchetSyncState)
    case verificationCodeReset
    
    var text: String {
        switch self {
        case let .switchQueue(phase):
            if case .completed = phase {
                return NSLocalizedString("changed address for you", comment: "chat item text")
            }
            return NSLocalizedString("changing address…", comment: "chat item text")
        case let .ratchetSync(syncStatus):
            return ratchetSyncStatusToText(syncStatus)
        case .verificationCodeReset:
            return NSLocalizedString("security code changed", comment: "chat item text")
        }
    }
}

func ratchetSyncStatusToText(_ ratchetSyncStatus: RatchetSyncState) -> String {
    switch ratchetSyncStatus {
    case .ok: return NSLocalizedString("encryption ok", comment: "chat item text")
    case .allowed: return NSLocalizedString("encryption re-negotiation allowed", comment: "chat item text")
    case .required: return NSLocalizedString("encryption re-negotiation required", comment: "chat item text")
    case .started: return NSLocalizedString("agreeing encryption…", comment: "chat item text")
    case .agreed: return NSLocalizedString("encryption agreed", comment: "chat item text")
    }
}

public enum SndConnEvent: Decodable {
    case switchQueue(phase: SwitchPhase, member: GroupMemberRef?)
    case ratchetSync(syncStatus: RatchetSyncState, member: GroupMemberRef?)

    var text: String {
        switch self {
        case let .switchQueue(phase, member):
            if let name = member?.profile.profileViewName {
                return phase == .completed
                ? String.localizedStringWithFormat(NSLocalizedString("you changed address for %@", comment: "chat item text"), name)
                : String.localizedStringWithFormat(NSLocalizedString("changing address for %@…", comment: "chat item text"), name)
            }
            return phase == .completed
            ? NSLocalizedString("you changed address", comment: "chat item text")
            : NSLocalizedString("changing address…", comment: "chat item text")
        case let .ratchetSync(syncStatus, member):
            if let name = member?.profile.profileViewName {
                switch syncStatus {
                case .ok: return String.localizedStringWithFormat(NSLocalizedString("encryption ok for %@", comment: "chat item text"), name)
                case .allowed: return String.localizedStringWithFormat(NSLocalizedString("encryption re-negotiation allowed for %@", comment: "chat item text"), name)
                case .required: return String.localizedStringWithFormat(NSLocalizedString("encryption re-negotiation required for %@", comment: "chat item text"), name)
                case .started: return String.localizedStringWithFormat(NSLocalizedString("agreeing encryption for %@…", comment: "chat item text"), name)
                case .agreed: return String.localizedStringWithFormat(NSLocalizedString("encryption agreed for %@", comment: "chat item text"), name)
                }
            }
            return ratchetSyncStatusToText(syncStatus)
        }
    }
}

public enum SwitchPhase: String, Decodable {
    case started
    case confirmed
    case secured
    case completed
}

public enum ChatItemTTL: Hashable, Identifiable, Comparable {
    case day
    case week
    case month
    case seconds(_ seconds: Int64)
    case none

    public static var values: [ChatItemTTL] { [.none, .month, .week, .day] }

    public var id: Self { self }

    public init(_ seconds: Int64?) {
        switch seconds {
        case 86400: self = .day
        case 7 * 86400: self = .week
        case 30 * 86400: self = .month
        case let .some(n): self = .seconds(n)
        case .none: self = .none
        }
    }

    public var deleteAfterText: LocalizedStringKey {
        switch self {
        case .day: return "1 day"
        case .week: return "1 week"
        case .month: return "1 month"
        case let .seconds(seconds): return "\(seconds) second(s)"
        case .none: return "never"
        }
    }

    public var seconds: Int64? {
        switch self {
        case .day: return 86400
        case .week: return 7 * 86400
        case .month: return 30 * 86400
        case let .seconds(seconds): return seconds
        case .none: return nil
        }
    }

    private var comparisonValue: Int64 {
        self.seconds ?? Int64.max
    }

    public static func < (lhs: Self, rhs: Self) -> Bool {
        return lhs.comparisonValue < rhs.comparisonValue
    }
}

public struct ChatItemInfo: Decodable {
    public var itemVersions: [ChatItemVersion]
    public var memberDeliveryStatuses: [MemberDeliveryStatus]?
}

public struct ChatItemVersion: Decodable {
    public var chatItemVersionId: Int64
    public var msgContent: MsgContent
    public var formattedText: [FormattedText]?
    public var itemVersionTs: Date
    public var createdAt: Date
}

public struct MemberDeliveryStatus: Decodable {
    public var groupMemberId: Int64
    public var memberDeliveryStatus: CIStatus
}
