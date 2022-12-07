//
//  ChatModel.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

public struct User: Decodable, NamedChat {
    var userId: Int64
    var userContactId: Int64
    var localDisplayName: ContactName
    public var profile: LocalProfile
    public var fullPreferences: FullPreferences
    var activeUser: Bool

    public var displayName: String { get { profile.displayName } }
    public var fullName: String { get { profile.fullName } }
    public var image: String? { get { profile.image } }
    public var localAlias: String { get { "" } }

    public static let sampleData = User(
        userId: 1,
        userContactId: 1,
        localDisplayName: "alice",
        profile: LocalProfile.sampleData,
        fullPreferences: FullPreferences.sampleData,
        activeUser: true
    )
}

public typealias ContactName = String

public typealias GroupName = String

public struct Profile: Codable, NamedChat {
    public init(displayName: String, fullName: String, image: String? = nil, preferences: Preferences? = nil) {
        self.displayName = displayName
        self.fullName = fullName
        self.image = image
        self.preferences = preferences
    }

    public var displayName: String
    public var fullName: String
    public var image: String?
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
    public init(profileId: Int64, displayName: String, fullName: String, image: String? = nil, preferences: Preferences? = nil, localAlias: String) {
        self.profileId = profileId
        self.displayName = displayName
        self.fullName = fullName
        self.image = image
        self.preferences = preferences
        self.localAlias = localAlias
    }

    public var profileId: Int64
    public var displayName: String
    public var fullName: String
    public var image: String?
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
    LocalProfile(profileId: profileId, displayName: profile.displayName, fullName: profile.fullName, image: profile.image, localAlias: localAlias)
}

public func fromLocalProfile (_ profile: LocalProfile) -> Profile {
    Profile(displayName: profile.displayName, fullName: profile.fullName, image: profile.image)
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
    public var fullDelete: Preference
    public var voice: Preference

    public init(fullDelete: Preference, voice: Preference) {
        self.fullDelete = fullDelete
        self.voice = voice
    }

    public static let sampleData = FullPreferences(fullDelete: Preference(allow: .no), voice: Preference(allow: .yes))
}

public struct Preferences: Codable {
    public var fullDelete: Preference?
    public var voice: Preference?

    public init(fullDelete: Preference?, voice: Preference?) {
        self.fullDelete = fullDelete
        self.voice = voice
    }

    public static let sampleData = Preferences(fullDelete: Preference(allow: .no), voice: Preference(allow: .yes))
}

public func fullPreferencesToPreferences(_ fullPreferences: FullPreferences) -> Preferences {
    Preferences(fullDelete: fullPreferences.fullDelete, voice: fullPreferences.voice)
}

public func contactUserPreferencesToPreferences(_ contactUserPreferences: ContactUserPreferences) -> Preferences {
    Preferences(
        fullDelete: contactUserPreferences.fullDelete.userPreference.preference,
        voice: contactUserPreferences.voice.userPreference.preference
    )
}

public struct Preference: Codable, Equatable {
    public var allow: FeatureAllowed

    public init(allow: FeatureAllowed) {
        self.allow = allow
    }
}

public struct ContactUserPreferences: Decodable {
    public var fullDelete: ContactUserPreference
    public var voice: ContactUserPreference

    public init(fullDelete: ContactUserPreference, voice: ContactUserPreference) {
        self.fullDelete = fullDelete
        self.voice = voice
    }

    public static let sampleData = ContactUserPreferences(
        fullDelete: ContactUserPreference(
            enabled: FeatureEnabled(forUser: false, forContact: false),
            userPreference: .user(preference: Preference(allow: .no)),
            contactPreference: Preference(allow: .no)
        ),
        voice: ContactUserPreference(
            enabled: FeatureEnabled(forUser: true, forContact: true),
            userPreference: .user(preference: Preference(allow: .yes)),
            contactPreference: Preference(allow: .yes)
        )
    )
}

public struct ContactUserPreference: Decodable {
    public var enabled: FeatureEnabled
    public var userPreference: ContactUserPref
    public var contactPreference: Preference

    public init(enabled: FeatureEnabled, userPreference: ContactUserPref, contactPreference: Preference) {
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

    public static func enabled(user: Preference, contact: Preference) -> FeatureEnabled {
        switch (user.allow, contact.allow) {
        case (.always, .no): return FeatureEnabled(forUser: false, forContact: true)
        case (.no, .always): return FeatureEnabled(forUser: true, forContact: false)
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

public enum ContactUserPref: Decodable {
    case contact(preference: Preference) // contact override is set
    case user(preference: Preference) // global user default is used

    public var preference: Preference {
        switch self {
        case let .contact(preference): return preference
        case let .user(preference): return preference
        }
    }
}

public protocol Feature {
    var iconFilled: String { get }
}

public enum ChatFeature: String, Decodable, Feature {
    case fullDelete
    case voice

    public var values: [ChatFeature] { [.fullDelete, .voice] }

    public var id: Self { self }

    public var text: String {
        switch self {
        case .fullDelete: return NSLocalizedString("Delete for everyone", comment: "chat feature")
        case .voice: return NSLocalizedString("Voice messages", comment: "chat feature")
        }
    }

    public var icon: String {
        switch self {
        case .fullDelete: return "trash.slash"
        case .voice: return "mic"
        }
    }

    public var iconFilled: String {
        switch self {
        case .fullDelete: return "trash.slash.fill"
        case .voice: return "mic.fill"
        }
    }

    public func allowDescription(_ allowed: FeatureAllowed) -> LocalizedStringKey {
        switch self {
        case .fullDelete:
            switch allowed {
            case .always: return "Allow your contacts to irreversibly delete sent messages."
            case .yes: return "Allow irreversible message deletion only if your contact allows it to you."
            case .no: return "Contacts can mark messages for deletion; you will be able to view them."
            }
        case .voice:
            switch allowed {
            case .always: return "Allow your contacts to send voice messages."
            case .yes: return "Allow voice messages only if your contact allows them."
            case .no: return "Prohibit sending voice messages."
            }
        }
    }

    public func enabledDescription(_ enabled: FeatureEnabled) -> LocalizedStringKey {
        switch self {
        case .fullDelete:
            return enabled.forUser && enabled.forContact
                    ? "Both you and your contact can irreversibly delete sent messages."
                    : enabled.forUser
                    ? "Only you can irreversibly delete messages (your contact can mark them for deletion)."
                    : enabled.forContact
                    ? "Only your contact can irreversibly delete messages (you can mark them for deletion)."
                    : "Irreversible message deletion is prohibited in this chat."
        case .voice:
            return enabled.forUser && enabled.forContact
                    ? "Both you and your contact can send voice messages."
                    : enabled.forUser
                    ? "Only you can send voice messages."
                    : enabled.forContact
                    ? "Only your contact can send voice messages."
                    : "Voice messages are prohibited in this chat."
        }
    }
}

public enum GroupFeature: String, Decodable, Feature {
    case fullDelete
    case voice
    case directMessages

    public var values: [GroupFeature] { [.directMessages, .fullDelete, .voice] }

    public var id: Self { self }

    public var text: String {
        switch self {
        case .directMessages: return NSLocalizedString("Direct messages", comment: "chat feature")
        case .fullDelete: return NSLocalizedString("Delete for everyone", comment: "chat feature")
        case .voice: return NSLocalizedString("Voice messages", comment: "chat feature")
        }
    }

    public var icon: String {
        switch self {
        case .directMessages: return "arrow.left.and.right.circle"
        case .fullDelete: return "trash.slash"
        case .voice: return "mic"
        }
    }

    public var iconFilled: String {
        switch self {
        case .directMessages: return "arrow.left.and.right.circle.fill"
        case .fullDelete: return "trash.slash.fill"
        case .voice: return "mic.fill"
        }
    }

    public func enableDescription(_ enabled: GroupFeatureEnabled, _ canEdit: Bool) -> LocalizedStringKey {
        if canEdit {
            switch self {
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
            case .voice:
                switch enabled {
                case .on: return "Allow to send voice messages."
                case .off: return "Prohibit sending voice messages."
                }
            }
        } else {
            switch self {
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
            case .voice:
                switch enabled {
                case .on: return "Group members can send voice messages."
                case .off: return "Voice messages are prohibited in this group."
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
    public var fullDelete: ContactFeatureAllowed
    public var voice: ContactFeatureAllowed

    public init(fullDelete: ContactFeatureAllowed, voice: ContactFeatureAllowed) {
        self.fullDelete = fullDelete
        self.voice = voice
    }

    public static let sampleData = ContactFeaturesAllowed(
        fullDelete: ContactFeatureAllowed.userDefault(.no),
        voice: ContactFeatureAllowed.userDefault(.yes)
    )
}

public func contactUserPrefsToFeaturesAllowed(_ contactUserPreferences: ContactUserPreferences) -> ContactFeaturesAllowed {
    ContactFeaturesAllowed(
        fullDelete: contactUserPrefToFeatureAllowed(contactUserPreferences.fullDelete),
        voice: contactUserPrefToFeatureAllowed(contactUserPreferences.voice)
    )
}

public func contactUserPrefToFeatureAllowed(_ contactUserPreference: ContactUserPreference) -> ContactFeatureAllowed {
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
        fullDelete: contactFeatureAllowedToPref(contactFeaturesAllowed.fullDelete),
        voice: contactFeatureAllowedToPref(contactFeaturesAllowed.voice)
    )
}

public func contactFeatureAllowedToPref(_ contactFeatureAllowed: ContactFeatureAllowed) -> Preference? {
    switch contactFeatureAllowed {
    case .userDefault: return nil
    case .always: return Preference(allow: .always)
    case .yes: return Preference(allow: .yes)
    case .no: return Preference(allow: .no)
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
    public var directMessages: GroupPreference
    public var fullDelete: GroupPreference
    public var voice: GroupPreference

    public init(directMessages: GroupPreference, fullDelete: GroupPreference, voice: GroupPreference) {
        self.directMessages = directMessages
        self.fullDelete = fullDelete
        self.voice = voice
    }

    public static let sampleData = FullGroupPreferences(directMessages: GroupPreference(enable: .off), fullDelete: GroupPreference(enable: .off), voice: GroupPreference(enable: .on))
}

public struct GroupPreferences: Codable {
    public var directMessages: GroupPreference?
    public var fullDelete: GroupPreference?
    public var voice: GroupPreference?

    public init(directMessages: GroupPreference?, fullDelete: GroupPreference?, voice: GroupPreference?) {
        self.directMessages = directMessages
        self.fullDelete = fullDelete
        self.voice = voice
    }

    public static let sampleData = GroupPreferences(directMessages: GroupPreference(enable: .off), fullDelete: GroupPreference(enable: .off), voice: GroupPreference(enable: .on))
}

public func toGroupPreferences(_ fullPreferences: FullGroupPreferences) -> GroupPreferences {
    GroupPreferences(directMessages: fullPreferences.directMessages, fullDelete: fullPreferences.fullDelete, voice: fullPreferences.voice)
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

    public var localDisplayName: String {
        get {
            switch self {
            case let .direct(contact): return contact.localDisplayName
            case let .group(groupInfo): return groupInfo.localDisplayName
            case let .contactRequest(contactRequest): return contactRequest.localDisplayName
            case let .contactConnection(contactConnection): return contactConnection.localDisplayName
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
            }
        }
    }

    public var contact: Contact? {
        switch self {
        case let .direct(contact): return contact
        default: return nil
        }
    }

    public var voiceMessageAllowed: Bool {
        switch self {
        case let .direct(contact): return contact.mergedPreferences.voice.enabled.forUser
        case let .group(groupInfo): return groupInfo.fullGroupPreferences.voice.on
        default: return false
        }
    }

    public var fullDeletionAllowed: Bool {
        switch self {
        case let .direct(contact): return contact.mergedPreferences.fullDelete.enabled.forUser
        case let .group(groupInfo): return groupInfo.fullGroupPreferences.fullDelete.on
        default: return false
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
        switch self {
        case let .direct(contact): return contact.chatSettings.enableNtfs
        case let .group(groupInfo): return groupInfo.chatSettings.enableNtfs
        default: return false
        }
    }

    var createdAt: Date {
        switch self {
        case let .direct(contact): return contact.createdAt
        case let .group(groupInfo): return groupInfo.createdAt
        case let .contactRequest(contactRequest): return contactRequest.createdAt
        case let .contactConnection(contactConnection): return contactConnection.createdAt
        }
    }

    public var updatedAt: Date {
        switch self {
        case let .direct(contact): return contact.updatedAt
        case let .group(groupInfo): return groupInfo.updatedAt
        case let .contactRequest(contactRequest): return contactRequest.updatedAt
        case let .contactConnection(contactConnection): return contactConnection.updatedAt
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
    public var activeConn: Connection
    public var viaGroup: Int64?
    public var contactUsed: Bool
    public var chatSettings: ChatSettings
    public var userPreferences: Preferences
    public var mergedPreferences: ContactUserPreferences
    var createdAt: Date
    var updatedAt: Date

    public var id: ChatId { get { "@\(contactId)" } }
    public var apiId: Int64 { get { contactId } }
    public var ready: Bool { get { activeConn.connStatus == .ready } }
    public var sendMsgEnabled: Bool { get { true } }
    public var displayName: String { localAlias == "" ? profile.displayName : localAlias }
    public var fullName: String { get { profile.fullName } }
    public var image: String? { get { profile.image } }
    public var localAlias: String { profile.localAlias }

    public var directContact: Bool {
        (activeConn.connLevel == 0 && !activeConn.viaGroupLink) || contactUsed
    }

    public var contactConnIncognito: Bool {
        activeConn.customUserProfileId != nil
    }

    public static let sampleData = Contact(
        contactId: 1,
        localDisplayName: "alice",
        profile: LocalProfile.sampleData,
        activeConn: Connection.sampleData,
        contactUsed: true,
        chatSettings: ChatSettings.defaults,
        userPreferences: Preferences.sampleData,
        mergedPreferences: ContactUserPreferences.sampleData,
        createdAt: .now,
        updatedAt: .now
    )
}

public struct ContactRef: Decodable, Equatable {
    var contactId: Int64
    var localDisplayName: ContactName

    public var id: ChatId { get { "@\(contactId)" } }
}

public struct ContactSubStatus: Decodable {
    public var contact: Contact
    public var contactError: ChatError?
}

public struct Connection: Decodable {
    var connId: Int64
    var connStatus: ConnStatus
    public var connLevel: Int
    public var viaGroupLink: Bool
    public var customUserProfileId: Int64?

    public var id: ChatId { get { ":\(connId)" } }

    static let sampleData = Connection(
        connId: 1,
        connStatus: .ready,
        connLevel: 0,
        viaGroupLink: false
    )
}

public struct UserContact: Decodable {
    public var userContactLinkId: Int64

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

public struct GroupProfile: Codable, NamedChat {
    public init(displayName: String, fullName: String, image: String? = nil, groupPreferences: GroupPreferences? = nil) {
        self.displayName = displayName
        self.fullName = fullName
        self.image = image
        self.groupPreferences = groupPreferences
    }

    public var displayName: String
    public var fullName: String
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
        return GroupMemberRole.allCases.filter { $0 <= userRole }
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
        invitedBy: .user,
        localDisplayName: "alice",
        memberProfile: LocalProfile.sampleData,
        memberContactId: 1,
        memberContactProfileId: 1,
        activeConn: Connection.sampleData
    )
}

public struct GroupMemberRef: Decodable {
    var groupMemberId: Int64
    var profile: Profile
}

public enum GroupMemberRole: String, Identifiable, CaseIterable, Comparable, Decodable {
    case member = "member"
    case admin = "admin"
    case owner = "owner"

    public var id: Self { self }

    public var text: String {
        switch self {
        case .member: return NSLocalizedString("member", comment: "member role")
        case .admin: return NSLocalizedString("admin", comment: "member role")
        case .owner: return NSLocalizedString("owner", comment: "member role")
        }
    }

    private var comparisonValue: Int {
        switch self {
        case .member: return 0
        case .admin: return 1
        case .owner: return 2
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
    var member: GroupMember
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
}

public struct NtfMsgInfo: Decodable {

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

public struct ChatItem: Identifiable, Decodable {
    public init(chatDir: CIDirection, meta: CIMeta, content: CIContent, formattedText: [FormattedText]? = nil, quotedItem: CIQuote? = nil, file: CIFile? = nil) {
        self.chatDir = chatDir
        self.meta = meta
        self.content = content
        self.formattedText = formattedText
        self.quotedItem = quotedItem
        self.file = file
    }

    public var chatDir: CIDirection
    public var meta: CIMeta
    public var content: CIContent
    public var formattedText: [FormattedText]?
    public var quotedItem: CIQuote?
    public var file: CIFile?

    public var viewTimestamp = Date.now

    private enum CodingKeys: String, CodingKey {
        case chatDir, meta, content, formattedText, quotedItem, file
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

    public func isRcvNew() -> Bool {
        if case .rcvNew = meta.itemStatus { return true }
        return false
    }

    public var isDeletedContent: Bool {
        switch content {
        case .sndDeleted: return true
        case .rcvDeleted: return true
        default: return false
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
        case .rcvGroupInvitation: return showNtfDir
        case .sndGroupInvitation: return showNtfDir
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
            }
        case .sndGroupEvent: return showNtfDir
        case .rcvConnEvent: return false
        case .sndConnEvent: return showNtfDir
        case .rcvChatFeature: return false
        case .sndChatFeature: return showNtfDir
        case .rcvGroupFeature: return false
        case .sndGroupFeature: return showNtfDir
        case .rcvChatFeatureRejected: return showNtfDir
        case .rcvGroupFeatureRejected: return showNtfDir
        }
    }

    public var showMutableNotification: Bool {
        switch content {
        case .rcvCall: return false
        case .rcvChatFeature: return false
        default: return showNtfDir
        }
    }

    public var memberDisplayName: String? {
        get {
            if case let .groupRcv(groupMember) = chatDir {
                return groupMember.displayName
            } else {
                return nil
            }
        }
    }

    public static func getSample (_ id: Int64, _ dir: CIDirection, _ ts: Date, _ text: String, _ status: CIStatus = .sndNew, quotedItem: CIQuote? = nil, file: CIFile? = nil, _ itemDeleted: Bool = false, _ itemEdited: Bool = false, _ editable: Bool = true) -> ChatItem {
        ChatItem(
            chatDir: dir,
            meta: CIMeta.getSample(id, ts, text, status, itemDeleted, itemEdited, editable),
            content: .sndMsgContent(msgContent: .text(text)),
            quotedItem: quotedItem,
            file: file
       )
    }

    public static func getVoiceMsgContentSample (id: Int64 = 1, text: String = "", fileName: String = "voice.m4a", fileSize: Int64 = 65536, fileStatus: CIFileStatus = .rcvComplete) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(id, .now, text, .rcvRead, false, false, false),
            content: .rcvMsgContent(msgContent: .voice(text: text, duration: 30)),
            quotedItem: nil,
            file: CIFile.getSample(fileName: fileName, fileSize: fileSize, fileStatus: fileStatus)
       )
    }

    public static func getFileMsgContentSample (id: Int64 = 1, text: String = "", fileName: String = "test.txt", fileSize: Int64 = 100, fileStatus: CIFileStatus = .rcvComplete) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(id, .now, text, .rcvRead, false, false, false),
            content: .rcvMsgContent(msgContent: .file(text)),
            quotedItem: nil,
            file: CIFile.getSample(fileName: fileName, fileSize: fileSize, fileStatus: fileStatus)
       )
    }

    public static func getDeletedContentSample (_ id: Int64 = 1, dir: CIDirection = .directRcv, _ ts: Date = .now, _ text: String = "this item is deleted", _ status: CIStatus = .rcvRead) -> ChatItem {
        ChatItem(
            chatDir: dir,
            meta: CIMeta.getSample(id, ts, text, status, false, false, false),
            content: .rcvDeleted(deleteMode: .cidmBroadcast),
            quotedItem: nil,
            file: nil
       )
    }

    public static func getIntegrityErrorSample (_ status: CIStatus = .rcvRead, fromMsgId: Int64 = 1, toMsgId: Int64 = 2) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "1 skipped message", status, false, false, false),
            content: .rcvIntegrityError(msgError: .msgSkipped(fromMsgId: fromMsgId, toMsgId: toMsgId)),
            quotedItem: nil,
            file: nil
       )
    }

    public static func getGroupInvitationSample (_ status: CIGroupInvitationStatus = .pending) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "received invitation to join group team as admin", .rcvRead, false, false, false),
            content: .rcvGroupInvitation(groupInvitation: CIGroupInvitation.getSample(status: status), memberRole: .admin),
            quotedItem: nil,
            file: nil
       )
    }

    public static func getGroupEventSample () -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "group event text", .rcvRead, false, false, false),
            content: .rcvGroupEvent(rcvGroupEvent: .memberAdded(groupMemberId: 1, profile: Profile.sampleData)),
            quotedItem: nil,
            file: nil
       )
    }

    public static func getChatFeatureSample(_ feature: ChatFeature, _ enabled: FeatureEnabled) -> ChatItem {
        let content = CIContent.rcvChatFeature(feature: feature, enabled: enabled)
        return ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, content.text, .rcvRead, false, false, false),
            content: content,
            quotedItem: nil,
            file: nil
       )
    }
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
    var itemId: Int64
    var itemTs: Date
    var itemText: String
    public var itemStatus: CIStatus
    var createdAt: Date
    var updatedAt: Date
    public var itemDeleted: Bool
    public var itemEdited: Bool
    public var editable: Bool

    var timestampText: Text { get { formatTimestampText(itemTs) } }

    public static func getSample(_ id: Int64, _ ts: Date, _ text: String, _ status: CIStatus = .sndNew, _ itemDeleted: Bool = false, _ itemEdited: Bool = false, _ editable: Bool = true) -> CIMeta {
        CIMeta(
            itemId: id,
            itemTs: ts,
            itemText: text,
            itemStatus: status,
            createdAt: ts,
            updatedAt: ts,
            itemDeleted: itemDeleted,
            itemEdited: itemEdited,
            editable: editable
        )
    }
}

let msgTimeFormat = Date.FormatStyle.dateTime.hour().minute()
let msgDateFormat = Date.FormatStyle.dateTime.day(.twoDigits).month(.twoDigits)

public func formatTimestampText(_ date: Date) -> Text {
    let now = Calendar.current.dateComponents([.day, .hour], from: .now)
    let dc = Calendar.current.dateComponents([.day, .hour], from: date)
    let recent = now.day == dc.day || ((now.day ?? 0) - (dc.day ?? 0) == 1 && (dc.hour ?? 0) >= 18 && (now.hour ?? 0) < 12)
    return Text(date, format: recent ? msgTimeFormat : msgDateFormat)
}

public enum CIStatus: Decodable {
    case sndNew
    case sndSent
    case sndErrorAuth
    case sndError(agentError: String)
    case rcvNew
    case rcvRead

    var id: String {
        switch self {
        case .sndNew: return  "sndNew"
        case .sndSent: return  "sndSent"
        case .sndErrorAuth: return  "sndErrorAuth"
        case .sndError: return  "sndError"
        case .rcvNew: return  "rcvNew"
        case .rcvRead: return "rcvRead"
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
    case sndDeleted(deleteMode: CIDeleteMode)
    case rcvDeleted(deleteMode: CIDeleteMode)
    case sndCall(status: CICallStatus, duration: Int)
    case rcvCall(status: CICallStatus, duration: Int)
    case rcvIntegrityError(msgError: MsgErrorType)
    case rcvGroupInvitation(groupInvitation: CIGroupInvitation, memberRole: GroupMemberRole)
    case sndGroupInvitation(groupInvitation: CIGroupInvitation, memberRole: GroupMemberRole)
    case rcvGroupEvent(rcvGroupEvent: RcvGroupEvent)
    case sndGroupEvent(sndGroupEvent: SndGroupEvent)
    case rcvConnEvent(rcvConnEvent: RcvConnEvent)
    case sndConnEvent(sndConnEvent: SndConnEvent)
    case rcvChatFeature(feature: ChatFeature, enabled: FeatureEnabled)
    case sndChatFeature(feature: ChatFeature, enabled: FeatureEnabled)
    case rcvGroupFeature(groupFeature: GroupFeature, preference: GroupPreference)
    case sndGroupFeature(groupFeature: GroupFeature, preference: GroupPreference)
    case rcvChatFeatureRejected(feature: ChatFeature)
    case rcvGroupFeatureRejected(groupFeature: GroupFeature)

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
            case let .rcvGroupInvitation(groupInvitation, _): return groupInvitation.text
            case let .sndGroupInvitation(groupInvitation, _): return groupInvitation.text
            case let .rcvGroupEvent(rcvGroupEvent): return rcvGroupEvent.text
            case let .sndGroupEvent(sndGroupEvent): return sndGroupEvent.text
            case let .rcvConnEvent(rcvConnEvent): return rcvConnEvent.text
            case let .sndConnEvent(sndConnEvent): return sndConnEvent.text
            case let .rcvChatFeature(feature, enabled): return "\(feature.text): \(enabled.text)"
            case let .sndChatFeature(feature, enabled): return "\(feature.text): \(enabled.text)"
            case let .rcvGroupFeature(feature, preference): return "\(feature.text): \(preference.enable.text)"
            case let .sndGroupFeature(feature, preference): return "\(feature.text): \(preference.enable.text)"
            case let .rcvChatFeatureRejected(feature): return String.localizedStringWithFormat("%@: received, prohibited", feature.text)
            case let .rcvGroupFeatureRejected(groupFeature): return String.localizedStringWithFormat("%@: received, prohibited", groupFeature.text)
            }
        }
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
}

public struct CIQuote: Decodable, ItemContent {
    var chatDir: CIDirection?
    public var itemId: Int64?
    var sharedMsgId: String? = nil
    var sentAt: Date
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
        case .groupSnd: return membership?.displayName
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

public struct CIFile: Decodable {
    public var fileId: Int64
    public var fileName: String
    public var fileSize: Int64
    public var filePath: String?
    public var fileStatus: CIFileStatus

    public static func getSample(fileId: Int64 = 1, fileName: String = "test.txt", fileSize: Int64 = 100, filePath: String? = "test.txt", fileStatus: CIFileStatus = .rcvComplete) -> CIFile {
        CIFile(fileId: fileId, fileName: fileName, fileSize: fileSize, filePath: filePath, fileStatus: fileStatus)
    }

    var loaded: Bool {
        get {
            switch self.fileStatus {
            case .sndStored: return true
            case .sndTransfer: return true
            case .sndComplete: return true
            case .sndCancelled: return true
            case .rcvInvitation: return false
            case .rcvAccepted: return false
            case .rcvTransfer: return false
            case .rcvCancelled: return false
            case .rcvComplete: return true
            }
        }
    }
}

public enum CIFileStatus: String, Decodable {
    case sndStored = "snd_stored"
    case sndTransfer = "snd_transfer"
    case sndComplete = "snd_complete"
    case sndCancelled = "snd_cancelled"
    case rcvInvitation = "rcv_invitation"
    case rcvAccepted = "rcv_accepted"
    case rcvTransfer = "rcv_transfer"
    case rcvComplete = "rcv_complete"
    case rcvCancelled = "rcv_cancelled"
}

public enum MsgContent {
    case text(String)
    case link(text: String, preview: LinkPreview)
    case image(text: String, image: String)
    case voice(text: String, duration: Int)
    case file(String)
    // TODO include original JSON, possibly using https://github.com/zoul/generic-json-swift
    case unknown(type: String, text: String)

    public var text: String {
        switch self {
        case let .text(text): return text
        case let .link(text, _): return text
        case let .image(text, _): return text
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

    var cmdString: String {
        switch self {
        case let .text(text): return "text \(text)"
        default: return "json \(encodeJSON(self))"
        }
    }

    enum CodingKeys: String, CodingKey {
        case type
        case text
        case preview
        case image
        case duration
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
    case simplexLink(linkType: SimplexLinkType, simplexUri: String, trustedUri: Bool, smpHosts: [String])
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
    String(format: "%02d:%02d", sec / 60, sec % 60)
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
    
    var text: String {
        switch self {
        case let .switchQueue(phase):
            if case .completed = phase {
                return NSLocalizedString("changed address for you", comment: "chat item text")
            }
            return NSLocalizedString("changing address...", comment: "chat item text")
        }
    }
}

public enum SndConnEvent: Decodable {
    case switchQueue(phase: SwitchPhase, member: GroupMemberRef?)
    
    var text: String {
        switch self {
        case let .switchQueue(phase, member):
            if let name = member?.profile.profileViewName {
                return phase == .completed
                    ? String.localizedStringWithFormat(NSLocalizedString("you changed address for %@", comment: "chat item text"), name)
                    : String.localizedStringWithFormat(NSLocalizedString("changing address for %@...", comment: "chat item text"), name)
            }
            return phase == .completed
                ? NSLocalizedString("you changed address", comment: "chat item text")
                : NSLocalizedString("changing address...", comment: "chat item text")
        }
    }
}

public enum SwitchPhase: String, Decodable {
    case started
    case confirmed
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
