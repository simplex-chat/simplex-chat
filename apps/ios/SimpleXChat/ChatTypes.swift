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
    public var profile: Profile
    var activeUser: Bool

    public var displayName: String { get { profile.displayName } }
    public var fullName: String { get { profile.fullName } }
    public var image: String? { get { profile.image } }

    public static let sampleData = User(
        userId: 1,
        userContactId: 1,
        localDisplayName: "alice",
        profile: Profile.sampleData,
        activeUser: true
    )
}

public typealias ContactName = String

public typealias GroupName = String

public struct Profile: Codable, NamedChat {
    public init(displayName: String, fullName: String, image: String? = nil) {
        self.displayName = displayName
        self.fullName = fullName
        self.image = image
    }

    public var displayName: String
    public var fullName: String
    public var image: String?

    var displayNameWithOptionalFullName: String {
        (fullName == "" || displayName == fullName) ? displayName : "\(displayName) (\(fullName))"
    }

    static let sampleData = Profile(
        displayName: "alice",
        fullName: "Alice"
    )
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
}

extension NamedChat {
    public var chatViewName: String {
        get { displayName + (fullName == "" || fullName == displayName ? "" : " / \(fullName)") }
    }
}

public typealias ChatId = String

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

    public var contact: Contact? {
        get {
            switch self {
            case let .direct(contact): return contact
            default: return nil
            }
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
    }

    public static var sampleData: ChatInfo.SampleData = SampleData(
        direct: ChatInfo.direct(contact: Contact.sampleData),
        group: ChatInfo.group(groupInfo: GroupInfo.sampleData),
        contactRequest: ChatInfo.contactRequest(contactRequest: UserContactRequest.sampleData)
    )
}

public struct ChatData: Decodable, Identifiable {
    public var chatInfo: ChatInfo
    public var chatItems: [ChatItem]
    public var chatStats: ChatStats

    public var id: ChatId { get { chatInfo.id } }
}

public struct ChatStats: Decodable {
    public init(unreadCount: Int = 0, minUnreadItemId: Int64 = 0) {
        self.unreadCount = unreadCount
        self.minUnreadItemId = minUnreadItemId
    }

    public var unreadCount: Int = 0
    public var minUnreadItemId: Int64 = 0
}

public struct Contact: Identifiable, Decodable, NamedChat {
    var contactId: Int64
    var localDisplayName: ContactName
    public var profile: Profile
    public var activeConn: Connection
    public var viaGroup: Int64?
    var createdAt: Date
    var updatedAt: Date

    public var id: ChatId { get { "@\(contactId)" } }
    public var apiId: Int64 { get { contactId } }
    public var ready: Bool { get { activeConn.connStatus == .ready } }
    public var sendMsgEnabled: Bool { get { true } }
    public var displayName: String { get { profile.displayName } }
    public var fullName: String { get { profile.fullName } }
    public var image: String? { get { profile.image } }

    public func isIndirectContact() -> Bool {
        return activeConn.connLevel > 0 || viaGroup != nil
    }

    public static let sampleData = Contact(
        contactId: 1,
        localDisplayName: "alice",
        profile: Profile.sampleData,
        activeConn: Connection.sampleData,
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
    var connLevel: Int

    public var id: ChatId { get { ":\(connId)" } }

    static let sampleData = Connection(
        connId: 1,
        connStatus: .ready,
        connLevel: 0
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
    var pccConnId: Int64
    var pccAgentConnId: String
    var pccConnStatus: ConnStatus
    public var viaContactUri: Bool
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

    public var description: String {
        get {
            if let initiated = pccConnStatus.initiated {
                return initiated && !viaContactUri
                ? NSLocalizedString("you shared one-time link", comment: "chat list item description")
                : viaContactUri
                ? NSLocalizedString("via contact address link", comment: "chat list item description")
                : NSLocalizedString("via one-time link", comment: "chat list item description")
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
}

public struct GroupInfo: Identifiable, Decodable, NamedChat {
    public var groupId: Int64
    var localDisplayName: GroupName
    var groupProfile: GroupProfile
    public var membership: GroupMember
    var createdAt: Date
    var updatedAt: Date

    public var id: ChatId { get { "#\(groupId)" } }
    public var apiId: Int64 { get { groupId } }
    public var ready: Bool { get { true } }
    public var sendMsgEnabled: Bool { get { membership.memberActive } }
    public var displayName: String { get { groupProfile.displayName } }
    public var fullName: String { get { groupProfile.fullName } }
    public var image: String? { get { groupProfile.image } }

    public var canDelete: Bool {
        let s = membership.memberStatus
        return membership.memberRole == .owner || (s == .memRemoved || s == .memLeft || s == .memGroupDeleted || s == .memInvited)
    }

    public static let sampleData = GroupInfo(
        groupId: 1,
        localDisplayName: "team",
        groupProfile: GroupProfile.sampleData,
        membership: GroupMember.sampleData,
        createdAt: .now,
        updatedAt: .now
    )
}

public struct GroupProfile: Codable, NamedChat {
    public init(displayName: String, fullName: String, image: String? = nil) {
        self.displayName = displayName
        self.fullName = fullName
        self.image = image
    }

    public var displayName: String
    public var fullName: String
    public var image: String?

    public static let sampleData = GroupProfile(
        displayName: "team",
        fullName: "My Team"
    )
}

public struct GroupMember: Identifiable, Decodable {
    public var groupMemberId: Int64
    var groupId: Int64
    var memberId: String
    public var memberRole: GroupMemberRole
    var memberCategory: GroupMemberCategory
    public var memberStatus: GroupMemberStatus
    var invitedBy: InvitedBy
    public var localDisplayName: ContactName
    public var memberProfile: Profile
    public var memberContactId: Int64?
    var activeConn: Connection?

    public var id: String { "#\(groupId) @\(groupMemberId)" }
    public var displayName: String { get { memberProfile.displayName } }
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
            return p.displayName + (p.fullName == "" || p.fullName == p.displayName ? "" : " / \(p.fullName)")
        }
    }

    public var memberActive: Bool {
        get {
            switch self.memberStatus {
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
        memberProfile: Profile.sampleData,
        memberContactId: 1,
        activeConn: Connection.sampleData
    )
}

public enum GroupMemberRole: String, Decodable {
    case member = "member"
    case admin = "admin"
    case owner = "owner"

    public var text: String {
        switch self {
        case .member:
            return NSLocalizedString("Member", comment: "group member role")
        case .admin:
            return NSLocalizedString("Admin", comment: "group member role")
        case .owner:
            return NSLocalizedString("Owner", comment: "group member role")
        }
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

    public var id: Int64 { get { meta.itemId } }

    public var timestampText: Text { get { meta.timestampText } }

    public var text: String {
        get {
            switch (content.text, file) {
            case let ("", .some(file)): return file.fileName
            default: return content.text
            }
        }
    }

    public func isRcvNew() -> Bool {
        if case .rcvNew = meta.itemStatus { return true }
        return false
    }

    public func isMsgContent() -> Bool {
        switch content {
        case .sndMsgContent: return true
        case .rcvMsgContent: return true
        default: return false
        }
    }

    public func isDeletedContent() -> Bool {
        switch content {
        case .sndDeleted: return true
        case .rcvDeleted: return true
        default: return false
        }
    }

    public func isCall() -> Bool {
        switch content {
        case .sndCall: return true
        case .rcvCall: return true
        default: return false
        }
    }

    public var memberDisplayName: String? {
        get {
            if case let .groupRcv(groupMember) = chatDir {
                return groupMember.memberProfile.displayName
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
    case sndError(agentError: AgentErrorType)
    case rcvNew
    case rcvRead
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
    var itemId: Int64?
    var sharedMsgId: String? = nil
    var sentAt: Date
    public var content: MsgContent
    public var formattedText: [FormattedText]?

    public var text: String { get { content.text } }

    public func getSender(_ currentUser: User?) -> String? {
        switch (chatDir) {
        case .directSnd: return "you"
        case .directRcv: return nil
        case .groupSnd: return currentUser?.displayName
        case let .groupRcv(member): return member.memberProfile.displayName
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
    case file(String)
    // TODO include original JSON, possibly using https://github.com/zoul/generic-json-swift
    case unknown(type: String, text: String)

    var text: String {
        get {
            switch self {
            case let .text(text): return text
            case let .link(text, _): return text
            case let .image(text, _): return text
            case let .file(text): return text
            case let .unknown(_, text): return text
            }
        }
    }

    var cmdString: String {
        get {
            switch self {
            case let .text(text): return "text \(text)"
            default: return "json \(encodeJSON(self))"
            }
        }
    }

    public func isFile() -> Bool {
        switch self {
        case .file: return true
        default: return false
        }
    }

    enum CodingKeys: String, CodingKey {
        case type
        case text
        case preview
        case image
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
    case email
    case phone
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
public struct LinkPreview: Codable {
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
        case .negotiated: return NSLocalizedString("connecting call…", comment: "call status")
        case .progress: return NSLocalizedString("call in progress", comment: "call status")
        case .ended: return String.localizedStringWithFormat(NSLocalizedString("ended call %@", comment: "call status"), CICallStatus.durationText(sec))
        case .error: return NSLocalizedString("call error", comment: "call status")
        }
    }

    public static func durationText(_ sec: Int) -> String {
        String(format: "%02d:%02d", sec / 60, sec % 60)
    }
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
    case memberDeleted(groupMemberId: Int64, profile: Profile)
    case userDeleted
    case groupDeleted

    var text: String {
        switch self {
        case let .memberAdded(_, profile):
            return String.localizedStringWithFormat(NSLocalizedString("invited %@", comment: "rcv group event chat item"), profile.displayNameWithOptionalFullName)
        case .memberConnected: return NSLocalizedString("member connected", comment: "rcv group event chat item")
        case .memberLeft: return NSLocalizedString("left", comment: "rcv group event chat item")
        case let .memberDeleted(_, profile):
            return String.localizedStringWithFormat(NSLocalizedString("removed %@", comment: "rcv group event chat item"), profile.displayNameWithOptionalFullName)
        case .userDeleted: return NSLocalizedString("removed you", comment: "rcv group event chat item")
        case .groupDeleted: return NSLocalizedString("deleted group", comment: "rcv group event chat item")
        }
    }
}

public enum SndGroupEvent: Decodable {
    case memberDeleted(groupMemberId: Int64, profile: Profile)
    case userLeft

    var text: String {
        switch self {
        case let .memberDeleted(_, profile):
            return String.localizedStringWithFormat(NSLocalizedString("you removed %@", comment: "snd group event chat item"), profile.displayNameWithOptionalFullName)
        case .userLeft: return NSLocalizedString("you left", comment: "snd group event chat item")
        }
    }
}
