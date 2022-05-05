//
//  ChatModel.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

struct User: Decodable, NamedChat {
    var userId: Int64
    var userContactId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var activeUser: Bool

    var displayName: String { get { profile.displayName } }
    var fullName: String { get { profile.fullName } }
    var image: String? { get { profile.image } }

    static let sampleData = User(
        userId: 1,
        userContactId: 1,
        localDisplayName: "alice",
        profile: Profile.sampleData,
        activeUser: true
    )
}

typealias ContactName = String

typealias GroupName = String

struct Profile: Codable, NamedChat {
    var displayName: String
    var fullName: String
    var image: String?

    static let sampleData = Profile(
        displayName: "alice",
        fullName: "Alice"
    )
}

enum ChatType: String {
    case direct = "@"
    case group = "#"
    case contactRequest = "<@"
    case contactConnection = ":"
}

protocol NamedChat {
    var displayName: String { get }
    var fullName: String { get }
    var image: String? { get }
}

extension NamedChat {
    var chatViewName: String {
        get { displayName + (fullName == "" || fullName == displayName ? "" : " / \(fullName)") }
    }
}

typealias ChatId = String

enum ChatInfo: Identifiable, Decodable, NamedChat {
    case direct(contact: Contact)
    case group(groupInfo: GroupInfo)
    case contactRequest(contactRequest: UserContactRequest)
    case contactConnection(contactConnection: PendingContactConnection)

    var localDisplayName: String {
        get {
            switch self {
            case let .direct(contact): return contact.localDisplayName
            case let .group(groupInfo): return groupInfo.localDisplayName
            case let .contactRequest(contactRequest): return contactRequest.localDisplayName
            case let .contactConnection(contactConnection): return contactConnection.localDisplayName
            }
        }
    }

    var displayName: String {
        get {
            switch self {
            case let .direct(contact): return contact.displayName
            case let .group(groupInfo): return groupInfo.displayName
            case let .contactRequest(contactRequest): return contactRequest.displayName
            case let .contactConnection(contactConnection): return contactConnection.displayName
            }
        }
    }

    var fullName: String {
        get {
            switch self {
            case let .direct(contact): return contact.fullName
            case let .group(groupInfo): return groupInfo.fullName
            case let .contactRequest(contactRequest): return contactRequest.fullName
            case let .contactConnection(contactConnection): return contactConnection.fullName
            }
        }
    }

    var image: String? {
        get {
            switch self {
            case let .direct(contact): return contact.image
            case let .group(groupInfo): return groupInfo.image
            case let .contactRequest(contactRequest): return contactRequest.image
            case let .contactConnection(contactConnection): return contactConnection.image
            }
        }
    }

    var id: ChatId {
        get {
            switch self {
            case let .direct(contact): return contact.id
            case let .group(groupInfo): return groupInfo.id
            case let .contactRequest(contactRequest): return contactRequest.id
            case let .contactConnection(contactConnection): return contactConnection.id
            }
        }
    }

    var chatType: ChatType {
        get {
            switch self {
            case .direct: return .direct
            case .group: return .group
            case .contactRequest: return .contactRequest
            case .contactConnection: return .contactConnection
            }
        }
    }

    var apiId: Int64 {
        get {
            switch self {
            case let .direct(contact): return contact.apiId
            case let .group(groupInfo): return groupInfo.apiId
            case let .contactRequest(contactRequest): return contactRequest.apiId
            case let .contactConnection(contactConnection): return contactConnection.apiId
            }
        }
    }

    var ready: Bool {
        get {
            switch self {
            case let .direct(contact): return contact.ready
            case let .group(groupInfo): return groupInfo.ready
            case let .contactRequest(contactRequest): return contactRequest.ready
            case let .contactConnection(contactConnection): return contactConnection.ready
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

    struct SampleData {
        var direct: ChatInfo
        var group: ChatInfo
        var contactRequest: ChatInfo
    }

    static var sampleData: ChatInfo.SampleData = SampleData(
        direct: ChatInfo.direct(contact: Contact.sampleData),
        group: ChatInfo.group(groupInfo: GroupInfo.sampleData),
        contactRequest: ChatInfo.contactRequest(contactRequest: UserContactRequest.sampleData)
    )
}

struct ChatData: Decodable, Identifiable {
    var chatInfo: ChatInfo
    var chatItems: [ChatItem]
    var chatStats: ChatStats

    var id: ChatId { get { chatInfo.id } }
}

struct ChatStats: Decodable {
    var unreadCount: Int = 0
    var minUnreadItemId: Int64 = 0
}

struct Contact: Identifiable, Decodable, NamedChat {
    var contactId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var activeConn: Connection
    var viaGroup: Int64?
    var createdAt: Date

    var id: ChatId { get { "@\(contactId)" } }
    var apiId: Int64 { get { contactId } }
    var ready: Bool { get { activeConn.connStatus == .ready } }
    var displayName: String { get { profile.displayName } }
    var fullName: String { get { profile.fullName } }
    var image: String? { get { profile.image } }

    static let sampleData = Contact(
        contactId: 1,
        localDisplayName: "alice",
        profile: Profile.sampleData,
        activeConn: Connection.sampleData,
        createdAt: .now
    )
}

struct ContactRef: Decodable {
    var contactId: Int64
    var localDisplayName: ContactName

    var id: ChatId { get { "@\(contactId)" } }
}

struct ContactSubStatus: Decodable {
    var contact: Contact
    var contactError: ChatError?
}

struct Connection: Decodable {
    var connId: Int64
    var connStatus: ConnStatus

    var id: ChatId { get { ":\(connId)" } }

    static let sampleData = Connection(
        connId: 1,
        connStatus: .ready
    )
}

struct UserContactRequest: Decodable, NamedChat {
    var contactRequestId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var createdAt: Date
    var updatedAt: Date

    var id: ChatId { get { "<@\(contactRequestId)" } }
    var apiId: Int64 { get { contactRequestId } }
    var ready: Bool { get { true } }
    var displayName: String { get { profile.displayName } }
    var fullName: String { get { profile.fullName } }
    var image: String? { get { profile.image } }

    static let sampleData = UserContactRequest(
        contactRequestId: 1,
        localDisplayName: "alice",
        profile: Profile.sampleData,
        createdAt: .now,
        updatedAt: .now
    )
}

struct PendingContactConnection: Decodable, NamedChat {
    var pccConnId: Int64
    var pccAgentConnId: String
    var pccConnStatus: ConnStatus
    var viaContactUri: Bool
    var createdAt: Date
    var updatedAt: Date

    var id: ChatId { get { ":\(pccConnId)" } }
    var apiId: Int64 { get { pccConnId } }
    var ready: Bool { get { false } }
    var localDisplayName: String {
        get { String.localizedStringWithFormat(NSLocalizedString("connection:%@", comment: "connection information"), pccConnId) }
    }
    var displayName: String {
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
    var fullName: String { get { "" } }
    var image: String? { get { nil } }
    var initiated: Bool { get { (pccConnStatus.initiated ?? false) && !viaContactUri } }

    var description: String {
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

    static func getSampleData(_ status: ConnStatus = .new, viaContactUri: Bool = false) -> PendingContactConnection {
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

enum ConnStatus: String, Decodable {
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

struct GroupInfo: Identifiable, Decodable, NamedChat {
    var groupId: Int64
    var localDisplayName: GroupName
    var groupProfile: GroupProfile
    var createdAt: Date

    var id: ChatId { get { "#\(groupId)" } }
    var apiId: Int64 { get { groupId } }
    var ready: Bool { get { true } }
    var displayName: String { get { groupProfile.displayName } }
    var fullName: String { get { groupProfile.fullName } }
    var image: String? { get { groupProfile.image } }

    static let sampleData = GroupInfo(
        groupId: 1,
        localDisplayName: "team",
        groupProfile: GroupProfile.sampleData,
        createdAt: .now
    )
}

struct GroupProfile: Codable, NamedChat {
    var displayName: String
    var fullName: String
    var image: String?

    static let sampleData = GroupProfile(
        displayName: "team",
        fullName: "My Team"
    )
}

struct GroupMember: Decodable {
    var groupMemberId: Int64
    var memberId: String
//    var memberRole: GroupMemberRole
//    var memberCategory: GroupMemberCategory
//    var memberStatus: GroupMemberStatus
//    var invitedBy: InvitedBy
    var localDisplayName: ContactName
    var memberProfile: Profile
    var memberContactId: Int64?
//    var activeConn: Connection?

    var directChatId: ChatId? {
        get {
            if let chatId = memberContactId {
                return "@\(chatId)"
            } else {
                return nil
            }
        }
    }

    static let sampleData = GroupMember(
        groupMemberId: 1,
        memberId: "abcd",
        localDisplayName: "alice",
        memberProfile: Profile.sampleData,
        memberContactId: 1
    )
}

struct MemberSubError: Decodable {
    var member: GroupMember
    var memberError: ChatError
}

struct AChatItem: Decodable {
    var chatInfo: ChatInfo
    var chatItem: ChatItem
}

struct ChatItem: Identifiable, Decodable {
    var chatDir: CIDirection
    var meta: CIMeta
    var content: CIContent
    var formattedText: [FormattedText]?
    var quotedItem: CIQuote?
    var file: CIFile?

    var id: Int64 { get { meta.itemId } }

    var timestampText: Text { get { meta.timestampText } }

    var text: String {
        get {
            switch (content.text, file) {
            case let ("", .some(file)): return file.fileName
            default: return content.text
            }
        }
    }

    func isRcvNew() -> Bool {
        if case .rcvNew = meta.itemStatus { return true }
        return false
    }

    func isMsgContent() -> Bool {
        switch content {
        case .sndMsgContent: return true
        case .rcvMsgContent: return true
        default: return false
        }
    }

    func isDeletedContent() -> Bool {
        switch content {
        case .sndDeleted: return true
        case .rcvDeleted: return true
        default: return false
        }
    }

    var memberDisplayName: String? {
        get {
            if case let .groupRcv(groupMember) = chatDir {
                return groupMember.memberProfile.displayName
            } else {
                return nil
            }
        }
    }

    static func getSample (_ id: Int64, _ dir: CIDirection, _ ts: Date, _ text: String, _ status: CIStatus = .sndNew, quotedItem: CIQuote? = nil, file: CIFile? = nil, _ itemDeleted: Bool = false, _ itemEdited: Bool = false, _ editable: Bool = true) -> ChatItem {
        ChatItem(
            chatDir: dir,
            meta: CIMeta.getSample(id, ts, text, status, itemDeleted, itemEdited, editable),
            content: .sndMsgContent(msgContent: .text(text)),
            quotedItem: quotedItem,
            file: file
       )
    }

    static func getFileMsgContentSample (id: Int64 = 1, text: String = "", fileName: String = "test.txt", fileSize: Int64 = 100, fileStatus: CIFileStatus = .rcvComplete) -> ChatItem {
        ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(id, .now, text, .rcvRead, false, false, false),
            content: .rcvMsgContent(msgContent: .file(text)),
            quotedItem: nil,
            file: CIFile.getSample(fileName: fileName, fileSize: fileSize, fileStatus: fileStatus)
       )
    }

    static func getDeletedContentSample (_ id: Int64 = 1, dir: CIDirection = .directRcv, _ ts: Date = .now, _ text: String = "this item is deleted", _ status: CIStatus = .rcvRead) -> ChatItem {
        ChatItem(
            chatDir: dir,
            meta: CIMeta.getSample(id, ts, text, status, false, false, false),
            content: .rcvDeleted(deleteMode: .cidmBroadcast),
            quotedItem: nil,
            file: nil
       )
    }
}

enum CIDirection: Decodable {
    case directSnd
    case directRcv
    case groupSnd
    case groupRcv(groupMember: GroupMember)

    var sent: Bool {
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

struct CIMeta: Decodable {
    var itemId: Int64
    var itemTs: Date
    var itemText: String
    var itemStatus: CIStatus
    var createdAt: Date
    var itemDeleted: Bool
    var itemEdited: Bool
    var editable: Bool

    var timestampText: Text { get { formatTimestampText(itemTs) } }

    static func getSample(_ id: Int64, _ ts: Date, _ text: String, _ status: CIStatus = .sndNew, _ itemDeleted: Bool = false, _ itemEdited: Bool = false, _ editable: Bool = true) -> CIMeta {
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

func formatTimestampText(_ date: Date) -> Text {
    let now = Calendar.current.dateComponents([.day, .hour], from: .now)
    let dc = Calendar.current.dateComponents([.day, .hour], from: date)
    let recent = now.day == dc.day || ((now.day ?? 0) - (dc.day ?? 0) == 1 && (dc.hour ?? 0) >= 18 && (now.hour ?? 0) < 12)
    return Text(date, format: recent ? msgTimeFormat : msgDateFormat)
}

enum CIStatus: Decodable {
    case sndNew
    case sndSent
    case sndErrorAuth
    case sndError(agentError: AgentErrorType)
    case rcvNew
    case rcvRead
}

enum CIDeleteMode: String, Decodable {
    case cidmBroadcast = "broadcast"
    case cidmInternal = "internal"
}

protocol ItemContent {
    var text: String { get }
}

enum CIContent: Decodable, ItemContent {
    case sndMsgContent(msgContent: MsgContent)
    case rcvMsgContent(msgContent: MsgContent)
    case sndDeleted(deleteMode: CIDeleteMode)
    case rcvDeleted(deleteMode: CIDeleteMode)

    var text: String {
        get {
            switch self {
            case let .sndMsgContent(mc): return mc.text
            case let .rcvMsgContent(mc): return mc.text
            case .sndDeleted: return NSLocalizedString("deleted", comment: "deleted chat item")
            case .rcvDeleted: return NSLocalizedString("deleted", comment: "deleted chat item")
            }
        }
    }

    var msgContent: MsgContent? {
        get {
            switch self {
            case let .sndMsgContent(mc): return mc
            case let .rcvMsgContent(mc): return mc
            default: return nil
            }
        }
    }
}

struct CIQuote: Decodable, ItemContent {
    var chatDir: CIDirection?
    var itemId: Int64?
    var sharedMsgId: String? = nil
    var sentAt: Date
    var content: MsgContent
    var formattedText: [FormattedText]?

    var text: String { get { content.text } }

    func getSender(_ currentUser: User?) -> String? {
        switch (chatDir) {
        case .directSnd: return "you"
        case .directRcv: return nil
        case .groupSnd: return currentUser?.displayName
        case let .groupRcv(member): return member.memberProfile.displayName
        case nil: return nil
        }
    }

    static func getSample(_ itemId: Int64?, _ sentAt: Date, _ text: String, chatDir: CIDirection?, image: String? = nil) -> CIQuote {
        let mc: MsgContent
        if let image = image {
            mc = .image(text: text, image: image)
        } else {
            mc = .text(text)
        }
        return CIQuote(chatDir: chatDir, itemId: itemId, sentAt: sentAt, content: mc)
    }
}

struct CIFile: Decodable {
    var fileId: Int64
    var fileName: String
    var fileSize: Int64
    var filePath: String?
    var fileStatus: CIFileStatus

    static func getSample(fileId: Int64 = 1, fileName: String = "test.txt", fileSize: Int64 = 100, filePath: String? = "test.txt", fileStatus: CIFileStatus = .rcvComplete) -> CIFile {
        CIFile(fileId: fileId, fileName: fileName, fileSize: fileSize, filePath: filePath, fileStatus: fileStatus)
    }

    var stored: Bool {
        get {
            switch self.fileStatus {
            case .sndStored: return true
            case .sndCancelled: return true
            case .rcvComplete: return true
            default: return false
            }
        }
    }
}

enum CIFileStatus: String, Decodable {
    case sndStored = "snd_stored"
    case sndCancelled = "snd_cancelled"
    case rcvInvitation = "rcv_invitation"
    case rcvAccepted = "rcv_accepted"
    case rcvTransfer = "rcv_transfer"
    case rcvComplete = "rcv_complete"
    case rcvCancelled = "rcv_cancelled"
}

enum MsgContent {
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

    enum CodingKeys: String, CodingKey {
        case type
        case text
        case preview
        case image
    }
}

// TODO define Encodable
extension MsgContent: Decodable {
    init(from decoder: Decoder) throws {
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
    func encode(to encoder: Encoder) throws {
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

struct FormattedText: Decodable {
    var text: String
    var format: Format?
}

enum Format: Decodable, Equatable {
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

enum FormatColor: String, Decodable {
    case red = "red"
    case green = "green"
    case blue = "blue"
    case yellow = "yellow"
    case cyan = "cyan"
    case magenta = "magenta"
    case black = "black"
    case white = "white"

    var uiColor: Color {
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
struct LinkPreview: Codable {
    var uri: URL
    var title: String
    // TODO remove once optional in haskell
    var description: String = ""
    var image: String
}

enum NtfTknStatus: String, Decodable {
    case new = "NEW"
    case registered = "REGISTERED"
    case invalid = "INVALID"
    case confirmed = "CONFIRMED"
    case active = "ACTIVE"
    case expired = "EXPIRED"
}

struct SndFileTransfer: Decodable {

}

struct FileTransferMeta: Decodable {
    
}
