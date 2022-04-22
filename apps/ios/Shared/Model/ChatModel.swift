//
//  ChatModel.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 22/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import Combine
import SwiftUI

final class ChatModel: ObservableObject {
    @Published var currentUser: User?
    // list of chat "previews"
    @Published var chats: [Chat] = []
    // current chat
    @Published var chatId: String?
    @Published var chatItems: [ChatItem] = []
    @Published var chatToTop: String?
    // items in the terminal view
    @Published var terminalItems: [TerminalItem] = []
    @Published var userAddress: String?
    @Published var userSMPServers: [String]?
    @Published var appOpenUrl: URL?

    var messageDelivery: Dictionary<Int64, () -> Void> = [:]

    static let shared = ChatModel()

    func hasChat(_ id: String) -> Bool {
        chats.first(where: { $0.id == id }) != nil
    }

    func getChat(_ id: String) -> Chat? {
        chats.first(where: { $0.id == id })
    }

    private func getChatIndex(_ id: String) -> Int? {
        chats.firstIndex(where: { $0.id == id })
    }

    func addChat(_ chat: Chat) {
        withAnimation {
            chats.insert(chat, at: 0)
        }
    }

    func updateChatInfo(_ cInfo: ChatInfo) {
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatInfo = cInfo
        }
    }

    func updateContact(_ contact: Contact) {
        let cInfo = ChatInfo.direct(contact: contact)
        if hasChat(contact.id) {
            updateChatInfo(cInfo)
        } else {
            addChat(Chat(chatInfo: cInfo, chatItems: []))
        }
    }

    func updateNetworkStatus(_ contact: Contact, _ status: Chat.NetworkStatus) {
        if let ix = getChatIndex(contact.id) {
            chats[ix].serverInfo.networkStatus = status
        }
    }

    func replaceChat(_ id: String, _ chat: Chat) {
        if let i = getChatIndex(id) {
            chats[i] = chat
        } else {
            // invalid state, correcting
            chats.insert(chat, at: 0)
        }
    }

    func addChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        // update previews
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatItems = [cItem]
            if case .rcvNew = cItem.meta.itemStatus {
                chats[i].chatStats.unreadCount = chats[i].chatStats.unreadCount + 1
            }
            if i > 0 {
                if chatId == nil {
                    withAnimation { popChat_(i) }
                } else if chatId == cInfo.id  {
                    chatToTop = cInfo.id
                } else {
                    popChat_(i)
                }
            }
        } else {
            addChat(Chat(chatInfo: cInfo, chatItems: [cItem]))
        }
        // add to current chat
        if chatId == cInfo.id {
            withAnimation { chatItems.append(cItem) }
            if case .rcvNew = cItem.meta.itemStatus {
                DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                    if self.chatId == cInfo.id {
                        Task { await SimpleX.markChatItemRead(cInfo, cItem) }
                    }
                }
            }
        }
    }

    func upsertChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) -> Bool {
        // update previews
        var res: Bool
        if let chat = getChat(cInfo.id) {
            if let pItem = chat.chatItems.last, pItem.id == cItem.id {
                chat.chatItems = [cItem]
            }
            res = false
        } else {
            addChat(Chat(chatInfo: cInfo, chatItems: [cItem]))
            res = true
        }
        // update current chat
        if chatId == cInfo.id {
            if let i = chatItems.firstIndex(where: { $0.id == cItem.id }) {
                withAnimation(.default) {
                    self.chatItems[i] = cItem
                }
                return false
            } else {
                withAnimation { chatItems.append(cItem) }
                return true
            }
        } else {
            return res
        }
    }
    
    func removeChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        // update previews
        if let chat = getChat(cInfo.id) {
            if let pItem = chat.chatItems.last, pItem.id == cItem.id {
                chat.chatItems = [cItem]
            }
        }
        // remove from current chat
        if chatId == cInfo.id {
            if let i = chatItems.firstIndex(where: { $0.id == cItem.id }) {
                _ = withAnimation {
                    self.chatItems.remove(at: i)
                }
            }
        }
    }

    func markChatItemsRead(_ cInfo: ChatInfo) {
        // update preview
        if let chat = getChat(cInfo.id) {
            chat.chatStats = ChatStats()
        }
        // update current chat
        if chatId == cInfo.id {
            var i = 0
            while i < chatItems.count {
                if case .rcvNew = chatItems[i].meta.itemStatus {
                    chatItems[i].meta.itemStatus = .rcvRead
                }
                i = i + 1
            }
        }
    }

    func markChatItemRead(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        // update preview
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatStats.unreadCount = chats[i].chatStats.unreadCount - 1
        }
        // update current chat
        if chatId == cInfo.id, let j = chatItems.firstIndex(where: { $0.id == cItem.id }) {
            chatItems[j].meta.itemStatus = .rcvRead
        }
    }

    func getPrevChatItem(_ ci: ChatItem) -> ChatItem? {
        if let i = chatItems.firstIndex(where: { $0.id == ci.id }), i > 0  {
            return chatItems[i - 1]
        } else {
            return nil
        }
    }
    
    func popChat(_ id: String) {
        if let i = getChatIndex(id) {
            popChat_(i)
        }
    }

    private func popChat_(_ i: Int) {
        let chat = chats.remove(at: i)
        chats.insert(chat, at: 0)
    }

    func removeChat(_ id: String) {
        withAnimation {
            chats.removeAll(where: { $0.id == id })
        }
    }
}

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
    
    var localDisplayName: String {
        get {
            switch self {
            case let .direct(contact): return contact.localDisplayName
            case let .group(groupInfo): return groupInfo.localDisplayName
            case let .contactRequest(contactRequest): return contactRequest.localDisplayName
            }
        }
    }

    var displayName: String {
        get {
            switch self {
            case let .direct(contact): return contact.displayName
            case let .group(groupInfo): return groupInfo.displayName
            case let .contactRequest(contactRequest): return contactRequest.displayName
            }
        }
    }

    var fullName: String {
        get {
            switch self {
            case let .direct(contact): return contact.fullName
            case let .group(groupInfo): return groupInfo.fullName
            case let .contactRequest(contactRequest): return contactRequest.fullName
            }
        }
    }

    var image: String? {
        get {
            switch self {
            case let .direct(contact): return contact.image
            case let .group(groupInfo): return groupInfo.image
            case let .contactRequest(contactRequest): return contactRequest.image
            }
        }
    }

    var id: ChatId {
        get {
            switch self {
            case let .direct(contact): return contact.id
            case let .group(groupInfo): return groupInfo.id
            case let .contactRequest(contactRequest): return contactRequest.id
            }
        }
    }

    var chatType: ChatType {
        get {
            switch self {
            case .direct: return .direct
            case .group: return .group
            case .contactRequest: return .contactRequest
            }
        }
    }
    
    var apiId: Int64 {
        get {
            switch self {
            case let .direct(contact): return contact.apiId
            case let .group(groupInfo): return groupInfo.apiId
            case let .contactRequest(contactRequest): return contactRequest.apiId
            }
        }
    }

    var ready: Bool {
        get {
            switch self {
            case let .direct(contact): return contact.ready
            case let .group(groupInfo): return groupInfo.ready
            case let .contactRequest(contactRequest): return contactRequest.ready
            }
        }
    }

    var createdAt: Date {
        switch self {
        case let .direct(contact): return contact.createdAt
        case let .group(groupInfo): return groupInfo.createdAt
        case let .contactRequest(contactRequest): return contactRequest.createdAt
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

final class Chat: ObservableObject, Identifiable {
    @Published var chatInfo: ChatInfo
    @Published var chatItems: [ChatItem]
    @Published var chatStats: ChatStats
    @Published var serverInfo = ServerInfo(networkStatus: .unknown)

    struct ServerInfo: Decodable {
        var networkStatus: NetworkStatus
    }

    enum NetworkStatus: Decodable, Equatable {
        case unknown
        case connected
        case connecting
        case disconnected
        case error(String)

        var statusString: LocalizedStringKey {
            get {
                switch self {
                case .connecting: return "Connection pending"
                case .connected: return "Server connected"
                case let .error(err): return "Connecting server… (error: \(err))"
                default: return "Connecting server…"
                }
            }
        }

        var statusExplanation: LocalizedStringKey {
            get {
                switch self {
                case .connecting: return "You are being connected to the server for this contact. Awaiting their confirmation before sending messages is enabled."
                case .connected: return "You are connected to the server used to receive messages from this contact."
                case let .error(err): return "Trying to connect to the server used to receive messages from this contact (error: \(err))."
                default: return "Trying to connect to the server used to receive messages from this contact."
                }
            }
        }

        var imageName: String {
            get {
                switch self {
                case .unknown: return "circle.dotted"
                case .connecting: return "circle.dotted"
                case .connected: return "circle.fill"
                case .disconnected: return "ellipsis.circle.fill"
                case .error: return "exclamationmark.circle.fill"
                }
            }
        }
    }

    init(_ cData: ChatData) {
        self.chatInfo = cData.chatInfo
        self.chatItems = cData.chatItems
        self.chatStats = cData.chatStats
    }

    init(chatInfo: ChatInfo, chatItems: [ChatItem] = [], chatStats: ChatStats = ChatStats()) {
        self.chatInfo = chatInfo
        self.chatItems = chatItems
        self.chatStats = chatStats
    }

    var id: ChatId { get { chatInfo.id } }
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
    var ready: Bool { get { activeConn.connStatus == "ready" || activeConn.connStatus == "snd-ready" } }
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

struct ContactSubStatus: Decodable {
    var contact: Contact
    var contactError: ChatError?
}

struct Connection: Decodable {
    var connStatus: String

    static let sampleData = Connection(connStatus: "ready")
}

struct UserContactRequest: Decodable, NamedChat {
    var contactRequestId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var createdAt: Date

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
        createdAt: .now
    )
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

    var timestampText: Text { get { SimpleX.timestampText(itemTs) } }

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

func timestampText(_ date: Date) -> Text {
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

    var sender: String? {
        get {
            switch (chatDir) {
            case .directSnd: return "you"
            case .directRcv: return nil
            case .groupSnd: return ChatModel.shared.currentUser?.displayName
            case let .groupRcv(member): return member.memberProfile.displayName
            case nil: return nil
            }
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

    static func getSample(_ fileId: Int64, _ fileName: String, _ fileSize: Int64, filePath: String?, fileStatus: CIFileStatus = .sndStored) -> CIFile {
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
    case rcvTransfer = "rcv_transfer"
    case rcvComplete = "rcv_complete"
    case rcvCancelled = "rcv_cancelled"
}

enum MsgContent {
    case text(String)
    case link(text: String, preview: LinkPreview)
    case image(text: String, image: String)
    // TODO include original JSON, possibly using https://github.com/zoul/generic-json-swift
    case unknown(type: String, text: String)

    var text: String {
        get {
            switch self {
            case let .text(text): return text
            case let .link(text, _): return text
            case let .image(text, _): return text
            case let .unknown(_, text): return text
            }
        }
    }

    var cmdString: String {
        get {
            switch self {
            case let .text(text): return "text \(text)"
            case let .link(text: text, preview: preview):
                return "json {\"type\":\"link\",\"text\":\(encodeJSON(text)),\"preview\":\(encodeJSON(preview))}"
            case let .image(text: text, image: image):
                return "json {\"type\":\"image\",\"text\":\(encodeJSON(text)),\"image\":\(encodeJSON(image))}"
            default: return ""
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
            default:
                let text = try? container.decode(String.self, forKey: CodingKeys.text)
                self = .unknown(type: type, text: text ?? "unknown message format")
            }
        } catch {
            self = .unknown(type: "unknown", text: "invalid message format")
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
