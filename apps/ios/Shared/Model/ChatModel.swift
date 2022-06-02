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
import WebKit
import SimpleXChat

final class ChatModel: ObservableObject {
    @Published var onboardingStage: OnboardingStage?
    @Published var currentUser: User?
    @Published var showChatInfo: Bool = false // TODO comprehensively close modal views on authentication
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
    @Published var deviceToken: String?
    @Published var tokenStatus = NtfTknStatus.new
    // current WebRTC call
    @Published var callInvitations: Dictionary<ChatId, CallInvitation> = [:]
    @Published var activeCall: Call?
    @Published var callCommand: WCallCommand?
    @Published var showCallView = false
    var callWebView: WKWebView?

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

    func updateContactConnection(_ contactConnection: PendingContactConnection) {
        updateChat(.contactConnection(contactConnection: contactConnection))
    }

    func updateContact(_ contact: Contact) {
        updateChat(.direct(contact: contact))
    }

    private func updateChat(_ cInfo: ChatInfo) {
        if hasChat(cInfo.id) {
            updateChatInfo(cInfo)
        } else {
            addChat(Chat(chatInfo: cInfo, chatItems: []))
        }
    }

    func updateNetworkStatus(_ id: ChatId, _ status: Chat.NetworkStatus) {
        if let i = getChatIndex(id) {
            chats[i].serverInfo.networkStatus = status
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
                        Task { await apiMarkChatItemRead(cInfo, cItem) }
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

    func clearChat(_ cInfo: ChatInfo) {
        // clear preview
        if let chat = getChat(cInfo.id) {
            chat.chatItems = []
            chat.chatStats = ChatStats()
            chat.chatInfo = cInfo
        }
        // clear current chat
        if chatId == cInfo.id {
            chatItems = []
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
        case disconnected
        case error(String)

        var statusString: LocalizedStringKey {
            get {
                switch self {
                case .connected: return "Server connected"
                case let .error(err): return "Connecting server… (error: \(err))"
                default: return "Connecting server…"
                }
            }
        }

        var statusExplanation: LocalizedStringKey {
            get {
                switch self {
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

    init(chatInfo: ChatInfo, chatItems: [ChatItem] = [], chatStats: ChatStats = ChatStats(), serverInfo: ServerInfo = ServerInfo(networkStatus: .unknown)) {
        self.chatInfo = chatInfo
        self.chatItems = chatItems
        self.chatStats = chatStats
        self.serverInfo = serverInfo
    }

    var id: ChatId { get { chatInfo.id } }
}
