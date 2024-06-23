//
//  ChatView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import SwiftyGif

private let memberImageSize: CGFloat = 34

struct ChatView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @Environment(\.dismiss) var dismiss
    @Environment(\.presentationMode) var presentationMode
    @Environment(\.scenePhase) var scenePhase
    @State @ObservedObject var chat: Chat
    @State private var showChatInfoSheet: Bool = false
    @State private var showAddMembersSheet: Bool = false
    @State private var composeState = ComposeState()
    @State private var keyboardVisible = false
    @State private var connectionStats: ConnectionStats?
    @State private var customUserProfile: Profile?
    @State private var connectionCode: String?
    @State private var searchMode = false
    @State private var searchText: String = ""
    @State private var timelineScroll: Timeline.Scroll = .isNearBottom(true)
    @FocusState private var searchFocussed
    // opening GroupLinkView on link button (incognito)
    @State private var showGroupLinkSheet: Bool = false
    @State private var groupLink: String?
    @State private var groupLinkMemberRole: GroupMemberRole = .member

    var body: some View {
        let cInfo = chat.chatInfo
        return VStack(spacing: 0) {
            if searchMode {
                searchToolbar()
                Divider()
            }
            Timeline.TimelineView(
                chat: chat,
                composeState: $composeState
            )
            .onChange(of: searchText) { _ in
                loadChat(chat: chat, search: searchText)
            }
            .onChange(of: chatModel.chatId) { chatId in
                if let chatId, let c = chatModel.getChat(chatId) {
                    chat = c
                    showChatInfoSheet = false
                    loadChat(chat: c)
                }
            }
            connectingText()
            ComposeView(
                chat: chat,
                composeState: $composeState,
                keyboardVisible: $keyboardVisible
            )
            .disabled(!cInfo.sendMsgEnabled)
        }
        .padding(.top, 1)
        .navigationTitle(cInfo.chatViewName)
        .navigationBarTitleDisplayMode(.inline)
        .onAppear {
            initChatView()
        }
        .onChange(of: chatModel.chatId) { cId in
            showChatInfoSheet = false
            if let cId {
                if let c = chatModel.getChat(cId) {
                    chat = c
                }
                initChatView()
            } else {
                dismiss()
            }
        }
        .onDisappear {
            VideoPlayerView.players.removeAll()
            if chatModel.chatId == cInfo.id && !presentationMode.wrappedValue.isPresented {
                chatModel.chatId = nil
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) {
                    if chatModel.chatId == nil {
                        chatModel.chatItemStatuses = [:]
                        chatModel.reversedChatItems = []
                        chatModel.groupMembers = []
                    }
                }
            }
        }
        .toolbar {
            ToolbarItem(placement: .principal) {
                if case let .direct(contact) = cInfo {
                    Button {
                        Task {
                            do {
                                let (stats, profile) = try await apiContactInfo(chat.chatInfo.apiId)
                                let (ct, code) = try await apiGetContactCode(chat.chatInfo.apiId)
                                await MainActor.run {
                                    connectionStats = stats
                                    customUserProfile = profile
                                    connectionCode = code
                                    if contact.activeConn?.connectionCode != ct.activeConn?.connectionCode {
                                        chat.chatInfo = .direct(contact: ct)
                                    }
                                }
                            } catch let error {
                                logger.error("apiContactInfo or apiGetContactCode error: \(responseError(error))")
                            }
                            await MainActor.run { showChatInfoSheet = true }
                        }
                    } label: {
                        ChatInfoToolbar(chat: chat)
                    }
                    .appSheet(isPresented: $showChatInfoSheet, onDismiss: {
                        connectionStats = nil
                        customUserProfile = nil
                        connectionCode = nil
                    }) {
                        ChatInfoView(chat: chat, contact: contact, connectionStats: $connectionStats, customUserProfile: $customUserProfile, localAlias: chat.chatInfo.localAlias, connectionCode: $connectionCode)
                    }
                } else if case let .group(groupInfo) = cInfo {
                    Button {
                        Task {
                            await chatModel.loadGroupMembers(groupInfo: groupInfo)
                            showChatInfoSheet = true
                        }
                    } label: {
                        ChatInfoToolbar(chat: chat)
                    }
                    .appSheet(isPresented: $showChatInfoSheet) {
                        GroupChatInfoView(
                            chat: chat,
                            groupInfo: Binding(
                                get: { groupInfo },
                                set: { gInfo in
                                    chat.chatInfo = .group(groupInfo: gInfo)
                                    chat.created = Date.now
                                }
                            )
                        )
                    }
                } else if case .local = cInfo {
                    ChatInfoToolbar(chat: chat)
                }
            }
            ToolbarItem(placement: .navigationBarTrailing) {
                switch cInfo {
                case let .direct(contact):
                    HStack {
                        let callsPrefEnabled = contact.mergedPreferences.calls.enabled.forUser
                        if callsPrefEnabled {
                            if chatModel.activeCall == nil {
                                callButton(contact, .audio, imageName: "phone")
                                    .disabled(!contact.ready || !contact.active)
                            } else if let call = chatModel.activeCall, call.contact.id == cInfo.id {
                                endCallButton(call)
                            }
                        }
                        Menu {
                            if callsPrefEnabled && chatModel.activeCall == nil {
                                Button {
                                    CallController.shared.startCall(contact, .video)
                                } label: {
                                    Label("Video call", systemImage: "video")
                                }
                                .disabled(!contact.ready || !contact.active)
                            }
                            searchButton()
                            ToggleNtfsButton(chat: chat)
                                .disabled(!contact.ready || !contact.active)
                        } label: {
                            Image(systemName: "ellipsis")
                        }
                    }
                case let .group(groupInfo):
                    HStack {
                        if groupInfo.canAddMembers {
                            if (chat.chatInfo.incognito) {
                                groupLinkButton()
                                    .appSheet(isPresented: $showGroupLinkSheet) {
                                        GroupLinkView(
                                            groupId: groupInfo.groupId,
                                            groupLink: $groupLink,
                                            groupLinkMemberRole: $groupLinkMemberRole,
                                            showTitle: true,
                                            creatingGroup: false
                                        )
                                    }
                            } else {
                                addMembersButton()
                                    .appSheet(isPresented: $showAddMembersSheet) {
                                        AddGroupMembersView(chat: chat, groupInfo: groupInfo)
                                    }
                            }
                        }
                        Menu {
                            searchButton()
                            ToggleNtfsButton(chat: chat)
                        } label: {
                            Image(systemName: "ellipsis")
                        }
                    }
                case .local:
                    searchButton()
                default:
                    EmptyView()
                }
            }
        }
    }

    private func initChatView() {
        let cInfo = chat.chatInfo
        // This check prevents the call to apiContactInfo after the app is suspended, and the database is closed.
        if case .active = scenePhase,
           case let .direct(contact) = cInfo {
            Task {
                do {
                    let (stats, _) = try await apiContactInfo(chat.chatInfo.apiId)
                    await MainActor.run {
                        if let s = stats {
                            chatModel.updateContactConnectionStats(contact, s)
                        }
                    }
                } catch let error {
                    logger.error("apiContactInfo error: \(responseError(error))")
                }
            }
        }
        if chatModel.draftChatId == cInfo.id && !composeState.forwarding,
           let draft = chatModel.draft {
            composeState = draft
        }
        if chat.chatStats.unreadChat {
            Task {
                await markChatUnread(chat, unreadChat: false)
            }
        }
    }

    private func searchToolbar() -> some View {
        HStack(spacing: 12) {
            HStack(spacing: 4) {
                Image(systemName: "magnifyingglass")
                TextField("Search", text: $searchText)
                    .focused($searchFocussed)
                    .foregroundColor(.primary)
                    .frame(maxWidth: .infinity)

                Button {
                    searchText = ""
                } label: {
                    Image(systemName: "xmark.circle.fill").opacity(searchText == "" ? 0 : 1)
                }
            }
            .padding(EdgeInsets(top: 7, leading: 7, bottom: 7, trailing: 7))
            .foregroundColor(.secondary)
            .background(Color(.tertiarySystemFill))
            .cornerRadius(10.0)

            Button ("Cancel") {
                searchText = ""
                searchMode = false
                searchFocussed = false
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) {
                    chatModel.reversedChatItems = []
                    loadChat(chat: chat)
                }
            }
        }
        .padding(.horizontal)
        .padding(.vertical, 8)
    }

    private func voiceWithoutFrame(_ ci: ChatItem) -> Bool {
        ci.content.msgContent?.isVoice == true && ci.content.text.count == 0 && ci.quotedItem == nil && ci.meta.itemForwarded == nil
    }

    @ViewBuilder private func connectingText() -> some View {
        if case let .direct(contact) = chat.chatInfo,
           !contact.ready,
           contact.active,
           !contact.nextSendGrpInv {
            Text("connecting…")
                .font(.caption)
                .foregroundColor(.secondary)
                .padding(.top)
        } else {
            EmptyView()
        }
    }

    private func circleButton<Content: View>(_ content: @escaping () -> Content) -> some View {
        ZStack {
            Circle()
                .foregroundColor(Color(uiColor: .tertiarySystemGroupedBackground))
                .frame(width: 44, height: 44)
            content()
        }
    }

    private func callButton(_ contact: Contact, _ media: CallMediaType, imageName: String) -> some View {
        Button {
            CallController.shared.startCall(contact, media)
        } label: {
            Image(systemName: imageName)
        }
    }

    private func endCallButton(_ call: Call) -> some View {
        Button {
            if let uuid = call.callkitUUID {
                CallController.shared.endCall(callUUID: uuid)
            } else {
                CallController.shared.endCall(call: call) {}
            }
        } label: {
            Image(systemName: "phone.down.fill").tint(.red)
        }
    }

    private func searchButton() -> some View {
        Button {
            searchMode = true
            searchFocussed = true
            searchText = ""
        } label: {
            Label("Search", systemImage: "magnifyingglass")
        }
    }

    private func addMembersButton() -> some View {
        Button {
            if case let .group(gInfo) = chat.chatInfo {
                Task {
                    await chatModel.loadGroupMembers(groupInfo: gInfo)
                    showAddMembersSheet = true
                }
            }
        } label: {
            Image(systemName: "person.crop.circle.badge.plus")
        }
    }

    private func groupLinkButton() -> some View {
        Button {
            if case let .group(gInfo) = chat.chatInfo {
                Task {
                    do {
                        if let link = try apiGetGroupLink(gInfo.groupId) {
                            (groupLink, groupLinkMemberRole) = link
                        }
                    } catch let error {
                        logger.error("ChatView apiGetGroupLink: \(responseError(error))")
                    }
                    showGroupLinkSheet = true
                }
            }
        } label: {
            Image(systemName: "link.badge.plus")
        }
    }
}

struct ToggleNtfsButton: View {
    @ObservedObject var chat: Chat

    var body: some View {
        Button {
            toggleNotifications(chat, enableNtfs: !chat.chatInfo.ntfsEnabled)
        } label: {
            if chat.chatInfo.ntfsEnabled {
                Label("Mute", systemImage: "speaker.slash")
            } else {
                Label("Unmute", systemImage: "speaker.wave.2")
            }
        }
    }
}

func toggleNotifications(_ chat: Chat, enableNtfs: Bool) {
    var chatSettings = chat.chatInfo.chatSettings ?? ChatSettings.defaults
    chatSettings.enableNtfs = enableNtfs ? .all : .none
    updateChatSettings(chat, chatSettings: chatSettings)
}

func toggleChatFavorite(_ chat: Chat, favorite: Bool) {
    var chatSettings = chat.chatInfo.chatSettings ?? ChatSettings.defaults
    chatSettings.favorite = favorite
    updateChatSettings(chat, chatSettings: chatSettings)
}

func updateChatSettings(_ chat: Chat, chatSettings: ChatSettings) {
    Task {
        do {
            try await apiSetChatSettings(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId, chatSettings: chatSettings)
            await MainActor.run {
                switch chat.chatInfo {
                case var .direct(contact):
                    contact.chatSettings = chatSettings
                    ChatModel.shared.updateContact(contact)
                case var .group(groupInfo):
                    groupInfo.chatSettings = chatSettings
                    ChatModel.shared.updateGroup(groupInfo)
                default: ()
                }
            }
        } catch let error {
            logger.error("apiSetChatSettings error \(responseError(error))")
        }
    }
}

struct ChatView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chatId = "@1"
        chatModel.reversedChatItems = [
            ChatItem.getSample(1, .directSnd, .now, "hello"),
            ChatItem.getSample(2, .directRcv, .now, "hi"),
            ChatItem.getSample(3, .directRcv, .now, "hi there"),
            ChatItem.getDeletedContentSample(4),
            ChatItem.getSample(5, .directRcv, .now, "hello again"),
            ChatItem.getSample(6, .directSnd, .now, "hi there!!!"),
            ChatItem.getSample(7, .directSnd, .now, "how are you?"),
            ChatItem.getSample(8, .directSnd, .now, "👍👍👍👍"),
            ChatItem.getSample(9, .directSnd, .now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
        ]
        @State var showChatInfo = false
        return ChatView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
            .environmentObject(chatModel)
    }
}
