//
//  ChatView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let memberImageSize: CGFloat = 34

struct ChatView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat
    @State var message: String = ""
    @State var quotedItem: ChatItem? = nil
    @State var editingItem: ChatItem? = nil
    @State var linkPreview: LinkPreview? = nil
    @State var deletingItem: ChatItem? = nil
    @State private var inProgress: Bool = false
    @FocusState private var keyboardVisible: Bool
    @State private var showChatInfo = false
    @State private var showDeleteMessage = false

    var body: some View {
        let cInfo = chat.chatInfo

        return VStack {
            GeometryReader { g in
                let maxWidth =
                    cInfo.chatType == .group
                    ? (g.size.width - 28) * 0.84 - 42
                    : (g.size.width - 32) * 0.84
                ScrollViewReader { proxy in
                    ScrollView {
                        LazyVStack(spacing: 5)  {
                            ForEach(chatModel.chatItems) { ci in
                                if case let .groupRcv(member) = ci.chatDir {
                                    let prevItem = chatModel.getPrevChatItem(ci)
                                    HStack(alignment: .top, spacing: 0) {
                                        let showMember = prevItem == nil || showMemberImage(member, prevItem)
                                        if showMember {
                                            ProfileImage(imageStr: member.memberProfile.image)
                                                .frame(width: memberImageSize, height: memberImageSize)
                                        } else {
                                            Rectangle().fill(.clear)
                                                .frame(width: memberImageSize, height: memberImageSize)
                                        }
                                        chatItemWithMenu(ci, maxWidth, showMember: showMember).padding(.leading, 8)
                                    }
                                    .padding(.trailing)
                                    .padding(.leading, 12)
                                } else {
                                    chatItemWithMenu(ci, maxWidth).padding(.horizontal)
                                }
                            }
                            .onAppear {
                                DispatchQueue.main.async {
                                    scrollToFirstUnread(proxy)
                                }
                                markAllRead()
                            }
                            .onChange(of: chatModel.chatItems.count) { _ in
                                scrollToBottom(proxy)
                            }
                            .onChange(of: keyboardVisible) { _ in
                                if keyboardVisible {
                                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
                                        scrollToBottom(proxy, animation: .easeInOut(duration: 1))
                                    }
                                }
                            }
                        }
                    }
                    .onTapGesture {
                        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
                    }
                }
            }

            Spacer(minLength: 0)

            ComposeView(
                message: $message,
                quotedItem: $quotedItem,
                editingItem: $editingItem,
                linkPreview: $linkPreview,
                sendMessage: sendMessage,
                resetMessage: { message = "" },
                inProgress: inProgress,
                keyboardVisible: $keyboardVisible
            )
        }
        .navigationTitle(cInfo.chatViewName)
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            ToolbarItem(placement: .navigationBarLeading) {
                Button { chatModel.chatId = nil } label: {
                    HStack(spacing: 4) {
                        Image(systemName: "chevron.backward")
                        Text("Chats")
                    }
                }
            }
            ToolbarItem(placement: .principal) {
                Button {
                    showChatInfo = true
                } label: {
                    ChatInfoToolbar(chat: chat)
                }
                .sheet(isPresented: $showChatInfo) {
                    ChatInfoView(chat: chat, showChatInfo: $showChatInfo)
                }
            }
        }
        .navigationBarBackButtonHidden(true)
    }

    private func chatItemWithMenu(_ ci: ChatItem, _ maxWidth: CGFloat, showMember: Bool = false) -> some View {
        let alignment: Alignment = ci.chatDir.sent ? .trailing : .leading
        return ChatItemView(chatItem: ci, showMember: showMember)
            .contextMenu {
                if ci.isMsgContent() {
                    Button {
                        withAnimation {
                            editingItem = nil
                            quotedItem = ci
                        }
                    } label: { Label("Reply", systemImage: "arrowshape.turn.up.left") }
                    Button {
                        showShareSheet(items: [ci.content.text])
                    } label: { Label("Share", systemImage: "square.and.arrow.up") }
                    Button {
                        UIPasteboard.general.string = ci.content.text
                    } label: { Label("Copy", systemImage: "doc.on.doc") }
                    if ci.meta.editable {
                        Button {
                            withAnimation {
                                quotedItem = nil
                                editingItem = ci
                                message = ci.content.text
                            }
                        } label: { Label("Edit", systemImage: "square.and.pencil") }
                    }
                    Button(role: .destructive) {
                        showDeleteMessage = true
                        deletingItem = ci
                    } label: {
                        Label("Delete", systemImage: "trash")
                    }
                }
            }
            .confirmationDialog("Delete message?", isPresented: $showDeleteMessage, titleVisibility: .visible) {
                Button("Delete for me", role: .destructive) {
                    deleteMessage(.cidmInternal)
                }
//                if let di = deletingItem {
//                    if di.meta.editable {
//                        Button("Delete for everyone",role: .destructive) { deleteMessage(.cidmBroadcast)
//                        }
//                    }
//                }
            }
            .frame(maxWidth: maxWidth, maxHeight: .infinity, alignment: alignment)
            .frame(minWidth: 0, maxWidth: .infinity, alignment: alignment)
    }
    
    private func showMemberImage(_ member: GroupMember, _ prevItem: ChatItem?) -> Bool {
        switch (prevItem?.chatDir) {
        case .groupSnd: return true
        case let .groupRcv(prevMember): return prevMember.groupMemberId != member.groupMemberId
        default: return false
        }
    }

    func scrollToBottom(_ proxy: ScrollViewProxy, animation: Animation = .default) {
        withAnimation(animation) { scrollToBottom_(proxy) }
    }

    func scrollToBottom_(_ proxy: ScrollViewProxy) {
        if let id = chatModel.chatItems.last?.id {
            proxy.scrollTo(id, anchor: .bottom)
        }
    }

    // align first unread with the top or the last unread with bottom
    func scrollToFirstUnread(_ proxy: ScrollViewProxy) {
        if let cItem = chatModel.chatItems.first(where: { $0.isRcvNew() }) {
            proxy.scrollTo(cItem.id)
        } else {
            scrollToBottom_(proxy)
        }
    }

    func markAllRead() {
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
            if chatModel.chatId == chat.id {
                Task { await markChatRead(chat) }
            }
        }
    }

    func sendMessage(_ text: String) {
        logger.debug("ChatView sendMessage")
        Task {
            logger.debug("ChatView sendMessage: in Task")
            do {
                if let ei = editingItem {
                    let chatItem = try await apiUpdateChatItem(
                        type: chat.chatInfo.chatType,
                        id: chat.chatInfo.apiId,
                        itemId: ei.id,
                        msg: .text(text)
                    )
                    DispatchQueue.main.async {
                        editingItem = nil
                        linkPreview = nil
                        let _ = chatModel.upsertChatItem(chat.chatInfo, chatItem)
                    }
                } else {
                    let mc: MsgContent
                    if let preview = linkPreview {
                        mc = .link(text: text, preview: preview)
                    } else {
                        mc = .text(text)
                    }
                    let chatItem = try await apiSendMessage(
                        type: chat.chatInfo.chatType,
                        id: chat.chatInfo.apiId,
                        file: nil,
                        quotedItemId: quotedItem?.meta.itemId,
                        msg: mc
                    )
                    DispatchQueue.main.async {
                        quotedItem = nil
                        linkPreview = nil
                        chatModel.addChatItem(chat.chatInfo, chatItem)
                    }
                }
            } catch {
                logger.error("ChatView.sendMessage error: \(error.localizedDescription)")
            }
        }
    }
    
    func deleteMessage(_ mode: CIDeleteMode) {
        logger.debug("ChatView deleteMessage")
        Task {
            logger.debug("ChatView deleteMessage: in Task")
            do {
                if let di = deletingItem {
                    let toItem = try await apiDeleteChatItem(
                        type: chat.chatInfo.chatType,
                        id: chat.chatInfo.apiId,
                        itemId: di.id,
                        mode: mode
                    )
                    DispatchQueue.main.async {
                        deletingItem = nil
                        let _ = chatModel.removeChatItem(chat.chatInfo, toItem)
                    }
                }
            } catch {
                logger.error("ChatView.deleteMessage error: \(error.localizedDescription)")
            }
        }
    }
}

struct ChatView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chatId = "@1"
        chatModel.chatItems = [
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
        return ChatView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
            .environmentObject(chatModel)
    }
}
