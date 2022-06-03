//
//  ChatView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let memberImageSize: CGFloat = 34

struct ChatView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @AppStorage(DEFAULT_EXPERIMENTAL_CALLS) private var enableCalls = false
    @ObservedObject var chat: Chat
    @Binding var showChatInfo: Bool
    @State private var composeState = ComposeState()
    @State private var deletingItem: ChatItem? = nil
    @FocusState private var keyboardVisible: Bool
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
                chat: chat,
                composeState: $composeState,
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
                        Text("Chats", comment: "back button to return to chats list")
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
            ToolbarItem(placement: .navigationBarTrailing) {
                if enableCalls, case let .direct(contact) = cInfo {
                    HStack {
                        callButton(contact, .audio, imageName: "phone")
                        callButton(contact, .video, imageName: "video")
                    }
                }
            }
        }
        .navigationBarBackButtonHidden(true)
    }

    private func callButton(_ contact: Contact, _ media: CallMediaType, imageName: String) -> some View {
        Button {
            CallController.shared.startCall(contact, media)
        } label: {
            Image(systemName: imageName)
        }
    }

    private func chatItemWithMenu(_ ci: ChatItem, _ maxWidth: CGFloat, showMember: Bool = false) -> some View {
        let alignment: Alignment = ci.chatDir.sent ? .trailing : .leading
        return ChatItemView(chatInfo: chat.chatInfo, chatItem: ci, showMember: showMember, maxWidth: maxWidth)
            .contextMenu {
                if ci.isMsgContent() {
                    Button {
                        withAnimation {
                            if composeState.editing() {
                                composeState = ComposeState(contextItem: .quotedItem(chatItem: ci))
                            } else {
                                composeState = composeState.copy(contextItem: .quotedItem(chatItem: ci))
                            }
                        }
                    } label: { Label("Reply", systemImage: "arrowshape.turn.up.left") }
                    Button {
                        var shareItems: [Any] = [ci.content.text]
                        if case .image = ci.content.msgContent, let image = getLoadedImage(ci.file) {
                            shareItems.append(image)
                        }
                        showShareSheet(items: shareItems)
                    } label: { Label("Share", systemImage: "square.and.arrow.up") }
                    Button {
                        if case let .image(text, _) = ci.content.msgContent,
                           text == "",
                           let image = getLoadedImage(ci.file) {
                            UIPasteboard.general.image = image
                        } else {
                            UIPasteboard.general.string = ci.content.text
                        }
                    } label: { Label("Copy", systemImage: "doc.on.doc") }
                    if case .image = ci.content.msgContent,
                       let image = getLoadedImage(ci.file) {
                        Button {
                            UIImageWriteToSavedPhotosAlbum(image, nil, nil, nil)
                        } label: { Label("Save", systemImage: "square.and.arrow.down") }
                    }
                    if ci.meta.editable {
                        Button {
                            withAnimation {
                                composeState = ComposeState(editingItem: ci)
                            }
                        } label: { Label("Edit", systemImage: "square.and.pencil") }
                    }
                    Button(role: .destructive) {
                        showDeleteMessage = true
                        deletingItem = ci
                    } label: { Label("Delete", systemImage: "trash") }
                } else if ci.isDeletedContent() {
                    Button(role: .destructive) {
                        showDeleteMessage = true
                        deletingItem = ci
                    } label: { Label("Delete", systemImage: "trash") }
                }
            }
            .confirmationDialog("Delete message?", isPresented: $showDeleteMessage, titleVisibility: .visible) {
                Button("Delete for me", role: .destructive) {
                    deleteMessage(.cidmInternal)
                }
                if let di = deletingItem {
                    if di.meta.editable {
                        Button("Delete for everyone",role: .destructive) {
                            deleteMessage(.cidmBroadcast)
                        }
                    }
                }
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
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.75) {
            if chatModel.chatId == chat.id {
                Task { await markChatRead(chat) }
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
        @State var showChatInfo = false
        return ChatView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []), showChatInfo: $showChatInfo)
            .environmentObject(chatModel)
    }
}
