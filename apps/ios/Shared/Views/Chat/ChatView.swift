//
//  ChatView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import Introspect

private let memberImageSize: CGFloat = 34

struct ChatView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat
    @State private var showChatInfoSheet: Bool = false
    @State private var showAddMembersSheet: Bool = false
    @State private var membersToAdd: [Contact] = []
    @State private var composeState = ComposeState()
    @State private var deletingItem: ChatItem? = nil
    @FocusState private var keyboardVisible: Bool
    @State private var showDeleteMessage = false
    @State private var connectionStats: ConnectionStats?
    @State private var tableView: UITableView?
    @State private var loadingItems = false
    @State private var firstPage = false
    @State private var scrolledToUnread = false

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
                            ForEach(chatModel.chatItems.reversed()) { ci in
                                chatItemView(ci, maxWidth)
                                .scaleEffect(x: 1, y: -1, anchor: .center)
                                .onAppear { loadChatItems(cInfo, ci, proxy) }
                            }
                        }
                    }
                    .onAppear {
                        DispatchQueue.main.async {
                            scrollToFirstUnread(proxy)
                            scrolledToUnread = true
                        }
                        markAllRead()
                    }
                    .onChange(of: chatModel.chatItems.last?.id) { _ in
                        scrollToBottom(proxy)
                    }
                    .onChange(of: keyboardVisible) { _ in
                        if keyboardVisible {
                            DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
                                scrollToBottom(proxy, animation: .easeInOut(duration: 1))
                            }
                        }
                    }
                    .onTapGesture { hideKeyboard() }
                }
            }
            .scaleEffect(x: 1, y: -1, anchor: .center)

            Spacer(minLength: 0)

            ComposeView(
                chat: chat,
                composeState: $composeState,
                keyboardVisible: $keyboardVisible
            )
            .disabled(!chat.chatInfo.sendMsgEnabled)
        }
        .padding(.top, 1)
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
                    if case .direct = cInfo {
                        Task {
                            do {
                                let stats = try await apiContactInfo(contactId: chat.chatInfo.apiId)
                                await MainActor.run { connectionStats = stats }
                            } catch let error {
                                logger.error("apiContactInfo error: \(responseError(error))")
                            }
                            await MainActor.run { showChatInfoSheet = true }
                        }
                    } else {
                        showChatInfoSheet = true
                    }
                } label: {
                    ChatInfoToolbar(chat: chat)
                }
                .sheet(isPresented: $showChatInfoSheet) {
                    switch cInfo {
                    case .direct:
                        ChatInfoView(chat: chat, connectionStats: connectionStats)
                    case let .group(groupInfo):
                        GroupChatInfoView(chat: chat, groupInfo: groupInfo)
                    default:
                        EmptyView()
                    }
                }
            }
            ToolbarItem(placement: .navigationBarTrailing) {
                switch cInfo {
                case let .direct(contact):
                    HStack {
                        callButton(contact, .audio, imageName: "phone")
                        callButton(contact, .video, imageName: "video")
                    }
                case let .group(groupInfo):
                    if groupInfo.canAddMembers {
                        addMembersButton()
                            .sheet(isPresented: $showAddMembersSheet) {
                                AddGroupMembersView(chat: chat, groupInfo: groupInfo, membersToAdd: membersToAdd)
                            }
                    }
                default:
                    EmptyView()
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

    private func addMembersButton() -> some View {
        Button {
            if case let .group(gInfo) = chat.chatInfo {
                Task {
                    let ms = await apiListMembers(gInfo.apiId)
                    await MainActor.run {
                        membersToAdd = filterMembersToAdd(ms)
                        showAddMembersSheet = true
                    }
                }
            }
        } label: {
            Image(systemName: "person.crop.circle.badge.plus")
        }
    }

    private func loadChatItems(_ cInfo: ChatInfo, _ ci: ChatItem, _ proxy: ScrollViewProxy) {
        if let firstItem = chatModel.chatItems.first, firstItem.id == ci.id {
            if loadingItems || firstPage || !scrolledToUnread { return }
            loadingItems = true
            Task {
                do {
                    let items = try await apiGetChatItems(
                        type: cInfo.chatType,
                        id: cInfo.apiId,
                        pagination: .before(chatItemId: firstItem.id, count: 50)
                    )
                    await MainActor.run {
                        if items.count == 0 {
                            firstPage = true
                        } else {
                            chatModel.chatItems.insert(contentsOf: items, at: 0)
                        }
                        loadingItems = false
                    }
                } catch let error {
                    logger.error("apiGetChat error: \(responseError(error))")
                    await MainActor.run { loadingItems = false }
                }
            }
        }
    }

    @ViewBuilder private func chatItemView(_ ci: ChatItem, _ maxWidth: CGFloat) -> some View {
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

    private func chatItemWithMenu(_ ci: ChatItem, _ maxWidth: CGFloat, showMember: Bool = false) -> some View {
        let alignment: Alignment = ci.chatDir.sent ? .trailing : .leading
        var menu: [UIAction] = []
        if ci.isMsgContent() {
            menu.append(contentsOf: [
                UIAction(
                    title: NSLocalizedString("Reply", comment: "chat item action"),
                    image: UIImage(systemName: "arrowshape.turn.up.left")
                ) { _ in
                    withAnimation {
                        if composeState.editing() {
                            composeState = ComposeState(contextItem: .quotedItem(chatItem: ci))
                        } else {
                            composeState = composeState.copy(contextItem: .quotedItem(chatItem: ci))
                        }
                    }
                },
                UIAction(
                    title: NSLocalizedString("Share", comment: "chat item action"),
                    image: UIImage(systemName: "square.and.arrow.up")
                ) { _ in
                    var shareItems: [Any] = [ci.content.text]
                    if case .image = ci.content.msgContent, let image = getLoadedImage(ci.file) {
                        shareItems.append(image)
                    }
                    showShareSheet(items: shareItems)
                },
                UIAction(
                    title: NSLocalizedString("Copy", comment: "chat item action"),
                    image: UIImage(systemName: "doc.on.doc")
                ) { _ in
                    if case let .image(text, _) = ci.content.msgContent,
                       text == "",
                       let image = getLoadedImage(ci.file) {
                        UIPasteboard.general.image = image
                    } else {
                        UIPasteboard.general.string = ci.content.text
                    }
                }
            ])
            if case .image = ci.content.msgContent,
               let image = getLoadedImage(ci.file) {
                menu.append(
                    UIAction(
                        title: NSLocalizedString("Save", comment: "chat item action"),
                        image: UIImage(systemName: "square.and.arrow.down")
                    ) { _ in
                        UIImageWriteToSavedPhotosAlbum(image, nil, nil, nil)
                    }
                )
            }
            if ci.meta.editable {
                menu.append(
                    UIAction(
                        title: NSLocalizedString("Edit", comment: "chat item action"),
                        image: UIImage(systemName: "square.and.pencil")
                    ) { _ in
                        withAnimation {
                            composeState = ComposeState(editingItem: ci)
                        }
                    }
                )
            }
            menu.append(
                UIAction(
                    title: NSLocalizedString("Delete", comment: "chat item action"),
                    image: UIImage(systemName: "trash"),
                    attributes: [.destructive]
                ) { _ in
                    showDeleteMessage = true
                    deletingItem = ci
                }
            )
        } else if ci.isDeletedContent() {
            menu.append(
                UIAction(
                    title: NSLocalizedString("Delete", comment: "chat item action"),
                    image: UIImage(systemName: "trash"),
                    attributes: [.destructive]
                ) { _ in
                    showDeleteMessage = true
                    deletingItem = ci
                }
            )
        }

        var _size = CGSize(width: 0, height: 0)
        let size = Binding(get: { _size }, set: { _size = $0 })

        return ChatItemView(chatInfo: chat.chatInfo, chatItem: ci, showMember: showMember, maxWidth: maxWidth)
            .contextMenuWithPreview(actions: menu, size: size)
            .overlay {
                GeometryReader { g in
                    Color.clear.preference(key: SizePreferenceKey.self, value: g.size)
                }
            }
            .onPreferenceChange(SizePreferenceKey.self) { size.wrappedValue = $0 }
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

    struct SizePreferenceKey: PreferenceKey {
        static var defaultValue = CGSize(width: 0, height: 0)
        static func reduce(value: inout CGSize, nextValue: () -> CGSize) {
            value = nextValue()
        }
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
        return ChatView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
            .environmentObject(chatModel)
    }
}
