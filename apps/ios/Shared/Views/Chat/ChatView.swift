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
    @ObservedObject var chat: Chat
    @State private var showChatInfoSheet: Bool = false
    @State private var showAddMembersSheet: Bool = false
    @State private var composeState = ComposeState()
    @State private var deletingItem: ChatItem? = nil
    @FocusState private var keyboardVisible: Bool
    @State private var showDeleteMessage = false
    @State private var connectionStats: ConnectionStats?
    @State private var tableView: UITableView?
    @State private var loadingItems = false
    @State private var firstPage = false
    @State private var itemsInView: Set<String> = []
    @State private var scrollProxy: ScrollViewProxy?
    @State private var searchMode = false
    @State private var searchText: String = ""
    @FocusState private var searchFocussed

    var body: some View {
        let cInfo = chat.chatInfo
        return VStack(spacing: 0) {
            if searchMode {
                searchToolbar()
                Divider()
            }
            ZStack(alignment: .trailing) {
                chatItemsList()
                if let proxy = scrollProxy {
                    floatingButtons(proxy)
                }
            }

            Spacer(minLength: 0)

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
        .navigationBarBackButtonHidden(true)
        .toolbar {
            ToolbarItem(placement: .navigationBarLeading) {
                Button {
                    chatModel.chatId = nil
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) {
                        if chatModel.chatId == nil {
                            chatModel.reversedChatItems = []
                        }
                    }
                } label: {
                    Image(systemName: "chevron.backward")
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
                    } else if case let .group(groupInfo) = cInfo {
                        Task {
                            let groupMembers = await apiListMembers(groupInfo.groupId)
                            await MainActor.run {
                                ChatModel.shared.groupMembers = groupMembers
                                showChatInfoSheet = true
                            }
                        }
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
                        Menu {
                            Button {
                                CallController.shared.startCall(contact, .video)
                            } label: {
                                Label("Video call", systemImage: "video")
                            }
                            searchButton()
                            toggleNtfsButton(chat: chat, enableNtfs: !contact.chatSettings.enableNtfs)
                        } label: {
                            Image(systemName: "ellipsis")
                        }
                    }
                case let .group(groupInfo):
                    HStack {
                        if groupInfo.canAddMembers {
                            addMembersButton()
                                .sheet(isPresented: $showAddMembersSheet) {
                                    AddGroupMembersView(chat: chat, groupInfo: groupInfo)
                                }
                        }
                        Menu {
                            searchButton()
                            toggleNtfsButton(chat: chat, enableNtfs: !groupInfo.chatSettings.enableNtfs)
                        } label: {
                            Image(systemName: "ellipsis")
                        }
                    }
                default:
                    EmptyView()
                }
            }
        }
    }

    private func searchToolbar() -> some View {
        HStack {
            HStack {
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
            .padding(EdgeInsets(top: 8, leading: 6, bottom: 8, trailing: 6))
            .foregroundColor(.secondary)
            .background(Color(.secondarySystemBackground))
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

    private func chatItemsList() -> some View {
        let cInfo = chat.chatInfo
        return GeometryReader { g in
            ScrollViewReader { proxy in
                ScrollView {
                    let maxWidth =
                        cInfo.chatType == .group
                        ? (g.size.width - 28) * 0.84 - 42
                        : (g.size.width - 32) * 0.84
                    LazyVStack(spacing: 5)  {
                        ForEach(chatModel.reversedChatItems, id: \.viewId) { ci in
                            chatItemView(ci, maxWidth)
                            .scaleEffect(x: 1, y: -1, anchor: .center)
                            .onAppear {
                                itemsInView.insert(ci.viewId)
                                loadChatItems(cInfo, ci, proxy)
                                if ci.isRcvNew() {
                                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.75) {
                                        if chatModel.chatId == cInfo.id && itemsInView.contains(ci.viewId) {
                                            Task {
                                                await apiMarkChatItemRead(cInfo, ci)
                                                NtfManager.shared.decNtfBadgeCount()
                                            }
                                        }
                                    }
                                }
                            }
                            .onDisappear {
                                itemsInView.remove(ci.viewId)
                            }
                        }
                    }
                }
                .onAppear {
                    scrollProxy = proxy
                }
                .onTapGesture { hideKeyboard() }
                .onChange(of: searchText) { _ in
                    loadChat(chat: chat, search: searchText)
                }
            }
        }
        .scaleEffect(x: 1, y: -1, anchor: .center)
    }

    private func floatingButtons(_ proxy: ScrollViewProxy) -> some View {
        let counts = chatModel.unreadChatItemCounts(itemsInView: itemsInView)
        return VStack {
            let unreadAbove = chat.chatStats.unreadCount - counts.unreadBelow
            if unreadAbove > 0 {
                circleButton {
                    unreadCountText(unreadAbove)
                        .font(.callout)
                        .foregroundColor(.accentColor)
                }
                .onTapGesture { scrollUp(proxy) }
                .contextMenu {
                    Button {
                        if let ci = chatModel.topItemInView(itemsInView: itemsInView) {
                            Task {
                                await markChatRead(chat, aboveItem: ci)
                            }
                        }
                    } label: {
                        Label("Mark read", systemImage: "checkmark")
                    }
                }
            }
            Spacer()
            if counts.unreadBelow > 0 {
                circleButton {
                    unreadCountText(counts.unreadBelow)
                        .font(.callout)
                        .foregroundColor(.accentColor)
                }
                .onTapGesture { scrollToBottom(proxy) }
            } else if counts.totalBelow > 16 {
                circleButton {
                    Image(systemName: "chevron.down")
                        .foregroundColor(.accentColor)
                }
                .onTapGesture { scrollToBottom(proxy) }
            }
        }
        .padding()
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
                    let groupMembers = await apiListMembers(gInfo.groupId)
                    await MainActor.run {
                        ChatModel.shared.groupMembers = groupMembers
                        showAddMembersSheet = true
                    }
                }
            }
        } label: {
            Image(systemName: "person.crop.circle.badge.plus")
        }
    }

    private func loadChatItems(_ cInfo: ChatInfo, _ ci: ChatItem, _ proxy: ScrollViewProxy) {
        if let firstItem = chatModel.reversedChatItems.last, firstItem.id == ci.id {
            if loadingItems || firstPage { return }
            loadingItems = true
            Task {
                do {
                    let items = try await apiGetChatItems(
                        type: cInfo.chatType,
                        id: cInfo.apiId,
                        pagination: .before(chatItemId: firstItem.id, count: 50),
                        search: searchText
                    )
                    await MainActor.run {
                        if items.count == 0 {
                            firstPage = true
                        } else {
                            chatModel.reversedChatItems.append(contentsOf: items.reversed())
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

        return ChatItemView(chatInfo: chat.chatInfo, chatItem: ci, showMember: showMember, maxWidth: maxWidth)
            .uiKitContextMenu(actions: menu)
            .confirmationDialog("Delete message?", isPresented: $showDeleteMessage, titleVisibility: .visible) {
                Button("Delete for me", role: .destructive) {
                    deleteMessage(.cidmInternal)
                }
                if let di = deletingItem, di.meta.editable {
                    Button("Delete for everyone",role: .destructive) {
                        deleteMessage(.cidmBroadcast)
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

    private func scrollToBottom(_ proxy: ScrollViewProxy) {
        if let ci = chatModel.reversedChatItems.first {
            withAnimation { proxy.scrollTo(ci.viewId, anchor: .top) }
        }
    }

    private func scrollUp(_ proxy: ScrollViewProxy) {
        if let ci = chatModel.topItemInView(itemsInView: itemsInView) {
            withAnimation { proxy.scrollTo(ci.viewId, anchor: .top) }
        }
    }

    private func deleteMessage(_ mode: CIDeleteMode) {
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

func toggleNtfsButton(chat: Chat, enableNtfs: Bool) -> some View {
    Button {
        toggleNotifications(chat: chat, enableNtfs: enableNtfs)
    } label: {
        if enableNtfs {
            Label("Unmute", systemImage: "speaker")
        } else {
            Label("Mute", systemImage: "speaker.slash")
        }
    }
}

func toggleNotifications(chat: Chat, enableNtfs: Bool) {
    let cInfo = chat.chatInfo
    Task {
        do {
            let chatSettings = ChatSettings(enableNtfs: enableNtfs)
            try await apiSetChatSettings(type: cInfo.chatType, id: cInfo.apiId, chatSettings: chatSettings)
            await MainActor.run {
                switch cInfo {
                case var .direct(contact):
                    contact.chatSettings = chatSettings
                    chat.chatInfo = .direct(contact: contact)
                case var .group(groupInfo):
                    groupInfo.chatSettings = chatSettings
                    chat.chatInfo = .group(groupInfo: groupInfo)
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
