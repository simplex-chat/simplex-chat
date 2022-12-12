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
    @State private var customUserProfile: Profile?
    @State private var connectionCode: String?
    @State private var tableView: UITableView?
    @State private var loadingItems = false
    @State private var firstPage = false
    @State private var itemsInView: Set<String> = []
    @State private var scrollProxy: ScrollViewProxy?
    @State private var searchMode = false
    @State private var searchText: String = ""
    @FocusState private var searchFocussed
    // opening GroupMemberInfoView on member icon
    @State private var selectedMember: GroupMember? = nil

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
        .onAppear {
            if chat.chatStats.unreadChat {
                Task {
                    await markChatUnread(chat, unreadChat: false)
                }
            }
        }
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
                    HStack(spacing: 0) {
                        Image(systemName: "chevron.backward")
                        Text("Chats")
                    }
                }
            }
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
                                    if contact.activeConn.connectionCode != ct.activeConn.connectionCode {
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
                    .sheet(isPresented: $showChatInfoSheet, onDismiss: {
                        connectionStats = nil
                        customUserProfile = nil
                        connectionCode = nil
                    }) {
                        ChatInfoView(chat: chat, contact: contact, connectionStats: $connectionStats, customUserProfile: $customUserProfile, localAlias: chat.chatInfo.localAlias, connectionCode: $connectionCode)
                    }
                } else if case let .group(groupInfo) = cInfo {
                    Button {
                        Task {
                            let groupMembers = await apiListMembers(groupInfo.groupId)
                            await MainActor.run {
                                ChatModel.shared.groupMembers = groupMembers
                                showChatInfoSheet = true
                            }
                        }
                    } label: {
                        ChatInfoToolbar(chat: chat)
                    }
                    .appSheet(isPresented: $showChatInfoSheet) {
                        GroupChatInfoView(chat: chat, groupInfo: groupInfo)
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
                            toggleNtfsButton(chat)
                        } label: {
                            Image(systemName: "ellipsis")
                        }
                    }
                case let .group(groupInfo):
                    HStack {
                        if groupInfo.canAddMembers {
                            if (chat.chatInfo.incognito) {
                                Image(systemName: "person.crop.circle.badge.plus")
                                    .foregroundColor(Color(uiColor: .tertiaryLabel))
                                    .onTapGesture { AlertManager.shared.showAlert(cantInviteIncognitoAlert()) }
                            } else {
                                addMembersButton()
                                    .appSheet(isPresented: $showAddMembersSheet) {
                                        AddGroupMembersView(chat: chat, groupInfo: groupInfo)
                                    }
                            }
                        }
                        Menu {
                            searchButton()
                            toggleNtfsButton(chat)
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
                                    if ci.isRcvNew {
                                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) {
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
                .onChange(of: chatModel.chatId) { _ in
                    if let chatId = chatModel.chatId, let chat = chatModel.getChat(chatId) {
                        showChatInfoSheet = false
                        loadChat(chat: chat)
                        DispatchQueue.main.async {
                            scrollToBottom(proxy)
                        }
                    }
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
        if case let .groupRcv(member) = ci.chatDir,
           case let .group(groupInfo) = chat.chatInfo {
            let prevItem = chatModel.getPrevChatItem(ci)
            HStack(alignment: .top, spacing: 0) {
                let showMember = prevItem == nil || showMemberImage(member, prevItem)
                if showMember {
                    ProfileImage(imageStr: member.memberProfile.image)
                        .frame(width: memberImageSize, height: memberImageSize)
                        .onTapGesture { selectedMember = member }
                        .appSheet(item: $selectedMember) { member in
                            GroupMemberInfoView(groupInfo: groupInfo, member: member, navigation: true)
                        }
                } else {
                    Rectangle().fill(.clear)
                        .frame(width: memberImageSize, height: memberImageSize)
                }
                ChatItemWithMenu(
                    chat: chat,
                    ci: ci,
                    showMember: showMember,
                    maxWidth: maxWidth,
                    scrollProxy: scrollProxy,
                    deleteMessage: deleteMessage,
                    deletingItem: $deletingItem,
                    composeState: $composeState,
                    showDeleteMessage: $showDeleteMessage
                ).padding(.leading, 8)
            }
            .padding(.trailing)
            .padding(.leading, 12)
        } else {
            ChatItemWithMenu(
                chat: chat,
                ci: ci,
                maxWidth: maxWidth,
                scrollProxy: scrollProxy,
                deleteMessage: deleteMessage,
                deletingItem: $deletingItem,
                composeState: $composeState,
                showDeleteMessage: $showDeleteMessage
            ).padding(.horizontal)
        }
    }
    
    private struct ChatItemWithMenu: View {
        var chat: Chat
        var ci: ChatItem
        var showMember: Bool = false
        var maxWidth: CGFloat
        var scrollProxy: ScrollViewProxy?
        var deleteMessage: (CIDeleteMode) -> Void
        @Binding var deletingItem: ChatItem?
        @Binding var composeState: ComposeState
        @Binding var showDeleteMessage: Bool
        
        @State private var revealed = false
        
        var body: some View {
            let alignment: Alignment = ci.chatDir.sent ? .trailing : .leading
            
            ChatItemView(chatInfo: chat.chatInfo, chatItem: ci, showMember: showMember, maxWidth: maxWidth, scrollProxy: scrollProxy, revealed: $revealed)
                .uiKitContextMenu(actions: menu())
                .confirmationDialog("Delete message?", isPresented: $showDeleteMessage, titleVisibility: .visible) {
                    Button("Delete for me", role: .destructive) {
                        deleteMessage(.cidmInternal)
                    }
                    if let di = deletingItem, di.meta.editable {
                        Button(broadcastDeleteButtonText, role: .destructive) {
                            deleteMessage(.cidmBroadcast)
                        }
                    }
                }
                .frame(maxWidth: maxWidth, maxHeight: .infinity, alignment: alignment)
                .frame(minWidth: 0, maxWidth: .infinity, alignment: alignment)
        }
        
        private func menu() -> [UIAction] {
            var menu: [UIAction] = []
            if let mc = ci.content.msgContent, !ci.meta.itemDeleted || revealed {
                if !ci.meta.itemDeleted {
                    menu.append(replyUIAction())
                }
                menu.append(shareUIAction())
                menu.append(copyUIAction())
                if let filePath = getLoadedFilePath(ci.file) {
                    if case .image = ci.content.msgContent, let image = UIImage(contentsOfFile: filePath) {
                        menu.append(saveImageAction(image))
                    } else {
                        menu.append(saveFileAction(filePath))
                    }
                }
                if ci.meta.editable && !mc.isVoice {
                    menu.append(editAction())
                }
                if revealed {
                    menu.append(hideUIAction())
                }
                menu.append(deleteUIAction())
            } else if ci.meta.itemDeleted {
                menu.append(revealUIAction())
                menu.append(deleteUIAction())
            } else if ci.isDeletedContent {
                menu.append(deleteUIAction())
            }
            return menu
        }
        
        private func replyUIAction() -> UIAction {
            UIAction(
                title: NSLocalizedString("Reply", comment: "chat item action"),
                image: UIImage(systemName: "arrowshape.turn.up.left")
            ) { _ in
                withAnimation {
                    if composeState.editing {
                        composeState = ComposeState(contextItem: .quotedItem(chatItem: ci))
                    } else {
                        composeState = composeState.copy(contextItem: .quotedItem(chatItem: ci))
                    }
                }
            }
        }
        
        private func shareUIAction() -> UIAction {
            UIAction(
                title: NSLocalizedString("Share", comment: "chat item action"),
                image: UIImage(systemName: "square.and.arrow.up")
            ) { _ in
                var shareItems: [Any] = [ci.content.text]
                if case .image = ci.content.msgContent, let image = getLoadedImage(ci.file) {
                    shareItems.append(image)
                }
                showShareSheet(items: shareItems)
            }
        }
        
        private func copyUIAction() -> UIAction {
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
        }
        
        private func saveImageAction(_ image: UIImage) -> UIAction {
            UIAction(
                title: NSLocalizedString("Save", comment: "chat item action"),
                image: UIImage(systemName: "square.and.arrow.down")
            ) { _ in
                UIImageWriteToSavedPhotosAlbum(image, nil, nil, nil)
            }
        }
        
        private func saveFileAction(_ filePath: String) -> UIAction {
            UIAction(
                title: NSLocalizedString("Save", comment: "chat item action"),
                image: UIImage(systemName: "square.and.arrow.down")
            ) { _ in
                let fileURL = URL(fileURLWithPath: filePath)
                showShareSheet(items: [fileURL])
            }
        }
        
        private func editAction() -> UIAction {
            UIAction(
                title: NSLocalizedString("Edit", comment: "chat item action"),
                image: UIImage(systemName: "square.and.pencil")
            ) { _ in
                withAnimation {
                    composeState = ComposeState(editingItem: ci)
                }
            }
        }

        private func hideUIAction() -> UIAction {
            UIAction(
                title: NSLocalizedString("Hide", comment: "chat item action"),
                image: UIImage(systemName: "eye.slash")
            ) { _ in
                withAnimation {
                    revealed = false
                }
            }
        }
        
        private func deleteUIAction() -> UIAction {
            UIAction(
                title: NSLocalizedString("Delete", comment: "chat item action"),
                image: UIImage(systemName: "trash"),
                attributes: [.destructive]
            ) { _ in
                showDeleteMessage = true
                deletingItem = ci
            }
        }
        
        private func revealUIAction() -> UIAction {
            UIAction(
                title: NSLocalizedString("Reveal", comment: "chat item action"),
                image: UIImage(systemName: "eye")
            ) { _ in
                withAnimation {
                    revealed = true
                }
            }
        }
        
        private var broadcastDeleteButtonText: LocalizedStringKey {
            chat.chatInfo.fullDeletionAllowed ? "Delete for everyone" : "Mark deleted for everyone"
        }
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
                    let (deletedItem, toItem) = try await apiDeleteChatItem(
                        type: chat.chatInfo.chatType,
                        id: chat.chatInfo.apiId,
                        itemId: di.id,
                        mode: mode
                    )
                    DispatchQueue.main.async {
                        deletingItem = nil
                        if let toItem = toItem {
                            _ = chatModel.upsertChatItem(chat.chatInfo, toItem)
                        } else {
                            chatModel.removeChatItem(chat.chatInfo, deletedItem)
                        }
                    }
                }
            } catch {
                logger.error("ChatView.deleteMessage error: \(error.localizedDescription)")
            }
        }
    }
}

@ViewBuilder func toggleNtfsButton(_ chat: Chat) -> some View {
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

func toggleNotifications(_ chat: Chat, enableNtfs: Bool) {
    Task {
        do {
            let chatSettings = ChatSettings(enableNtfs: enableNtfs)
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
