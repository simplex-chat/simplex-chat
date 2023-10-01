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
    @State @ObservedObject var chat: Chat
    @State private var showChatInfoSheet: Bool = false
    @State private var showAddMembersSheet: Bool = false
    @State private var composeState = ComposeState()
    @State private var deletingItem: ChatItem? = nil
    @State private var keyboardVisible = false
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
        if #available(iOS 16.0, *) {
            viewBody
            .scrollDismissesKeyboard(.immediately)
            .keyboardPadding()
        } else {
            viewBody
        }
    }

    private var viewBody: some View {
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
            if cId != nil {
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
                        chatModel.reversedChatItems = []
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
                        if contact.allowsFeature(.calls) {
                            callButton(contact, .audio, imageName: "phone")
                                .disabled(!contact.ready)
                        }
                        Menu {
                            if contact.allowsFeature(.calls) {
                                Button {
                                    CallController.shared.startCall(contact, .video)
                                } label: {
                                    Label("Video call", systemImage: "video")
                                }
                                .disabled(!contact.ready)
                            }
                            searchButton()
                            toggleNtfsButton(chat)
                                .disabled(!contact.ready)
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

    private func initChatView() {
        let cInfo = chat.chatInfo
        if case let .direct(contact) = cInfo {
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
        if chatModel.draftChatId == cInfo.id, let draft = chatModel.draft {
            composeState = draft
        }
        if chat.chatStats.unreadChat {
            Task {
                await markChatUnread(chat, unreadChat: false)
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
    
    private func voiceWithoutFrame(_ ci: ChatItem) -> Bool {
        ci.content.msgContent?.isVoice == true && ci.content.text.count == 0 && ci.quotedItem == nil
    }

    private func chatItemsList() -> some View {
        let cInfo = chat.chatInfo
        return GeometryReader { g in
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack(spacing: 0)  {
                        ForEach(chatModel.reversedChatItems, id: \.viewId) { ci in
                            let voiceNoFrame = voiceWithoutFrame(ci)
                            let maxWidth = cInfo.chatType == .group
                                            ? voiceNoFrame
                                                ? (g.size.width - 28) - 42
                                                : (g.size.width - 28) * 0.84 - 42
                                            : voiceNoFrame
                                                ? (g.size.width - 32)
                                                : (g.size.width - 32) * 0.84
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
                    if let chatId = chatModel.chatId, let c = chatModel.getChat(chatId) {
                        chat = c
                        showChatInfoSheet = false
                        loadChat(chat: c)
                        DispatchQueue.main.async {
                            scrollToBottom(proxy)
                        }
                    }
                }
            }
        }
        .scaleEffect(x: 1, y: -1, anchor: .center)
    }

    @ViewBuilder private func connectingText() -> some View {
        if case let .direct(contact) = chat.chatInfo,
           !contact.ready,
           !contact.nextSendGrpInv {
            Text("connecting…")
                .font(.caption)
                .foregroundColor(.secondary)
                .padding(.top)
        } else {
            EmptyView()
        }
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
            let (prevItem, nextItem) = chatModel.getChatItemNeighbors(ci)
            if ci.memberConnected != nil && nextItem?.memberConnected != nil {
                // memberConnected events are aggregated at the last chat item in a row of such events, see ChatItemView
                ZStack {} // scroll doesn't work if it's EmptyView()
            } else {
                if prevItem == nil || showMemberImage(member, prevItem) {
                    VStack(alignment: .leading, spacing: 4) {
                        if ci.content.showMemberName {
                            Text(member.displayName)
                                .font(.caption)
                                .foregroundStyle(.secondary)
                                .padding(.leading, memberImageSize + 14)
                                .padding(.top, 7)
                        }
                        HStack(alignment: .top, spacing: 8) {
                            ProfileImage(imageStr: member.memberProfile.image)
                                .frame(width: memberImageSize, height: memberImageSize)
                                .onTapGesture { selectedMember = member }
                                .appSheet(item: $selectedMember) { member in
                                    GroupMemberInfoView(groupInfo: groupInfo, member: member, navigation: true)
                                }
                            chatItemWithMenu(ci, maxWidth)
                        }
                    }
                    .padding(.top, 5)
                    .padding(.trailing)
                    .padding(.leading, 12)
                } else {
                    chatItemWithMenu(ci, maxWidth)
                        .padding(.top, 5)
                        .padding(.trailing)
                        .padding(.leading, memberImageSize + 8 + 12)
                }
            }
        } else {
            chatItemWithMenu(ci, maxWidth)
                .padding(.horizontal)
                .padding(.top, 5)
        }
    }

    private func chatItemWithMenu(_ ci: ChatItem, _ maxWidth: CGFloat) -> some View {
        ChatItemWithMenu(
            ci: ci,
            maxWidth: maxWidth,
            scrollProxy: scrollProxy,
            deleteMessage: deleteMessage,
            deletingItem: $deletingItem,
            composeState: $composeState,
            showDeleteMessage: $showDeleteMessage
        )
        .environmentObject(chat)
    }

    private struct ChatItemWithMenu: View {
        @EnvironmentObject var chat: Chat
        @Environment(\.colorScheme) var colorScheme
        var ci: ChatItem
        var maxWidth: CGFloat
        var scrollProxy: ScrollViewProxy?
        var deleteMessage: (CIDeleteMode) -> Void
        @Binding var deletingItem: ChatItem?
        @Binding var composeState: ComposeState
        @Binding var showDeleteMessage: Bool

        @State private var revealed = false
        @State private var showChatItemInfoSheet: Bool = false
        @State private var chatItemInfo: ChatItemInfo?

        @State private var allowMenu: Bool = true

        @State private var audioPlayer: AudioPlayer?
        @State private var playbackState: VoiceMessagePlaybackState = .noPlayback
        @State private var playbackTime: TimeInterval?

        var body: some View {
            let alignment: Alignment = ci.chatDir.sent ? .trailing : .leading
            let uiMenu: Binding<UIMenu> = Binding(
                get: { UIMenu(title: "", children: menu(live: composeState.liveMessage != nil)) },
                set: { _ in }
            )
            
            VStack(alignment: alignment.horizontal, spacing: 3) {
                ChatItemView(chatInfo: chat.chatInfo, chatItem: ci, maxWidth: maxWidth, scrollProxy: scrollProxy, revealed: $revealed, allowMenu: $allowMenu, audioPlayer: $audioPlayer, playbackState: $playbackState, playbackTime: $playbackTime)
                    .uiKitContextMenu(menu: uiMenu, allowMenu: $allowMenu)
                    .accessibilityLabel("")
                if ci.content.msgContent != nil && (ci.meta.itemDeleted == nil || revealed) && ci.reactions.count > 0 {
                    chatItemReactions()
                        .padding(.bottom, 4)
                }
            }
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
                .onDisappear {
                    if ci.content.msgContent?.isVoice == true {
                        allowMenu = true
                        audioPlayer?.stop()
                        playbackState = .noPlayback
                        playbackTime = TimeInterval(0)
                    }
                }
                .sheet(isPresented: $showChatItemInfoSheet, onDismiss: {
                    chatItemInfo = nil
                }) {
                    ChatItemInfoView(ci: ci, chatItemInfo: $chatItemInfo)
                }
        }

        private func chatItemReactions() -> some View {
            HStack(spacing: 4) {
                ForEach(ci.reactions, id: \.reaction) { r in
                    let v = HStack(spacing: 4) {
                        switch r.reaction {
                        case let .emoji(emoji): Text(emoji.rawValue).font(.caption)
                        case .unknown: EmptyView()
                        }
                        if r.totalReacted > 1 {
                            Text("\(r.totalReacted)")
                                .font(.caption)
                                .fontWeight(r.userReacted ? .bold : .light)
                                .foregroundColor(r.userReacted ? .accentColor : .secondary)
                        }
                    }
                    .padding(.horizontal, 6)
                    .padding(.vertical, 4)

                    if chat.chatInfo.featureEnabled(.reactions) && (ci.allowAddReaction || r.userReacted) {
                        v.onTapGesture {
                            setReaction(add: !r.userReacted, reaction: r.reaction)
                        }
                    } else {
                        v
                    }
                }
            }
        }

        private func menu(live: Bool) -> [UIMenuElement] {
            var menu: [UIMenuElement] = []
            if let mc = ci.content.msgContent, ci.meta.itemDeleted == nil || revealed {
                let rs = allReactions()
                if chat.chatInfo.featureEnabled(.reactions) && ci.allowAddReaction,
                   rs.count > 0 {
                    var rm: UIMenu
                    if #available(iOS 16, *) {
                        var children: [UIMenuElement] = Array(rs.prefix(topReactionsCount(rs)))
                        if let sm = reactionUIMenu(rs) {
                            children.append(sm)
                        }
                        rm = UIMenu(title: "", options: .displayInline, children: children)
                        rm.preferredElementSize = .small
                    } else {
                        rm = reactionUIMenuPreiOS16(rs)
                    }
                    menu.append(rm)
                }
                if ci.meta.itemDeleted == nil && !ci.isLiveDummy && !live {
                    menu.append(replyUIAction())
                }
                menu.append(shareUIAction())
                menu.append(copyUIAction())
                if let fileSource = getLoadedFileSource(ci.file) {
                    if case .image = ci.content.msgContent, let image = getLoadedImage(ci.file) {
                        if image.imageData != nil {
                            menu.append(saveFileAction(fileSource))
                        } else {
                            menu.append(saveImageAction(image))
                        }
                    } else {
                        menu.append(saveFileAction(fileSource))
                    }
                }
                if ci.meta.editable && !mc.isVoice && !live {
                    menu.append(editAction())
                }
                menu.append(viewInfoUIAction())
                if revealed {
                    menu.append(hideUIAction())
                }
                if ci.meta.itemDeleted == nil,
                   let file = ci.file,
                   let cancelAction = file.cancelAction  {
                    menu.append(cancelFileUIAction(file.fileId, cancelAction))
                }
                if !live || !ci.meta.isLive {
                    menu.append(deleteUIAction())
                }
                if let (groupInfo, _) = ci.memberToModerate(chat.chatInfo) {
                    menu.append(moderateUIAction(groupInfo))
                }
            } else if ci.meta.itemDeleted != nil {
                if !ci.isDeletedContent {
                    menu.append(revealUIAction())
                }
                menu.append(viewInfoUIAction())
                menu.append(deleteUIAction())
            } else if ci.isDeletedContent {
                menu.append(viewInfoUIAction())
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

        private func reactionUIMenuPreiOS16(_ rs: [UIAction]) -> UIMenu {
            UIMenu(
                title: NSLocalizedString("React…", comment: "chat item menu"),
                image: UIImage(systemName: "face.smiling"),
                children: rs
            )
        }

        @available(iOS 16.0, *)
        private func reactionUIMenu(_ rs: [UIAction]) -> UIMenu? {
            var children = rs
            children.removeFirst(min(rs.count, topReactionsCount(rs)))
            if children.count == 0 { return nil }
            return UIMenu(
                title: "",
                image: UIImage(systemName: "ellipsis"),
                children: children
            )
        }

        private func allReactions() -> [UIAction] {
            MsgReaction.values.compactMap { r in
                ci.reactions.contains(where: { $0.userReacted && $0.reaction == r })
                ? nil
                : UIAction(title: r.text) { _ in setReaction(add: true, reaction: r) }
            }
        }

        private func topReactionsCount(_ rs: [UIAction]) -> Int {
            rs.count > 4 ? 3 : 4
        }

        private func setReaction(add: Bool, reaction: MsgReaction) {
            Task {
                do {
                    let cInfo = chat.chatInfo
                    let chatItem = try await apiChatItemReaction(
                        type: cInfo.chatType,
                        id: cInfo.apiId,
                        itemId: ci.id,
                        add: add,
                        reaction: reaction
                    )
                    await MainActor.run {
                        ChatModel.shared.updateChatItem(chat.chatInfo, chatItem)
                    }
                } catch let error {
                    logger.error("apiChatItemReaction error: \(responseError(error))")
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
        
        private func saveFileAction(_ fileSource: CryptoFile) -> UIAction {
            UIAction(
                title: NSLocalizedString("Save", comment: "chat item action"),
                image: UIImage(systemName: fileSource.cryptoArgs == nil ? "square.and.arrow.down" : "lock.open")
            ) { _ in
                saveCryptoFile(fileSource)
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

        private func viewInfoUIAction() -> UIAction {
            UIAction(
                title: NSLocalizedString("Info", comment: "chat item action"),
                image: UIImage(systemName: "info.circle")
            ) { _ in
                Task {
                    do {
                        let cInfo = chat.chatInfo
                        let ciInfo = try await apiGetChatItemInfo(type: cInfo.chatType, id: cInfo.apiId, itemId: ci.id)
                        await MainActor.run {
                            chatItemInfo = ciInfo
                        }
                        if case let .group(gInfo) = chat.chatInfo {
                            let groupMembers = await apiListMembers(gInfo.groupId)
                            await MainActor.run {
                                ChatModel.shared.groupMembers = groupMembers
                            }
                        }
                    } catch let error {
                        logger.error("apiGetChatItemInfo error: \(responseError(error))")
                    }
                    await MainActor.run { showChatItemInfoSheet = true }
                }
            }
        }

        private func cancelFileUIAction(_ fileId: Int64, _ cancelAction: CancelAction) -> UIAction {
            return UIAction(
                title: cancelAction.uiAction,
                image: UIImage(systemName: "xmark"),
                attributes: [.destructive]
            ) { _ in
                AlertManager.shared.showAlert(Alert(
                    title: Text(cancelAction.alert.title),
                    message: Text(cancelAction.alert.message),
                    primaryButton: .destructive(Text(cancelAction.alert.confirm)) {
                        Task {
                            if let user = ChatModel.shared.currentUser {
                                await cancelFile(user: user, fileId: fileId)
                            }
                        }
                    },
                    secondaryButton: .cancel()
                ))
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

        private func moderateUIAction(_ groupInfo: GroupInfo) -> UIAction {
            UIAction(
                title: NSLocalizedString("Moderate", comment: "chat item action"),
                image: UIImage(systemName: "flag"),
                attributes: [.destructive]
            ) { _ in
                AlertManager.shared.showAlert(Alert(
                    title: Text("Delete member message?"),
                    message: Text(
                                groupInfo.fullGroupPreferences.fullDelete.on
                                ? "The message will be deleted for all members."
                                : "The message will be marked as moderated for all members."
                            ),
                    primaryButton: .destructive(Text("Delete")) {
                        deletingItem = ci
                        deleteMessage(.cidmBroadcast)
                    },
                    secondaryButton: .cancel()
                ))
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
            chat.chatInfo.featureEnabled(.fullDelete) ? "Delete for everyone" : "Mark deleted for everyone"
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
                    var deletedItem: ChatItem
                    var toItem: ChatItem?
                    if case .cidmBroadcast = mode,
                       let (groupInfo, groupMember) = di.memberToModerate(chat.chatInfo) {
                        (deletedItem, toItem) = try await apiDeleteMemberChatItem(
                            groupId: groupInfo.apiId,
                            groupMemberId: groupMember.groupMemberId,
                            itemId: di.id
                        )
                    } else {
                        (deletedItem, toItem) = try await apiDeleteChatItem(
                            type: chat.chatInfo.chatType,
                            id: chat.chatInfo.apiId,
                            itemId: di.id,
                            mode: mode
                        )
                    }
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
    var chatSettings = chat.chatInfo.chatSettings ?? ChatSettings.defaults
    chatSettings.enableNtfs = enableNtfs
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
