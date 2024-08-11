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
import Combine

private let memberImageSize: CGFloat = 34

struct ChatView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var im = ItemsModel.shared
    @State var theme: AppTheme = buildTheme()
    @Environment(\.dismiss) var dismiss
    @Environment(\.colorScheme) var colorScheme
    @Environment(\.presentationMode) var presentationMode
    @Environment(\.scenePhase) var scenePhase
    @State @ObservedObject var chat: Chat
    @StateObject private var scrollModel = ReverseListScrollModel<ChatItem>()
    @StateObject private var floatingButtonModel = FloatingButtonModel()
    @State private var showChatInfoSheet: Bool = false
    @State private var showAddMembersSheet: Bool = false
    @State private var composeState = ComposeState()
    @State private var keyboardVisible = false
    @State private var connectionStats: ConnectionStats?
    @State private var customUserProfile: Profile?
    @State private var connectionCode: String?
    @State private var loadingItems = false
    @State private var firstPage = false
    @State private var revealedChatItem: ChatItem?
    @State private var searchMode = false
    @State private var searchText: String = ""
    @FocusState private var searchFocussed
    // opening GroupMemberInfoView on member icon
    @State private var selectedMember: GMember? = nil
    // opening GroupLinkView on link button (incognito)
    @State private var showGroupLinkSheet: Bool = false
    @State private var groupLink: String?
    @State private var groupLinkMemberRole: GroupMemberRole = .member
    @State private var selectedChatItems: Set<Int64>? = nil
    @State private var showDeleteSelectedMessages: Bool = false
    @State private var allowToDeleteSelectedMessagesForAll: Bool = false

    @AppStorage(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial

    var body: some View {
        if #available(iOS 16.0, *) {
            viewBody
                .scrollDismissesKeyboard(.immediately)
                .keyboardPadding()
                .toolbarBackground(.hidden, for: .navigationBar)
        } else {
            viewBody
        }
    }
    
    @ViewBuilder
    private var viewBody: some View {
        let cInfo = chat.chatInfo
        ZStack {
            let wallpaperImage = theme.wallpaper.type.image
            let wallpaperType = theme.wallpaper.type
            let backgroundColor = theme.wallpaper.background ?? wallpaperType.defaultBackgroundColor(theme.base, theme.colors.background)
            let tintColor = theme.wallpaper.tint ?? wallpaperType.defaultTintColor(theme.base)
            Color.clear.ignoresSafeArea(.all)
                .if(wallpaperImage != nil) { view in
                    view.modifier(
                        ChatViewBackground(image: wallpaperImage!, imageType: wallpaperType, background: backgroundColor, tint: tintColor)
                    )
            }
            VStack(spacing: 0) {
                ZStack(alignment: .bottomTrailing) {
                    chatItemsList()
                    floatingButtons(counts: floatingButtonModel.unreadChatItemCounts)
                }
                connectingText()
                if selectedChatItems == nil {
                    ComposeView(
                        chat: chat,
                        composeState: $composeState,
                        keyboardVisible: $keyboardVisible
                    )
                    .disabled(!cInfo.sendMsgEnabled)
                } else {
                    SelectedItemsBottomToolbar(
                        chatItems: ItemsModel.shared.reversedChatItems,
                        selectedChatItems: $selectedChatItems,
                        chatInfo: chat.chatInfo,
                        deleteItems: { forAll in
                            allowToDeleteSelectedMessagesForAll = forAll
                            showDeleteSelectedMessages = true
                        },
                        moderateItems: {
                            if case let .group(groupInfo) = chat.chatInfo {
                                showModerateSelectedMessagesAlert(groupInfo)
                            }
                        }
                    )
                }
            }
        }
        .safeAreaInset(edge: .top) {
            VStack(spacing: .zero) {
                if searchMode { searchToolbar() }
                Divider()
            }
            .background(ToolbarMaterial.material(toolbarMaterial))
        }
        .navigationTitle(cInfo.chatViewName)
        .background(theme.colors.background)
        .navigationBarTitleDisplayMode(.inline)
        .environmentObject(theme)
        .confirmationDialog(selectedChatItems?.count == 1 ? "Delete message?" : "Delete \((selectedChatItems?.count ?? 0)) messages?", isPresented: $showDeleteSelectedMessages, titleVisibility: .visible) {
            Button("Delete for me", role: .destructive) {
                if let selected = selectedChatItems {
                    deleteMessages(chat, selected.sorted(), .cidmInternal, moderate: false, deletedSelectedMessages)                }
            }
            if allowToDeleteSelectedMessagesForAll {
                Button(broadcastDeleteButtonText(chat), role: .destructive) {
                    if let selected = selectedChatItems {
                        allowToDeleteSelectedMessagesForAll = false
                        deleteMessages(chat, selected.sorted(), .cidmBroadcast, moderate: false, deletedSelectedMessages)
                    }
                }
            }
        }
        .onAppear {
            loadChat(chat: chat)
            initChatView()
            selectedChatItems = nil
        }
        .onChange(of: chatModel.chatId) { cId in
            showChatInfoSheet = false
            stopAudioPlayer()
            if let cId {
                selectedChatItems = nil
                if let c = chatModel.getChat(cId) {
                    chat = c
                }
                initChatView()
                theme = buildTheme()
            } else {
                dismiss()
            }
        }
        .onChange(of: revealedChatItem) { _ in
            NotificationCenter.postReverseListNeedsLayout()
        }
        .onChange(of: im.reversedChatItems) { reversedChatItems in
            if reversedChatItems.count <= loadItemsPerPage && filtered(reversedChatItems).count < 10 {
                loadChatItems(chat.chatInfo)
            }
        }
        .environmentObject(scrollModel)
        .onDisappear {
            VideoPlayerView.players.removeAll()
            stopAudioPlayer()
            if chatModel.chatId == cInfo.id && !presentationMode.wrappedValue.isPresented {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) {
                    if chatModel.chatId == nil {
                        chatModel.chatItemStatuses = [:]
                        ItemsModel.shared.reversedChatItems = []
                        chatModel.groupMembers = []
                        chatModel.groupMembersIndexes.removeAll()
                        chatModel.membersLoaded = false
                    }
                }
            }
        }
        .onChange(of: colorScheme) { _ in
            theme = buildTheme()
        }
        .toolbar {
            ToolbarItem(placement: .principal) {
                if selectedChatItems != nil {
                    SelectedItemsTopToolbar(selectedChatItems: $selectedChatItems)
                } else if case let .direct(contact) = cInfo {
                    Button {
                        Task {
                            showChatInfoSheet = true
                        }
                    } label: {
                        ChatInfoToolbar(chat: chat)
                    }
                    .appSheet(isPresented: $showChatInfoSheet) {
                        ChatInfoView(
                            chat: chat,
                            contact: contact,
                            localAlias: chat.chatInfo.localAlias,
                            onSearch: { focusSearch() }
                        )
                    }
                } else if case let .group(groupInfo) = cInfo {
                    Button {
                        Task { await chatModel.loadGroupMembers(groupInfo) { showChatInfoSheet = true } }
                    } label: {
                        ChatInfoToolbar(chat: chat)
                            .tint(theme.colors.primary)
                    }
                    .appSheet(isPresented: $showChatInfoSheet, onDismiss: { theme = buildTheme() }) {
                        GroupChatInfoView(
                            chat: chat,
                            groupInfo: Binding(
                                get: { groupInfo },
                                set: { gInfo in
                                    chat.chatInfo = .group(groupInfo: gInfo)
                                    chat.created = Date.now
                                }
                            ),
                            onSearch: { focusSearch() }
                        )
                    }
                } else if case .local = cInfo {
                    ChatInfoToolbar(chat: chat)
                }
            }
            ToolbarItem(placement: .navigationBarTrailing) {
                if selectedChatItems != nil {
                    Button {
                        withAnimation {
                            selectedChatItems = nil
                        }
                    } label: {
                        Text("Cancel")
                    }
                } else {
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
                    .foregroundColor(theme.colors.onBackground)
                    .frame(maxWidth: .infinity)

                Button {
                    searchText = ""
                } label: {
                    Image(systemName: "xmark.circle.fill").opacity(searchText == "" ? 0 : 1)
                }
            }
            .padding(EdgeInsets(top: 7, leading: 7, bottom: 7, trailing: 7))
            .foregroundColor(theme.colors.secondary)
            .background(Color(.tertiarySystemFill))
            .cornerRadius(10.0)

            Button ("Cancel") {
                searchText = ""
                searchMode = false
                searchFocussed = false
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) {
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

    private func filtered(_ reversedChatItems: Array<ChatItem>) -> Array<ChatItem> {
        reversedChatItems
            .enumerated()
            .filter { (index, chatItem) in
                if let mergeCategory = chatItem.mergeCategory, index > 0 {
                    mergeCategory != reversedChatItems[index - 1].mergeCategory
                } else {
                    true
                }
            }
            .map { $0.element }
    }


    private func chatItemsList() -> some View {
        let cInfo = chat.chatInfo
        let mergedItems = filtered(im.reversedChatItems)
        return GeometryReader { g in
            ReverseList(items: mergedItems, scrollState: $scrollModel.state) { ci in
                let voiceNoFrame = voiceWithoutFrame(ci)
                let maxWidth = cInfo.chatType == .group
                                ? voiceNoFrame
                                    ? (g.size.width - 28) - 42
                                    : (g.size.width - 28) * 0.84 - 42
                                : voiceNoFrame
                                    ? (g.size.width - 32)
                                    : (g.size.width - 32) * 0.84
                return ChatItemWithMenu(
                    chat: $chat,
                    chatItem: ci,
                    maxWidth: maxWidth,
                    composeState: $composeState,
                    selectedMember: $selectedMember,
                    revealedChatItem: $revealedChatItem,
                    selectedChatItems: $selectedChatItems
                )
                .onAppear {
                    floatingButtonModel.appeared(viewId: ci.viewId)
                }
                .onDisappear {
                    floatingButtonModel.disappeared(viewId: ci.viewId)
                }
                .id(ci.id) // Required to trigger `onAppear` on iOS15
            } loadPage: {
                loadChatItems(cInfo)
            }
            .padding(.vertical, -InvertedTableView.inset)
            .onTapGesture { hideKeyboard() }
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
            .onChange(of: im.reversedChatItems) { _ in
                floatingButtonModel.chatItemsChanged()
            }
        }
    }

    @ViewBuilder private func connectingText() -> some View {
        if case let .direct(contact) = chat.chatInfo,
           !contact.sndReady,
           contact.active,
           !contact.nextSendGrpInv {
            Text("connecting…")
                .font(.caption)
                .foregroundColor(theme.colors.secondary)
                .padding(.top)
        } else {
            EmptyView()
        }
    }

    class FloatingButtonModel: ObservableObject {
        private enum Event {
            case appeared(String)
            case disappeared(String)
            case chatItemsChanged
        }

        @Published var unreadChatItemCounts: UnreadChatItemCounts

        private let events = PassthroughSubject<Event, Never>()
        private var bag = Set<AnyCancellable>()

        init() {
            unreadChatItemCounts = UnreadChatItemCounts(
                isNearBottom: true,
                unreadBelow: 0
            )
            events
                .receive(on: DispatchQueue.global(qos: .background))
                .scan(Set<String>()) { itemsInView, event in
                    return switch event {
                    case let .appeared(viewId):
                        itemsInView.union([viewId])
                    case let .disappeared(viewId):
                        itemsInView.subtracting([viewId])
                    case .chatItemsChanged:
                        itemsInView
                    }
                }
                .map { ChatModel.shared.unreadChatItemCounts(itemsInView: $0) }
                .removeDuplicates()
                .throttle(for: .seconds(0.2), scheduler: DispatchQueue.main, latest: true)
                .assign(to: \.unreadChatItemCounts, on: self)
                .store(in: &bag)
        }

        func appeared(viewId: String) {
            events.send(.appeared(viewId))
        }

        func disappeared(viewId: String) {
            events.send(.disappeared(viewId))
        }

        func chatItemsChanged() {
            events.send(.chatItemsChanged)
        }
    }

    private func floatingButtons(counts: UnreadChatItemCounts) -> some View {
        VStack {
            let unreadAbove = chat.chatStats.unreadCount - counts.unreadBelow
            if unreadAbove > 0 {
                circleButton {
                    unreadCountText(unreadAbove)
                        .font(.callout)
                        .foregroundColor(theme.colors.primary)
                }
                .onTapGesture {
                    scrollModel.scrollToNextPage()
                }
                .contextMenu {
                    Button {
                        Task {
                            await markChatRead(chat)
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
                        .foregroundColor(theme.colors.primary)
                }
                .onTapGesture {
                    scrollModel.scrollToBottom()
                }
            } else if !counts.isNearBottom {
                circleButton {
                    Image(systemName: "chevron.down")
                        .foregroundColor(theme.colors.primary)
                }
                .onTapGesture { scrollModel.scrollToBottom() }
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
            focusSearch()
        } label: {
            Label("Search", systemImage: "magnifyingglass")
        }
    }

    private func focusSearch() {
        searchMode = true
        searchFocussed = true
        searchText = ""
    }

    private func addMembersButton() -> some View {
        Button {
            if case let .group(gInfo) = chat.chatInfo {
                Task { await chatModel.loadGroupMembers(gInfo) { showAddMembersSheet = true } }
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

    private func showModerateSelectedMessagesAlert(_ groupInfo: GroupInfo) {
        guard let count = selectedChatItems?.count, count > 0 else { return }

        AlertManager.shared.showAlert(Alert(
            title: Text(count == 1 ? "Delete member message?" : "Delete \(count) messages of members?"),
            message: Text(
                groupInfo.fullGroupPreferences.fullDelete.on
                ? (count == 1 ? "The message will be deleted for all members." : "The messages will be deleted for all members.")
                : (count == 1 ? "The message will be marked as moderated for all members." : "The messages will be marked as moderated for all members.")
            ),
            primaryButton: .destructive(Text("Delete")) {
                if let selected = selectedChatItems {
                    deleteMessages(chat, selected.sorted(), .cidmBroadcast, moderate: true, deletedSelectedMessages)
                }
            },
            secondaryButton: .cancel()
        ))
    }

    private func deletedSelectedMessages() async {
        await MainActor.run {
            withAnimation {
                selectedChatItems = nil
            }
        }
    }

    private func loadChatItems(_ cInfo: ChatInfo) {
        Task {
            if loadingItems || firstPage { return }
            loadingItems = true
            do {
                var reversedPage = Array<ChatItem>()
                var chatItemsAvailable = true
                // Load additional items until the page is +50 large after merging
                while chatItemsAvailable && filtered(reversedPage).count < loadItemsPerPage {
                    let pagination: ChatPagination =
                        if let lastItem = reversedPage.last ?? im.reversedChatItems.last {
                            .before(chatItemId: lastItem.id, count: loadItemsPerPage)
                        } else {
                            .last(count: loadItemsPerPage)
                        }
                    let chatItems = try await apiGetChatItems(
                        type: cInfo.chatType,
                        id: cInfo.apiId,
                        pagination: pagination,
                        search: searchText
                    )
                    chatItemsAvailable = !chatItems.isEmpty
                    reversedPage.append(contentsOf: chatItems.reversed())
                }
                await MainActor.run {
                    if reversedPage.count == 0 {
                        firstPage = true
                    } else {
                        im.reversedChatItems.append(contentsOf: reversedPage)
                    }
                    loadingItems = false
                }
            } catch let error {
                logger.error("apiGetChat error: \(responseError(error))")
                await MainActor.run { loadingItems = false }
            }
        }
    }

    func stopAudioPlayer() {
        VoiceItemState.chatView.values.forEach { $0.audioPlayer?.stop() }
        VoiceItemState.chatView = [:]
    }

    private struct ChatItemWithMenu: View {
        @EnvironmentObject var m: ChatModel
        @EnvironmentObject var theme: AppTheme
        @Binding @ObservedObject var chat: Chat
        let chatItem: ChatItem
        let maxWidth: CGFloat
        @Binding var composeState: ComposeState
        @Binding var selectedMember: GMember?
        @Binding var revealedChatItem: ChatItem?

        @State private var deletingItem: ChatItem? = nil
        @State private var showDeleteMessage = false
        @State private var deletingItems: [Int64] = []
        @State private var showDeleteMessages = false
        @State private var showChatItemInfoSheet: Bool = false
        @State private var chatItemInfo: ChatItemInfo?
        @State private var showForwardingSheet: Bool = false

        @Binding var selectedChatItems: Set<Int64>?

        @State private var allowMenu: Bool = true

        var revealed: Bool { chatItem == revealedChatItem }

        var body: some View {
            let (currIndex, _) = m.getNextChatItem(chatItem)
            let ciCategory = chatItem.mergeCategory
            let (prevHidden, prevItem) = m.getPrevShownChatItem(currIndex, ciCategory)
            let range = itemsRange(currIndex, prevHidden)
            let im = ItemsModel.shared
            Group {
                if revealed, let range = range {
                    let items = Array(zip(Array(range), im.reversedChatItems[range]))
                    ForEach(items, id: \.1.viewId) { (i, ci) in
                        let prev = i == prevHidden ? prevItem : im.reversedChatItems[i + 1]
                        chatItemView(ci, nil, prev)
                        .overlay {
                            if let selected = selectedChatItems, ci.canBeDeletedForSelf {
                                Color.clear
                                    .contentShape(Rectangle())
                                    .onTapGesture {
                                        let checked = selected.contains(ci.id)
                                        selectUnselectChatItem(select: !checked, ci)
                                    }
                            }
                        }
                    }
                } else {
                    chatItemView(chatItem, range, prevItem)
                    .overlay {
                        if let selected = selectedChatItems, chatItem.canBeDeletedForSelf {
                            Color.clear
                                .contentShape(Rectangle())
                                .onTapGesture {
                                    let checked = selected.contains(chatItem.id)
                                    selectUnselectChatItem(select: !checked, chatItem)
                                }
                        }
                    }
                }
            }
            .onAppear {
                if let range {
                    if let items = unreadItems(range) {
                        waitToMarkRead {
                            for ci in items {
                                await apiMarkChatItemRead(chat.chatInfo, ci)
                            }
                        }
                    }
                } else if chatItem.isRcvNew  {
                    waitToMarkRead {
                        await apiMarkChatItemRead(chat.chatInfo, chatItem)
                    }
                }
            }
        }
        
        private func unreadItems(_ range: ClosedRange<Int>) -> [ChatItem]? {
            let im = ItemsModel.shared
            let items = range.compactMap { i in
                if i >= 0 && i < im.reversedChatItems.count {
                    let ci = im.reversedChatItems[i]
                    return if ci.isRcvNew { ci } else { nil }
                } else {
                    return nil
                }
            }
            return if items.isEmpty { nil } else { items }
        }
        
        private func waitToMarkRead(_ op: @Sendable @escaping () async -> Void) {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) {
                if m.chatId == chat.chatInfo.id {
                    Task(operation: op)
                }
            }
        }

        @ViewBuilder func chatItemView(_ ci: ChatItem, _ range: ClosedRange<Int>?, _ prevItem: ChatItem?) -> some View {
            if case let .groupRcv(member) = ci.chatDir,
               case let .group(groupInfo) = chat.chatInfo {
                let (prevMember, memCount): (GroupMember?, Int) =
                if let range = range {
                    m.getPrevHiddenMember(member, range)
                } else {
                    (nil, 1)
                }
                if prevItem == nil || showMemberImage(member, prevItem) || prevMember != nil {
                    VStack(alignment: .leading, spacing: 4) {
                        if ci.content.showMemberName {
                            let t = if memCount == 1 && member.memberRole > .member {
                                Text(member.memberRole.text + " ").fontWeight(.semibold) + Text(member.displayName)
                            } else {
                                Text(memberNames(member, prevMember, memCount))
                            }
                            t
                                .font(.caption)
                                .foregroundStyle(.secondary)
                                .lineLimit(2)
                                .padding(.leading, memberImageSize + 14 + (selectedChatItems != nil && ci.canBeDeletedForSelf ? 12 + 24 : 0))
                                .padding(.top, 7)
                        }
                        HStack(alignment: .center, spacing: 0) {
                            if selectedChatItems != nil && ci.canBeDeletedForSelf {
                                SelectedChatItem(ciId: ci.id, selectedChatItems: $selectedChatItems)
                                    .padding(.trailing, 12)
                            }
                            HStack(alignment: .top, spacing: 8) {
                                MemberProfileImage(member, size: memberImageSize, backgroundColor: theme.colors.background)
                                    .onTapGesture {
                                        if m.membersLoaded {
                                            selectedMember = m.getGroupMember(member.groupMemberId)
                                        } else {
                                            Task {
                                                await m.loadGroupMembers(groupInfo) {
                                                    selectedMember = m.getGroupMember(member.groupMemberId)
                                                }
                                            }
                                        }
                                    }
                                    .appSheet(item: $selectedMember) { member in
                                        GroupMemberInfoView(groupInfo: groupInfo, groupMember: member, navigation: true)
                                    }
                                chatItemWithMenu(ci, range, maxWidth)
                            }
                        }
                    }
                    .padding(.bottom, 5)
                    .padding(.trailing)
                    .padding(.leading, 12)
                } else {
                    HStack(alignment: .center, spacing: 0) {
                        if selectedChatItems != nil && ci.canBeDeletedForSelf {
                            SelectedChatItem(ciId: ci.id, selectedChatItems: $selectedChatItems)
                                .padding(.leading, 12)
                        }
                        chatItemWithMenu(ci, range, maxWidth)
                            .padding(.trailing)
                            .padding(.leading, memberImageSize + 8 + 12)
                    }
                    .padding(.bottom, 5)
                }
            } else {
                HStack(alignment: .center, spacing: 0) {
                    if selectedChatItems != nil && ci.canBeDeletedForSelf {
                        if chat.chatInfo.chatType == .group {
                            SelectedChatItem(ciId: ci.id, selectedChatItems: $selectedChatItems)
                                .padding(.leading, 12)
                        } else {
                            SelectedChatItem(ciId: ci.id, selectedChatItems: $selectedChatItems)
                                .padding(.leading)
                        }
                    }
                    chatItemWithMenu(ci, range, maxWidth)
                        .padding(.horizontal)
                }
                .padding(.bottom, 5)
            }
        }

        private func memberNames(_ member: GroupMember, _ prevMember: GroupMember?, _ memCount: Int) -> LocalizedStringKey {
            let name = member.displayName
            return if let prevName = prevMember?.displayName {
                memCount > 2
                ? "\(name), \(prevName) and \(memCount - 2) members"
                : "\(name) and \(prevName)"
            } else {
                "\(name)"
            }
        }

        @ViewBuilder func chatItemWithMenu(_ ci: ChatItem, _ range: ClosedRange<Int>?, _ maxWidth: CGFloat) -> some View {
            let alignment: Alignment = ci.chatDir.sent ? .trailing : .leading
            VStack(alignment: alignment.horizontal, spacing: 3) {
                ChatItemView(
                    chat: chat,
                    chatItem: ci,
                    maxWidth: maxWidth,
                    revealed: .constant(revealed),
                    allowMenu: $allowMenu
                )
                .modifier(ChatItemClipped(ci))
                .contextMenu { menu(ci, range, live: composeState.liveMessage != nil) }
                .accessibilityLabel("")
                if ci.content.msgContent != nil && (ci.meta.itemDeleted == nil || revealed) && ci.reactions.count > 0 {
                    chatItemReactions(ci)
                        .padding(.bottom, 4)
                }
            }
                .confirmationDialog("Delete message?", isPresented: $showDeleteMessage, titleVisibility: .visible) {
                    Button("Delete for me", role: .destructive) {
                        deleteMessage(.cidmInternal, moderate: false)
                    }
                    if let di = deletingItem, di.meta.deletable && !di.localNote {
                        Button(broadcastDeleteButtonText(chat), role: .destructive) {
                            deleteMessage(.cidmBroadcast, moderate: false)
                        }
                    }
                }
                .confirmationDialog(deleteMessagesTitle, isPresented: $showDeleteMessages, titleVisibility: .visible) {
                    Button("Delete for me", role: .destructive) {
                        deleteMessages(chat, deletingItems, moderate: false)
                    }
                }
                .frame(maxWidth: maxWidth, maxHeight: .infinity, alignment: alignment)
                .frame(minWidth: 0, maxWidth: .infinity, alignment: alignment)
                .sheet(isPresented: $showChatItemInfoSheet, onDismiss: {
                    chatItemInfo = nil
                }) {
                    ChatItemInfoView(ci: ci, chatItemInfo: $chatItemInfo)
                }
                .sheet(isPresented: $showForwardingSheet) {
                    if #available(iOS 16.0, *) {
                        ChatItemForwardingView(ci: ci, fromChatInfo: chat.chatInfo, composeState: $composeState)
                            .presentationDetents([.fraction(0.8)])
                    } else {
                        ChatItemForwardingView(ci: ci, fromChatInfo: chat.chatInfo, composeState: $composeState)
                    }
                }
        }

        private func showMemberImage(_ member: GroupMember, _ prevItem: ChatItem?) -> Bool {
            switch (prevItem?.chatDir) {
            case .groupSnd: return true
            case let .groupRcv(prevMember): return prevMember.groupMemberId != member.groupMemberId
            default: return false
            }
        }

        private func chatItemReactions(_ ci: ChatItem) -> some View {
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
                                .foregroundColor(r.userReacted ? theme.colors.primary : theme.colors.secondary)
                        }
                    }
                    .padding(.horizontal, 6)
                    .padding(.vertical, 4)

                    if chat.chatInfo.featureEnabled(.reactions) && (ci.allowAddReaction || r.userReacted) {
                        v.onTapGesture {
                            setReaction(ci, add: !r.userReacted, reaction: r.reaction)
                        }
                    } else {
                        v
                    }
                }
            }
        }

        @ViewBuilder
        private func menu(_ ci: ChatItem, _ range: ClosedRange<Int>?, live: Bool) -> some View {
            if let mc = ci.content.msgContent, ci.meta.itemDeleted == nil || revealed {
                if chat.chatInfo.featureEnabled(.reactions) && ci.allowAddReaction,
                   availableReactions.count > 0 {
                    reactionsGroup
                }
                if ci.meta.itemDeleted == nil && !ci.isLiveDummy && !live && !ci.localNote {
                    replyButton
                }
                let fileSource = getLoadedFileSource(ci.file)
                let fileExists = if let fs = fileSource, FileManager.default.fileExists(atPath: getAppFilePath(fs.filePath).path) { true } else { false }
                let copyAndShareAllowed = !ci.content.text.isEmpty || (ci.content.msgContent?.isImage == true && fileExists)
                if copyAndShareAllowed {
                    shareButton(ci)
                    copyButton(ci)
                }
                if let fileSource = fileSource, fileExists {
                    if case .image = ci.content.msgContent, let image = getLoadedImage(ci.file) {
                        if image.imageData != nil {
                            saveButton(file: fileSource)
                        } else {
                            saveButton(image: image)
                        }
                    } else {
                        saveButton(file: fileSource)
                    }
                } else if let file = ci.file, case .rcvInvitation = file.fileStatus, fileSizeValid(file) {
                    downloadButton(file: file)
                }
                if ci.meta.editable && !mc.isVoice && !live {
                    editButton(chatItem)
                }
                if ci.meta.itemDeleted == nil
                    && (ci.file == nil || (fileSource != nil && fileExists))
                    && !ci.isLiveDummy && !live {
                    forwardButton
                }
                if !ci.isLiveDummy {
                    viewInfoButton(ci)
                }
                if revealed {
                   hideButton()
                }
                if ci.meta.itemDeleted == nil && !ci.localNote,
                   let file = ci.file,
                   let cancelAction = file.cancelAction {
                    cancelFileButton(file.fileId, cancelAction)
                }
                if !live || !ci.meta.isLive {
                    deleteButton(ci)
                }
                if let (groupInfo, _) = ci.memberToModerate(chat.chatInfo), ci.chatDir != .groupSnd {
                    moderateButton(ci, groupInfo)
                }
            } else if ci.meta.itemDeleted != nil {
                if revealed {
                    hideButton()
                } else if !ci.isDeletedContent {
                    revealButton(ci)
                } else if range != nil {
                    expandButton()
                }
                viewInfoButton(ci)
                deleteButton(ci)
            } else if ci.isDeletedContent {
                viewInfoButton(ci)
                deleteButton(ci)
            } else if ci.mergeCategory != nil && ((range?.count ?? 0) > 1 || revealed) {
                if revealed { shrinkButton() } else { expandButton() }
                deleteButton(ci)
            } else if ci.showLocalDelete {
                deleteButton(ci)
            } else {
                EmptyView()
            }
            if selectedChatItems == nil && ci.canBeDeletedForSelf {
                Divider()
                selectButton(ci)
            }
        }

        var replyButton: Button<some View> {
            Button {
                withAnimation {
                    if composeState.editing {
                        composeState = ComposeState(contextItem: .quotedItem(chatItem: chatItem))
                    } else {
                        composeState = composeState.copy(contextItem: .quotedItem(chatItem: chatItem))
                    }
                }
            } label: {
                Label(
                    NSLocalizedString("Reply", comment: "chat item action"),
                    systemImage: "arrowshape.turn.up.left"
                )
            }
        }

        var forwardButton: Button<some View> {
            Button {
                showForwardingSheet = true
            } label: {
                Label(
                    NSLocalizedString("Forward", comment: "chat item action"),
                    systemImage: "arrowshape.turn.up.forward"
                )
            }
        }

        private var reactionsGroup: some View {
            if #available(iOS 16.4, *) {
                return ControlGroup {
                    if availableReactions.count > 4 {
                        reactions(till: 3)
                        Menu {
                            reactions(from: 3)
                        } label: {
                            Image(systemName: "ellipsis")
                        }
                    } else { reactions() }
                }.controlGroupStyle(.compactMenu)
            } else {
                return Menu {
                    reactions()
                } label: {
                    Label(
                        NSLocalizedString("React…", comment: "chat item menu"),
                        systemImage: "face.smiling"
                    )
                }
            }
        }

        func reactions(from: Int? = nil, till: Int? = nil) -> some View {
            ForEach(availableReactions[(from ?? 0)..<(till ?? availableReactions.count)]) { reaction in
                Button(reaction.text) {
                    setReaction(chatItem, add: true, reaction: reaction)
                }
            }
        }

        /// Reactions, which has not been used yet
        private var availableReactions: Array<MsgReaction> {
            MsgReaction.values
                .filter { reaction in
                    !chatItem.reactions.contains {
                        $0.userReacted && $0.reaction == reaction
                    }
                }
        }

        private func setReaction(_ ci: ChatItem, add: Bool, reaction: MsgReaction) {
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
                        m.updateChatItem(chat.chatInfo, chatItem)
                    }
                } catch let error {
                    logger.error("apiChatItemReaction error: \(responseError(error))")
                }
            }
        }

        private func shareButton(_ ci: ChatItem) -> Button<some View> {
            Button {
                var shareItems: [Any] = [ci.content.text]
                if case .image = ci.content.msgContent, let image = getLoadedImage(ci.file) {
                    shareItems.append(image)
                }
                showShareSheet(items: shareItems)
            } label: {
                Label(
                    NSLocalizedString("Share", comment: "chat item action"),
                    systemImage: "square.and.arrow.up"
                )
            }
        }

        private func copyButton(_ ci: ChatItem) -> Button<some View> {
            Button {
                if case let .image(text, _) = ci.content.msgContent,
                   text == "",
                   let image = getLoadedImage(ci.file) {
                    UIPasteboard.general.image = image
                } else {
                    UIPasteboard.general.string = ci.content.text
                }
            } label: {
                Label("Copy", systemImage: "doc.on.doc")
            }
        }

        func saveButton(image: UIImage) -> Button<some View> {
            Button {
                UIImageWriteToSavedPhotosAlbum(image, nil, nil, nil)
            } label: {
                Label(
                    NSLocalizedString("Save", comment: "chat item action"),
                    systemImage: "square.and.arrow.down"
                )
            }
        }

        func saveButton(file: CryptoFile) -> Button<some View> {
            Button {
                saveCryptoFile(file)
            } label: {
                Label(
                    NSLocalizedString("Save", comment: "chat item action"),
                    systemImage: file.cryptoArgs == nil ? "square.and.arrow.down" : "lock.open"
                )
            }
        }

        func downloadButton(file: CIFile) -> Button<some View> {
            Button {
                Task {
                    logger.debug("ChatView downloadFileAction, in Task")
                    if let user = m.currentUser {
                        await receiveFile(user: user, fileId: file.fileId)
                    }
                }
            } label: {
                Label(
                    NSLocalizedString("Download", comment: "chat item action"),
                    systemImage: "arrow.down.doc"
                )
            }
        }

        private func editButton(_ ci: ChatItem) -> Button<some View> {
            Button {
                withAnimation {
                    composeState = ComposeState(editingItem: ci)
                }
            } label: {
                Label(
                    NSLocalizedString("Edit", comment: "chat item action"),
                    systemImage: "square.and.pencil"
                )
            }
        }

        private func selectButton(_ ci: ChatItem) -> Button<some View> {
            Button {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                    withAnimation {
                        selectUnselectChatItem(select: true, ci)
                    }
                }
            } label: {
                Label(
                    NSLocalizedString("Select", comment: "chat item action"),
                    systemImage: "checkmark.circle"
                )
            }
        }

        private func viewInfoButton(_ ci: ChatItem) -> Button<some View> {
            Button {
                Task {
                    do {
                        let cInfo = chat.chatInfo
                        let ciInfo = try await apiGetChatItemInfo(type: cInfo.chatType, id: cInfo.apiId, itemId: ci.id)
                        await MainActor.run {
                            chatItemInfo = ciInfo
                        }
                        if case let .group(gInfo) = chat.chatInfo {
                            await m.loadGroupMembers(gInfo)
                        }
                    } catch let error {
                        logger.error("apiGetChatItemInfo error: \(responseError(error))")
                    }
                    await MainActor.run { showChatItemInfoSheet = true }
                }
            } label: {
                Label(
                    NSLocalizedString("Info", comment: "chat item action"),
                    systemImage: "info.circle"
                )
            }
        }

        private func cancelFileButton(_ fileId: Int64, _ cancelAction: CancelAction) -> Button<some View> {
            Button {
                AlertManager.shared.showAlert(Alert(
                    title: Text(cancelAction.alert.title),
                    message: Text(cancelAction.alert.message),
                    primaryButton: .destructive(Text(cancelAction.alert.confirm)) {
                        Task {
                            if let user = m.currentUser {
                                await cancelFile(user: user, fileId: fileId)
                            }
                        }
                    },
                    secondaryButton: .cancel()
                ))
            } label: {
                Label(
                    cancelAction.uiAction,
                    systemImage: "xmark"
                )
            }
        }

        private func hideButton() -> Button<some View> {
            Button {
                withConditionalAnimation {
                    revealedChatItem = nil
                }
            } label: {
                Label(
                    NSLocalizedString("Hide", comment: "chat item action"),
                    systemImage: "eye.slash"
                )
            }
        }

        private func deleteButton(_ ci: ChatItem) -> Button<some View> {
            Button(role: .destructive) {
                if !revealed,
                   let currIndex = m.getChatItemIndex(ci),
                   let ciCategory = ci.mergeCategory {
                    let (prevHidden, _) = m.getPrevShownChatItem(currIndex, ciCategory)
                    if let range = itemsRange(currIndex, prevHidden) {
                        var itemIds: [Int64] = []
                        for i in range {
                            itemIds.append(ItemsModel.shared.reversedChatItems[i].id)
                        }
                        showDeleteMessages = true
                        deletingItems = itemIds
                    } else {
                        showDeleteMessage = true
                        deletingItem = ci
                    }
                } else {
                    showDeleteMessage = true
                    deletingItem = ci
                }
            } label: {
                Label(
                    NSLocalizedString("Delete", comment: "chat item action"),
                    systemImage: "trash"
                )
            }
        }

        private func itemsRange(_ currIndex: Int?, _ prevHidden: Int?) -> ClosedRange<Int>? {
            if let currIndex = currIndex,
               let prevHidden = prevHidden,
               prevHidden > currIndex {
                currIndex...prevHidden
            } else {
                nil
            }
        }

        private func moderateButton(_ ci: ChatItem, _ groupInfo: GroupInfo) -> Button<some View> {
            Button(role: .destructive) {
                AlertManager.shared.showAlert(Alert(
                    title: Text("Delete member message?"),
                    message: Text(
                                groupInfo.fullGroupPreferences.fullDelete.on
                                ? "The message will be deleted for all members."
                                : "The message will be marked as moderated for all members."
                            ),
                    primaryButton: .destructive(Text("Delete")) {
                        deletingItem = ci
                        deleteMessage(.cidmBroadcast, moderate: true)
                    },
                    secondaryButton: .cancel()
                ))
            } label: {
                Label(
                    NSLocalizedString("Moderate", comment: "chat item action"),
                    systemImage: "flag"
                )
            }
        }

        private func revealButton(_ ci: ChatItem) -> Button<some View> {
            Button {
                withConditionalAnimation {
                    revealedChatItem = ci
                }
            } label: {
                Label(
                    NSLocalizedString("Reveal", comment: "chat item action"),
                    systemImage: "eye"
                )
            }
        }

        private func expandButton() -> Button<some View> {
            Button {
                withConditionalAnimation {
                    revealedChatItem = chatItem
                }
            } label: {
                Label(
                    NSLocalizedString("Expand", comment: "chat item action"),
                    systemImage: "arrow.up.and.line.horizontal.and.arrow.down"
                )
            }
        }

        private func shrinkButton() -> Button<some View> {
            Button {
                withConditionalAnimation {
                    revealedChatItem = nil
                }
            } label: {
                Label (
                    NSLocalizedString("Hide", comment: "chat item action"),
                    systemImage: "arrow.down.and.line.horizontal.and.arrow.up"
                )
            }
        }

        var deleteMessagesTitle: LocalizedStringKey {
            let n = deletingItems.count
            return n == 1 ? "Delete message?" : "Delete \(n) messages?"
        }

        private func selectUnselectChatItem(select: Bool, _ ci: ChatItem) {
            selectedChatItems = selectedChatItems ?? []
            var itemIds: [Int64] = []
            if !revealed,
               let currIndex = m.getChatItemIndex(ci),
               let ciCategory = ci.mergeCategory {
                let (prevHidden, _) = m.getPrevShownChatItem(currIndex, ciCategory)
                if let range = itemsRange(currIndex, prevHidden) {
                    for i in range {
                        itemIds.append(ItemsModel.shared.reversedChatItems[i].id)
                    }
                } else {
                    itemIds.append(ci.id)
                }
            } else {
                itemIds.append(ci.id)
            }
            if select {
                if let sel = selectedChatItems {
                    selectedChatItems = sel.union(itemIds)
                }
            } else {
                itemIds.forEach { selectedChatItems?.remove($0) }
            }
        }

        private func deleteMessage(_ mode: CIDeleteMode, moderate: Bool) {
            logger.debug("ChatView deleteMessage")
            Task {
                logger.debug("ChatView deleteMessage: in Task")
                do {
                    if let di = deletingItem {
                        let r = if case .cidmBroadcast = mode,
                           moderate,
                           let (groupInfo, _) = di.memberToModerate(chat.chatInfo) {
                            try await apiDeleteMemberChatItems(
                                groupId: groupInfo.apiId,
                                itemIds: [di.id]
                            )
                        } else {
                            try await apiDeleteChatItems(
                                type: chat.chatInfo.chatType,
                                id: chat.chatInfo.apiId,
                                itemIds: [di.id],
                                mode: mode
                            )
                        }
                        if let itemDeletion = r.first {
                            await MainActor.run {
                                deletingItem = nil
                                if let toItem = itemDeletion.toChatItem {
                                    _ = m.upsertChatItem(chat.chatInfo, toItem.chatItem)
                                } else {
                                    m.removeChatItem(chat.chatInfo, itemDeletion.deletedChatItem.chatItem)
                                }
                            }
                        }
                    }
                } catch {
                    logger.error("ChatView.deleteMessage error: \(error.localizedDescription)")
                }
            }
        }

        private struct SelectedChatItem: View {
            @EnvironmentObject var theme: AppTheme
            var ciId: Int64
            @Binding var selectedChatItems: Set<Int64>?
            @State var checked: Bool = false
            var body: some View {
                Image(systemName: checked ? "checkmark.circle.fill" : "circle")
                    .resizable()
                    .foregroundColor(checked ? theme.colors.primary : Color(uiColor: .tertiaryLabel))
                    .frame(width: 24, height: 24)
                    .onAppear {
                        checked = selectedChatItems?.contains(ciId) == true
                    }
                    .onChange(of: selectedChatItems) { selected in
                        checked = selected?.contains(ciId) == true
                    }
            }
        }
    }
}

private func broadcastDeleteButtonText(_ chat: Chat) -> LocalizedStringKey {
    chat.chatInfo.featureEnabled(.fullDelete) ? "Delete for everyone" : "Mark deleted for everyone"
}

private func deleteMessages(_ chat: Chat, _ deletingItems: [Int64], _ mode: CIDeleteMode = .cidmInternal, moderate: Bool, _ onSuccess: @escaping () async -> Void = {}) {
    let itemIds = deletingItems
    if itemIds.count > 0 {
        let chatInfo = chat.chatInfo
        Task {
            do {
                let deletedItems = if case .cidmBroadcast = mode,
                    moderate,
                    case .group = chat.chatInfo {
                    try await apiDeleteMemberChatItems(
                        groupId: chatInfo.apiId,
                        itemIds: itemIds
                    )
                } else {
                    try await apiDeleteChatItems(
                        type: chatInfo.chatType,
                        id: chatInfo.apiId,
                        itemIds: itemIds,
                        mode: mode
                    )
                }

                await MainActor.run {
                    for di in deletedItems {
                        if let toItem = di.toChatItem {
                            _ = ChatModel.shared.upsertChatItem(chat.chatInfo, toItem.chatItem)
                        } else {
                            ChatModel.shared.removeChatItem(chatInfo, di.deletedChatItem.chatItem)
                        }
                    }
                }
                await onSuccess()
            } catch {
                logger.error("ChatView.deleteMessages error: \(error.localizedDescription)")
            }
        }
    }
}

private func buildTheme() -> AppTheme {
    if let cId = ChatModel.shared.chatId, let chat = ChatModel.shared.getChat(cId) {
        let perChatTheme = if case let .direct(contact) = chat.chatInfo {
            contact.uiThemes?.preferredMode(!AppTheme.shared.colors.isLight)
        } else if case let .group(groupInfo) = chat.chatInfo {
            groupInfo.uiThemes?.preferredMode(!AppTheme.shared.colors.isLight)
        } else {
            nil as ThemeModeOverride?
        }
        let overrides = if perChatTheme != nil {
            ThemeManager.currentColors(nil, perChatTheme, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
        } else {
            nil as ThemeManager.ActiveTheme?
        }
        let theme = overrides ?? CurrentColors
        return AppTheme(name: theme.name, base: theme.base, colors: theme.colors, appColors: theme.appColors, wallpaper: theme.wallpaper)
    } else {
        return AppTheme.shared
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
        ItemsModel.shared.reversedChatItems = [
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
