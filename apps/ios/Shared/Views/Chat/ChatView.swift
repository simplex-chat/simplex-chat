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
    @StateObject private var connectProgressManager = ConnectProgressManager.shared
    @State var revealedItems: Set<Int64> = Set()
    @State var theme: AppTheme = buildTheme()
    @Environment(\.dismiss) var dismiss
    @Environment(\.colorScheme) var colorScheme
    @Environment(\.presentationMode) var presentationMode
    @Environment(\.scenePhase) var scenePhase
    @State @ObservedObject var chat: Chat
    @ObservedObject var im: ItemsModel
    @State var mergedItems: BoxedValue<MergedItems>
    @State var floatingButtonModel: FloatingButtonModel
    @Binding var scrollToItemId: ChatItem.ID?
    @State private var showChatInfoSheet: Bool = false
    @State private var showAddMembersSheet: Bool = false
    @State private var composeState = ComposeState()
    @State private var selectedRange = NSRange()
    @State private var keyboardVisible = false
    @State private var keyboardHiddenDate = Date.now
    @State private var connectionStats: ConnectionStats?
    @State private var customUserProfile: Profile?
    @State private var connectionCode: String?
    @State private var loadingMoreItems = false
    @State private var loadingTopItems = false
    @State private var requestedTopScroll = false
    @State private var loadingBottomItems = false
    @State private var requestedBottomScroll = false
    @State private var showSearch = false
    @State private var searchText: String = ""
    @FocusState private var searchFocussed
    // opening GroupMemberInfoView on member icon
    @State private var selectedMember: GMember? = nil
    // opening GroupLinkView on link button (incognito)
    @State private var showGroupLinkSheet: Bool = false
    @State private var groupLink: GroupLink?
    @State private var groupLinkMemberRole: GroupMemberRole = .member
    @State private var forwardedChatItems: [ChatItem] = []
    @State private var selectedChatItems: Set<Int64>? = nil
    @State private var showDeleteSelectedMessages: Bool = false
    @State private var showArchiveSelectedReports: Bool = false
    @State private var allowToDeleteSelectedMessagesForAll: Bool = false
    @State private var allowLoadMoreItems: Bool = false
    @State private var ignoreLoadingRequests: Int64? = nil
    @State private var animatedScrollingInProgress: Bool = false
    @State private var showUserSupportChatSheet = false
    @State private var showCommandsMenu = false
    @State private var supportChatMemberInfoLinkActive = false

    @State private var scrollView: EndlessScrollView<MergedItem> = EndlessScrollView(frame: .zero)

    @AppStorage(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial

    let userSupportScopeInfo: GroupChatScopeInfo = .memberSupport(groupMember_: nil)

    var body: some View {
        if #available(iOS 16.0, *) {
            viewBody
                .scrollDismissesKeyboard(.immediately)
                .toolbarBackground(.hidden, for: .navigationBar)
        } else {
            viewBody
        }
    }

    private var viewBody: some View {
        let cInfo = chat.chatInfo
        let memberSupportChat: (groupInfo: GroupInfo, member: GroupMember?)? =
                if case let .group(groupInfo, .memberSupport(member)) = cInfo {
                    (groupInfo, member)
                } else {
                    nil
                }
        let userMemberKnockingChat = memberSupportChat?.groupInfo.membership.memberPending == true
        return ZStack {
            let wallpaperImage = theme.wallpaper.type.image
            let wallpaperType = theme.wallpaper.type
            let backgroundColor = theme.wallpaper.background ?? wallpaperType.defaultBackgroundColor(theme.base, theme.colors.background)
            let tintColor = theme.wallpaper.tint ?? wallpaperType.defaultTintColor(theme.base)
            Color.clear.ignoresSafeArea(.all)
                .if(wallpaperImage != nil && im.secondaryIMFilter == nil) { view in
                    view.modifier(
                        ChatViewBackground(image: wallpaperImage!, imageType: wallpaperType, background: backgroundColor, tint: tintColor)
                    )
            }
            VStack(spacing: 0) {
                ZStack(alignment: .bottomTrailing) {
                    if userMemberKnockingChat {
                        ZStack(alignment: .top) {
                            chatItemsList()
                            userMemberKnockingTitleBar()
                        }
                    } else {
                        chatItemsList()
                    }
                    if let groupInfo = chat.chatInfo.groupInfo, !composeState.message.isEmpty {
                        GroupMentionsView(im: im, groupInfo: groupInfo, composeState: $composeState, selectedRange: $selectedRange, keyboardVisible: $keyboardVisible)
                    }
                    if !chat.chatInfo.menuCommands.isEmpty {
                        CommandsMenuView(chat: chat, composeState: $composeState, selectedRange: $selectedRange, showCommandsMenu: $showCommandsMenu)
                    }
                    FloatingButtons(im: im, theme: theme, scrollView: scrollView, chat: chat, loadingMoreItems: $loadingMoreItems, loadingTopItems: $loadingTopItems, requestedTopScroll: $requestedTopScroll, loadingBottomItems: $loadingBottomItems, requestedBottomScroll: $requestedBottomScroll, animatedScrollingInProgress: $animatedScrollingInProgress, listState: scrollView.listState, model: floatingButtonModel, reloadItems: {
                            mergedItems.boxedValue = MergedItems.create(im, revealedItems)
                            scrollView.updateItems(mergedItems.boxedValue.items)
                        }
                    )
                }
                if let connectInProgressText = connectProgressManager.showConnectProgress {
                    connectInProgressView(connectInProgressText)
                }
                if let connectingText {
                    Text(connectingText)
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                        .padding(.top)
                }
                if selectedChatItems == nil {
                    let reason = chat.chatInfo.userCantSendReason
                    let composeEnabled = (
                        chat.chatInfo.sendMsgEnabled ||
                        (chat.chatInfo.groupInfo?.nextConnectPrepared ?? false) || // allow to join prepared group without message
                        (chat.chatInfo.contact?.nextAcceptContactRequest ?? false) // allow to accept or reject contact request
                    )
                    ComposeView(
                        chat: chat,
                        im: im,
                        composeState: $composeState,
                        showCommandsMenu: $showCommandsMenu,
                        keyboardVisible: $keyboardVisible,
                        keyboardHiddenDate: $keyboardHiddenDate,
                        selectedRange: $selectedRange,
                        disabledText: reason?.composeLabel
                    )
                    .disabled(!composeEnabled)
                    .if(!composeEnabled) { v in
                        v.disabled(true).onTapGesture {
                            AlertManager.shared.showAlertMsg(
                                title: "You can't send messages!",
                                message: reason?.alertMessage
                            )
                        }
                    }
                } else {
                    SelectedItemsBottomToolbar(
                        im: im,
                        selectedChatItems: $selectedChatItems,
                        chatInfo: chat.chatInfo,
                        deleteItems: { forAll in
                            allowToDeleteSelectedMessagesForAll = forAll
                            showDeleteSelectedMessages = true
                        },
                        archiveItems: {
                            showArchiveSelectedReports = true
                        },
                        moderateItems: {
                            if case let .group(groupInfo, _) = chat.chatInfo {
                                showModerateSelectedMessagesAlert(groupInfo)
                            }
                        },
                        forwardItems: forwardSelectedMessages
                    )
                }
            }
            if im.showLoadingProgress == chat.id {
                ProgressView().scaleEffect(2)
            }
            if case let .group(groupInfo, _) = chat.chatInfo,
               case let .groupChatScopeContext(groupScopeInfo) = im.secondaryIMFilter,
               case let .memberSupport(groupMember_) = groupScopeInfo,
               let groupMember = groupMember_ {
                NavigationLink(isActive: $supportChatMemberInfoLinkActive) {
                    GroupMemberInfoView(
                        groupInfo: groupInfo,
                        chat: chat,
                        groupMember: GMember(groupMember),
                        scrollToItemId: $scrollToItemId,
                        openedFromSupportChat: true
                    )
                    .navigationBarHidden(false)
                    .modifier(BackButton(disabled: Binding.constant(false)) {
                        supportChatMemberInfoLinkActive = false
                    })
                } label: {
                    EmptyView()
                }
                .frame(width: 1, height: 1)
                .hidden()
            }
        }
        .safeAreaInset(edge: .top) {
            VStack(spacing: .zero) {
                if showSearch { searchToolbar() }
                Divider()
            }
            .background(ToolbarMaterial.material(toolbarMaterial))
        }
        .navigationTitle(
            memberSupportChat == nil
            ? cInfo.chatViewName
            : memberSupportChat?.member?.chatViewName ?? NSLocalizedString("Chat with admins", comment: "chat toolbar")
        )
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
        .confirmationDialog(selectedChatItems?.count == 1 ? "Archive report?" : "Archive \((selectedChatItems?.count ?? 0)) reports?", isPresented: $showArchiveSelectedReports, titleVisibility: .visible) {
            Button("For me", role: .destructive) {
                if let selected = selectedChatItems {
                    archiveReports(chat, selected.sorted(), false, deletedSelectedMessages)
                }
            }
            if case let ChatInfo.group(groupInfo, _) = chat.chatInfo, groupInfo.membership.memberActive {
                Button("For all moderators", role: .destructive) {
                    if let selected = selectedChatItems {
                        archiveReports(chat, selected.sorted(), true, deletedSelectedMessages)
                    }
                }
            }
        }
        .appSheet(item: $selectedMember, onDismiss: {
            chatModel.secondaryIM = nil
        }) { member in
            if case let .group(groupInfo, _) = chat.chatInfo {
                GroupMemberInfoView(
                    groupInfo: groupInfo,
                    chat: chat,
                    groupMember: member,
                    scrollToItemId: $scrollToItemId,
                    navigation: true
                )
            }
        }
        // it should be presented on top level in order to prevent a bug in SwiftUI on iOS 16 related to .focused() modifier in AddGroupMembersView's search field
        .appSheet(isPresented: $showAddMembersSheet) {
            if case let .group(groupInfo, _) = cInfo {
                AddGroupMembersView(chat: chat, groupInfo: groupInfo)
            }
        }
        .sheet(isPresented: Binding(
            get: { !forwardedChatItems.isEmpty },
            set: { isPresented in
                if !isPresented {
                    forwardedChatItems = []
                    selectedChatItems = nil
                }
            }
        )) {
            if #available(iOS 16.0, *) {
                ChatItemForwardingView(chatItems: forwardedChatItems, fromChatInfo: chat.chatInfo, composeState: $composeState)
                    .presentationDetents([.fraction(0.8)])
            } else {
                ChatItemForwardingView(chatItems: forwardedChatItems, fromChatInfo: chat.chatInfo, composeState: $composeState)
            }
        }
        .appSheet(
            isPresented: $showUserSupportChatSheet,
            onDismiss: {
                if chat.chatInfo.groupInfo?.membership.memberPending ?? false {
                    chatModel.chatId = nil
                }
            }
        ) {
            if let groupInfo = cInfo.groupInfo {
                SecondaryChatView(
                    chat: Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: userSupportScopeInfo), chatItems: [], chatStats: ChatStats()),
                    scrollToItemId: $scrollToItemId
                )
            }
        }
        .onAppear {
            ConnectProgressManager.shared.cancelConnectProgress()
            scrollView.listState.onUpdateListener = onChatItemsUpdated
            selectedChatItems = nil
            revealedItems = Set()
            initChatView()
            if im.isLoading {
                Task {
                    try? await Task.sleep(nanoseconds: 500_000000)
                    await MainActor.run {
                        if im.isLoading {
                            im.showLoadingProgress = chat.id
                        }
                    }
                }
            }
            // if this is the main chat of the group with the pending member (knocking)
            if case let .group(groupInfo, nil) = chat.chatInfo,
               groupInfo.membership.memberPending {
                ItemsModel.loadSecondaryChat(chat.id, chatFilter: .groupChatScopeContext(groupScopeInfo: userSupportScopeInfo)) {
                    showUserSupportChatSheet = true
                    chatModel.secondaryPendingInviteeChatOpened = true
                }
            }
        }
        .onChange(of: chatModel.secondaryPendingInviteeChatOpened) { secondaryChatOpened in
            if secondaryChatOpened {
                ItemsModel.loadSecondaryChat(chat.id, chatFilter: .groupChatScopeContext(groupScopeInfo: userSupportScopeInfo)) {
                    showUserSupportChatSheet = true
                }
            }
        }
        .onChange(of: chatModel.chatId) { cId in
            ConnectProgressManager.shared.cancelConnectProgress()
            showChatInfoSheet = false
            selectedChatItems = nil
            revealedItems = Set()
            stopAudioPlayer()
            if let cId {
                if let c = chatModel.getChat(cId) {
                    chat = c
                }
                scrollView.listState.onUpdateListener = onChatItemsUpdated
                initChatView()
                theme = buildTheme()
                closeSearch()
                mergedItems.boxedValue = MergedItems.create(im, revealedItems)
                scrollView.updateItems(mergedItems.boxedValue.items)

                if let openAround = chatModel.openAroundItemId, let index = mergedItems.boxedValue.indexInParentItems[openAround] {
                    scrollView.scrollToItem(index)
                } else if let unreadIndex = mergedItems.boxedValue.items.lastIndex(where: { $0.hasUnread() }) {
                    scrollView.scrollToItem(unreadIndex)
                } else {
                    scrollView.scrollToBottom()
                }
                if chatModel.openAroundItemId != nil {
                    chatModel.openAroundItemId = nil
                }
            } else {
                dismiss()
            }
        }
        .onChange(of: chatModel.secondaryPendingInviteeChatOpened) { opened in
            if im.secondaryIMFilter != nil && !opened {
                Task {
                    try? await Task.sleep(nanoseconds: 650_000000)
                    dismiss()
                }
            }
        }
        .onChange(of: chatModel.openAroundItemId) { openAround in
            if let openAround {
                closeSearch()
                mergedItems.boxedValue = MergedItems.create(im, revealedItems)
                scrollView.updateItems(mergedItems.boxedValue.items)
                chatModel.openAroundItemId = nil

                if let index = mergedItems.boxedValue.indexInParentItems[openAround] {
                    scrollView.scrollToItem(index)
                }

                // this may already being loading because of changed chat id (see .onChange(of: chat.id)
                if !loadingBottomItems {
                    allowLoadMoreItems = false
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                        allowLoadMoreItems = true
                    }
                }
            }
        }
        .onDisappear {
            ConnectProgressManager.shared.cancelConnectProgress()
            VideoPlayerView.players.removeAll()
            stopAudioPlayer()
            if chatModel.chatId == cInfo.id && !presentationMode.wrappedValue.isPresented {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) {
                    if chatModel.chatId == nil {
                        im.reversedChatItems = []
                        im.chatState.clear()
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
                if im.secondaryIMFilter == nil {
                    primaryPrincipalToolbarContent()
                } else if !userMemberKnockingChat { // no toolbar while knocking chat, it's unstable on sheet
                    secondaryPrincipalToolbarContent()
                }
            }
            ToolbarItem(placement: .navigationBarTrailing) {
                if im.secondaryIMFilter == nil {
                    primaryTrailingToolbarContent()
                } else if !userMemberKnockingChat {
                    secondaryTrailingToolbarContent()
                }
            }
        }
        .if(im.secondaryIMFilter == nil) { v in
            v.onChange(of: scrollToItemId) { itemId in
                if let itemId = itemId {
                    dismissAllSheets(animated: false) {
                        scrollToItem(itemId)
                        scrollToItemId = nil
                    }
                }
            }
        }
    }

    private func connectInProgressView(_ s: String) -> some View {
        VStack(spacing: 0) {
            Divider()

            HStack(spacing: 12) {
                ProgressView()
                Text(s)

                Spacer()

                Button {
                    ConnectProgressManager.shared.cancelConnectProgress()
                } label: {
                    Image(systemName: "multiply")
                }
                .tint(theme.colors.primary)
            }
            .padding(12)
            .frame(minHeight: 54)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(ToolbarMaterial.material(toolbarMaterial))
        }
    }

    @inline(__always)
    @ViewBuilder private func primaryPrincipalToolbarContent() -> some View {
        let cInfo = chat.chatInfo
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
            .appSheet(isPresented: $showChatInfoSheet, onDismiss: { theme = buildTheme() }) {
                ChatInfoView(
                    chat: chat,
                    contact: contact,
                    localAlias: chat.chatInfo.localAlias,
                    featuresAllowed: contactUserPrefsToFeaturesAllowed(contact.mergedPreferences),
                    currentFeaturesAllowed: contactUserPrefsToFeaturesAllowed(contact.mergedPreferences),
                    onSearch: { focusSearch() }
                )
            }
        } else if case let .group(groupInfo, _) = cInfo {
            Button {
                Task { await chatModel.loadGroupMembers(groupInfo) { showChatInfoSheet = true } }
            } label: {
                ChatInfoToolbar(chat: chat)
                    .tint(theme.colors.primary)
            }
            .appSheet(isPresented: $showChatInfoSheet, onDismiss: {
                chatModel.secondaryIM = nil
                theme = buildTheme()
            }) {
                GroupChatInfoView(
                    chat: chat,
                    groupInfo: Binding(
                        get: { groupInfo },
                        set: { gInfo in
                            chat.chatInfo = .group(groupInfo: gInfo, groupChatScope: nil)
                            chat.created = Date.now
                        }
                    ),
                    scrollToItemId: $scrollToItemId,
                    onSearch: { focusSearch() },
                    localAlias: groupInfo.localAlias
                )
            }
        } else if case .local = cInfo {
            ChatInfoToolbar(chat: chat)
        }
    }

    @inline(__always)
    @ViewBuilder private func primaryTrailingToolbarContent() -> some View {
        let cInfo = chat.chatInfo
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
            case let .group(groupInfo, _):
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

    @inline(__always)
    @ViewBuilder private func secondaryPrincipalToolbarContent() -> some View {
        if selectedChatItems != nil {
            SelectedItemsTopToolbar(selectedChatItems: $selectedChatItems)
        } else {
            switch im.secondaryIMFilter {
            case let .groupChatScopeContext(groupScopeInfo):
                switch groupScopeInfo {
                case let .memberSupport(groupMember_):
                    if let groupMember = groupMember_ {
                        Button {
                            supportChatMemberInfoLinkActive = true
                        } label: {
                            MemberSupportChatToolbar(groupMember: groupMember)
                        }
                    } else {
                        textChatToolbar("Chat with admins")
                    }
                case .reports:
                    textChatToolbar("Member reports")
                }
            case let .msgContentTagContext(contentTag):
                switch contentTag {
                case .report:
                    textChatToolbar("Member reports")
                default:
                    EmptyView()
                }
            case .none:
                EmptyView()
            }
        }
    }

    @inline(__always)
    @ViewBuilder private func secondaryTrailingToolbarContent() -> some View {
        if selectedChatItems != nil {
            Button {
                withAnimation {
                    selectedChatItems = nil
                }
            } label: {
                Text("Cancel")
            }
        } else {
            searchButton()
        }
    }

    @inline(__always)
    private func userMemberKnockingTitleBar() -> some View {
         VStack(spacing: 0) {
             Text("Chat with admins")
                 .font(.headline)
                 .foregroundColor(theme.colors.onBackground)
                 .padding(.top, 8)
                 .padding(.bottom, 14)
                 .frame(maxWidth: .infinity)
                 .background(ToolbarMaterial.material(toolbarMaterial))
             Divider()
         }
    }

    func textChatToolbar(_ text: LocalizedStringKey) -> some View {
        Text(text)
        .font(.headline)
        .lineLimit(1)
        .foregroundColor(theme.colors.onBackground)
        .frame(width: 220)
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
        floatingButtonModel.updateOnListChange(scrollView.listState)
    }

    private func scrollToItem(_ itemId: ChatItem.ID) {
        Task {
            do {
                var index = mergedItems.boxedValue.indexInParentItems[itemId]
                if index == nil {
                    let pagination = ChatPagination.around(chatItemId: itemId, count: ChatPagination.PRELOAD_COUNT * 2)
                    let oldSize = im.reversedChatItems.count
                    let triedToLoad = await loadChatItems(chat, pagination)
                    if !triedToLoad {
                        return
                    }
                    var repeatsLeft = 50
                    while oldSize == im.reversedChatItems.count && repeatsLeft > 0 {
                        try await Task.sleep(nanoseconds: 20_000000)
                        repeatsLeft -= 1
                    }
                    index = mergedItems.boxedValue.indexInParentItems[itemId]
                }
                if let index {
                    closeKeyboardAndRun {
                        Task {
                            await MainActor.run { animatedScrollingInProgress = true }
                            await scrollView.scrollToItemAnimated(min(im.reversedChatItems.count - 1, index))
                            await MainActor.run { animatedScrollingInProgress = false }
                        }
                    }
                }
            } catch {
                logger.error("Error scrolling to item: \(error)")
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
                closeSearch()
                searchTextChanged("")
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
        return GeometryReader { g in
            //let _ = logger.debug("Reloading chatItemsList with number of itmes: \(im.reversedChatItems.count)")
            ScrollRepresentable(scrollView: scrollView) { (index: Int, mergedItem: MergedItem) in
                let ci = switch mergedItem {
                case let .single(item, _, _): item.item
                case let .grouped(items, _, _, _, _, _, _, _): items.boxedValue.last!.item
                }
                return Group {
                    if case .chatBanner = ci.content {
                        VStack {
                            ChatBannerView(chat: $chat)
                                .padding(.bottom, 90)
                                .padding(.top, 8)

                            let listItem = mergedItem.newest()
                            if let prevItem = listItem.prevItem {
                                DateSeparator(date: prevItem.meta.itemTs).padding(8)
                            }
                        }
                    } else {
                        let voiceNoFrame = voiceWithoutFrame(ci)
                        let maxWidth = cInfo.chatType == .group
                        ? voiceNoFrame
                        ? (g.size.width - 28) - 42
                        : (g.size.width - 28) * 0.84 - 42
                        : voiceNoFrame
                        ? (g.size.width - 32)
                        : (g.size.width - 32) * 0.84
                        ChatItemWithMenu(
                            im: im,
                            chat: $chat,
                            index: index,
                            isLastItem: index == mergedItems.boxedValue.items.count - 1,
                            chatItem: ci,
                            scrollToItem: scrollToItem,
                            scrollToItemId: $scrollToItemId,
                            merged: mergedItem,
                            maxWidth: maxWidth,
                            composeState: $composeState,
                            selectedMember: $selectedMember,
                            showChatInfoSheet: $showChatInfoSheet,
                            revealedItems: $revealedItems,
                            selectedChatItems: $selectedChatItems,
                            forwardedChatItems: $forwardedChatItems,
                            searchText: $searchText,
                            closeKeyboardAndRun: closeKeyboardAndRun
                        )
                    }
                }
                // crashes on Cell size calculation without this line
                .environmentObject(ChatModel.shared)
                .environmentObject(theme) // crashes without this line when scrolling to the first unread in EndlessScrollVIew
                .id(ci.id) // Required to trigger `onAppear` on iOS15
            }
            .onAppear {
                if !im.isLoading {
                    updateWithInitiallyLoadedItems()
                }
            }
            .onChange(of: im.isLoading) { loading in
                if !loading {
                    updateWithInitiallyLoadedItems()
                }
            }
            .onChange(of: im.reversedChatItems) { items in
                mergedItems.boxedValue = MergedItems.create(im, revealedItems)
                scrollView.updateItems(mergedItems.boxedValue.items)
                if im.itemAdded {
                    im.itemAdded = false
                    if scrollView.listState.firstVisibleItemIndex < 2 {
                        scrollView.scrollToBottomAnimated()
                    } else {
                        scrollView.scroll(by: 34)
                    }
                }
            }
            .onChange(of: revealedItems) { revealed in
                mergedItems.boxedValue = MergedItems.create(im, revealed)
                scrollView.updateItems(mergedItems.boxedValue.items)
            }
            .onChange(of: chat.id) { _ in
                allowLoadMoreItems = false
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                    allowLoadMoreItems = true
                }
            }
            .padding(.vertical, -100)
            .onTapGesture { hideKeyboard() }
            .onChange(of: searchText) { s in
                if showSearch {
                    searchTextChanged(s)
                }
            }
        }
    }

    struct ChatBannerView: View {
        @EnvironmentObject var theme: AppTheme
        @AppStorage(DEFAULT_CHAT_ITEM_ROUNDNESS) private var roundness = defaultChatItemRoundness
        @Binding @ObservedObject var chat: Chat
        @State private var showSecrets: Set<Int> = []

        var body: some View {
            let v = VStack(spacing: 8) {
                ChatInfoImage(chat: chat, size: alertProfileImageSize)

                Text(chat.chatInfo.displayName)
                    .font(.title3)
                    .multilineTextAlignment(.center)
                    .lineLimit(2)
                    .fixedSize(horizontal: false, vertical: true)
                    .frame(maxWidth: 240)

                let fullName = chat.chatInfo.fullName.trimmingCharacters(in: .whitespacesAndNewlines)
                if fullName != "" && fullName != chat.chatInfo.displayName && fullName != chat.chatInfo.displayName.trimmingCharacters(in: .whitespacesAndNewlines) {
                    Text(chat.chatInfo.fullName)
                        .font(.subheadline)
                        .multilineTextAlignment(.center)
                        .lineLimit(3)
                        .fixedSize(horizontal: false, vertical: true)
                        .frame(maxWidth: 260)
                }

                if let shortDescr = chat.chatInfo.shortDescr {
                    let r = markdownText(shortDescr, textStyle: .subheadline, showSecrets: showSecrets, backgroundColor: theme.colors.background)
                    msgTextResultView(r, Text(AttributedString(r.string)), showSecrets: $showSecrets, centered: true, smallFont: true)
                        .multilineTextAlignment(.center)
                        .lineLimit(4)
                        .fixedSize(horizontal: false, vertical: true)
                        .padding(.horizontal)
                }

                if let chatContext {
                    Text(chatContext)
                        .font(.callout)
                        .foregroundColor(theme.colors.secondary)
                        .padding(.top, 8)
                }
            }
            .frame(maxWidth: .infinity)
            .padding()
            .background(theme.appColors.receivedMessage)
            .clipShape(RoundedRectangle(cornerRadius: msgRectMaxRadius * roundness))
            if let (label, connLink) = chatAddress() {
                v.contextMenu {
                    Button {
                        let shareItems: [Any] = [connLink]
                        showShareSheet(items: shareItems)
                    } label: {
                        Label(label, systemImage: "square.and.arrow.up")
                    }
                }
                .padding(.horizontal)
            } else {
                v.padding(.horizontal)
            }

        }

        func chatAddress() -> (label: LocalizedStringKey, connLink: String)? {
            switch chat.chatInfo {
            case let .direct(contact):
                if !contact.nextConnectPrepared && !contact.nextAcceptContactRequest {
                    let connLink: String? = if let pct = contact.preparedContact, case .con = pct.uiConnLinkType {
                        pct.connLinkToConnect.simplexChatUri()
                    } else {
                        contact.profile.contactLink
                    }
                    if let connLink {
                        return ("SimpleX address", connLink)
                    }
                }
            case let .group(groupInfo, _):
                if !groupInfo.nextConnectPrepared {
                    if let pg = groupInfo.preparedGroup {
                        let connLink = pg.connLinkToConnect.simplexChatUri()
                        switch groupInfo.businessChat?.chatType {
                        case .none: return ("Group link", connLink)
                        case .business: return ("Business address", connLink)
                        default: ()
                        }
                    }
                }
            default: ()
            }
            return nil
        }

        var chatContext: LocalizedStringKey? {
            switch chat.chatInfo {
            case let .direct(contact):
                if contact.nextConnectPrepared, let linkType = contact.preparedContact?.uiConnLinkType {
                    switch linkType {
                    case .inv:
                        "Tap Connect to chat"
                    case .con:
                        contact.isBot ? "Tap Connect to use bot" : "Tap Connect to send request"
                    }
                } else if contact.nextAcceptContactRequest {
                    "Accept contact request"
                } else if case .bot = contact.profile.peerType {
                    "Bot"
                } else {
                    "Your contact"
                }
            case let .group(groupInfo, _):
                switch groupInfo.businessChat?.chatType {
                case .none:
                    if groupInfo.nextConnectPrepared {
                        "Tap Join group"
                    } else {
                        switch (groupInfo.membership.memberStatus) {
                        case .memInvited: "Join group"
                        case .memCreator: "Your group"
                        default: "Group"
                        }
                    }
                case .business:
                    if groupInfo.nextConnectPrepared {
                        "Tap Connect to chat"
                    } else {
                        "Business connection"
                    }
                case .customer:
                    "Your business contact"
                }
            default: nil
            }
        }
    }

    private var connectingText: LocalizedStringKey? {
        switch (chat.chatInfo) {
        case let .direct(contact):
            if !contact.sndReady && contact.active && !contact.sendMsgToConnect && !contact.nextAcceptContactRequest {
                (contact.preparedContact?.uiConnLinkType == .con && !contact.isBot) || contact.contactGroupMemberId != nil
                ? "contact should accept…"
                : "connecting…"
            } else {
                nil
            }
        case let .group(groupInfo, _):
            switch (groupInfo.membership.memberStatus) {
            case .memUnknown: groupInfo.preparedGroup?.connLinkStartedConnection == true ? "connecting…" : nil
            case .memAccepted: "connecting…"
            default: nil
            }
        default: nil
        }
    }

    private func updateWithInitiallyLoadedItems() {
        if mergedItems.boxedValue.items.isEmpty {
            mergedItems.boxedValue = MergedItems.create(im, revealedItems)
        }
        let unreadIndex = mergedItems.boxedValue.items.lastIndex(where: { $0.hasUnread() })
        let unreadItemId: Int64? = if let unreadIndex { mergedItems.boxedValue.items[unreadIndex].newest().item.id } else { nil }
        // this helps to speed up initial process of setting scroll position and reduce time needed
        // to layout items on screen
        if let unreadIndex, let unreadItemId {
            scrollView.setScrollPosition(unreadIndex, unreadItemId)
        }
        scrollView.updateItems(mergedItems.boxedValue.items)
        if let unreadIndex {
            scrollView.scrollToItem(unreadIndex)
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            allowLoadMoreItems = true
        }
    }

    private func searchTextChanged(_ s: String) {
        Task {
            await loadChat(chat: chat, im: im, search: s)
            mergedItems.boxedValue = MergedItems.create(im, revealedItems)
            await MainActor.run {
                scrollView.updateItems(mergedItems.boxedValue.items)
            }
            if !s.isEmpty {
                scrollView.scrollToBottom()
            } else if let index = scrollView.listState.items.lastIndex(where: { $0.hasUnread() }) {
                // scroll to the top unread item
                scrollView.scrollToItem(index)
            } else {
                scrollView.scrollToBottom()
            }
        }
    }

    private struct FloatingButtons: View {
        @ObservedObject var im: ItemsModel
        let theme: AppTheme
        let scrollView: EndlessScrollView<MergedItem>
        let chat: Chat
        @Binding var loadingMoreItems: Bool
        @Binding var loadingTopItems: Bool
        @Binding var requestedTopScroll: Bool
        @Binding var loadingBottomItems: Bool
        @Binding var requestedBottomScroll: Bool
        @Binding var animatedScrollingInProgress: Bool
        let listState: EndlessScrollView<MergedItem>.ListState
        @ObservedObject var model: FloatingButtonModel
        let reloadItems: () -> Void

        var body: some View {
            ZStack(alignment: .top) {
                if let date = model.date, date.timeIntervalSince1970 > 0 {
                     DateSeparator(date: date)
                         .padding(.vertical, 4).padding(.horizontal, 8)
                         .background(.thinMaterial)
                         .clipShape(Capsule())
                         .opacity(model.isDateVisible ? 1 : 0)
                         .padding(.vertical, 4)
                }
                VStack {
                    if model.unreadAbove > 0 && !animatedScrollingInProgress {
                        if loadingTopItems && requestedTopScroll {
                            circleButton { ProgressView() }
                        } else {
                            circleButton {
                                unreadCountText(model.unreadAbove)
                                    .font(.callout)
                                    .foregroundColor(theme.colors.primary)
                            }
                            .onTapGesture {
                                if loadingTopItems {
                                    requestedTopScroll = true
                                    requestedBottomScroll = false
                                } else {
                                    scrollToTopUnread()
                                }
                            }
                            .contextMenu {
                                Button {
                                    Task {
                                        await markChatRead(im, chat)
                                    }
                                } label: {
                                    Label("Mark read", systemImage: "checkmark")
                                }
                            }
                        }
                    }
                    Spacer()
                    if listState.firstVisibleItemIndex != 0 && !animatedScrollingInProgress {
                        if loadingBottomItems && requestedBottomScroll {
                            circleButton { ProgressView() }
                        } else {
                            circleButton {
                                Group {
                                    if model.unreadBelow > 0 {
                                        unreadCountText(model.unreadBelow)
                                            .font(.callout)
                                            .foregroundColor(theme.colors.primary)
                                    } else {
                                        Image(systemName: "chevron.down").foregroundColor(theme.colors.primary)
                                    }
                                }
                            }
                            .onTapGesture {
                                if loadingBottomItems || !im.lastItemsLoaded {
                                    requestedTopScroll = false
                                    requestedBottomScroll = true
                                } else {
                                    scrollToBottom()
                                }
                            }
                        }
                    }
                }
                .padding()
                .frame(maxWidth: .infinity, alignment: .trailing)
            }
            .onChange(of: loadingTopItems) { loading in
                if !loading && requestedTopScroll {
                    requestedTopScroll = false
                    scrollToTopUnread()
                }
            }
            .onChange(of: loadingBottomItems) { loading in
                if !loading && requestedBottomScroll && im.lastItemsLoaded {
                    requestedBottomScroll = false
                    scrollToBottom()
                }
            }
            .onDisappear(perform: model.resetDate)
        }

        private func scrollToTopUnread() {
            Task {
                if !im.chatState.splits.isEmpty {
                    await MainActor.run { loadingMoreItems = true }
                    await loadChat(chatId: chat.id, im: im, openAroundItemId: nil, clearItems: false)
                    await MainActor.run { reloadItems() }
                    if let index = listState.items.lastIndex(where: { $0.hasUnread() }) {
                        await MainActor.run { animatedScrollingInProgress = true }
                        await scrollView.scrollToItemAnimated(index)
                        await MainActor.run { animatedScrollingInProgress = false }
                    }
                    await MainActor.run { loadingMoreItems = false }
                } else if let index = listState.items.lastIndex(where: { $0.hasUnread() }) {
                    await MainActor.run { animatedScrollingInProgress = true }
                    // scroll to the top unread item
                    await scrollView.scrollToItemAnimated(index)
                    await MainActor.run { animatedScrollingInProgress = false }
                } else {
                    logger.debug("No more unread items, total: \(listState.items.count)")
                }
            }
        }

        private func scrollToBottom() {
            animatedScrollingInProgress = true
            Task {
                await scrollView.scrollToItemAnimated(0, top: false)
                await MainActor.run { animatedScrollingInProgress = false }
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
    }

    private struct DateSeparator: View {
        let date: Date

        var body: some View {
            Text(String.localizedStringWithFormat(
                NSLocalizedString("%@, %@", comment: "format for date separator in chat"),
                date.formatted(.dateTime.weekday(.abbreviated)),
                date.formatted(
                    Calendar.current.isDate(date, equalTo: .now, toGranularity: .year)
                    ? .dateTime.day().month(.abbreviated)
                    : .dateTime.day().month(.abbreviated).year()
                )
            ))
            .font(.callout)
            .fontWeight(.medium)
            .foregroundStyle(.secondary)
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
            if CallController.useCallKit(), let callUUID = call.callUUID {
                CallController.shared.endCall(callUUID: callUUID)
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
        showSearch = true
        searchFocussed = true
        searchText = ""
    }

    private func closeSearch() {
        showSearch = false
        searchText = ""
        searchFocussed = false
    }

    private func closeKeyboardAndRun(_ action: @escaping () -> Void) {
        var delay: TimeInterval = 0
        if keyboardVisible || keyboardHiddenDate.timeIntervalSinceNow >= -1 || showSearch {
            delay = 0.5
            closeSearch()
            hideKeyboard()
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + delay) {
            action()
        }
    }

    private func addMembersButton() -> some View {
        Button {
            if case let .group(gInfo, _) = chat.chatInfo {
                Task { await chatModel.loadGroupMembers(gInfo) { showAddMembersSheet = true } }
            }
        } label: {
            Image(systemName: "person.crop.circle.badge.plus")
        }
    }

    private func groupLinkButton() -> some View {
        Button {
            if case let .group(gInfo, _) = chat.chatInfo {
                Task {
                    do {
                        if let gLink = try apiGetGroupLink(gInfo.groupId) {
                            groupLink = gLink
                            groupLinkMemberRole = gLink.acceptMemberRole
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

    private func forwardSelectedMessages() {
        Task {
            do {
                if let selectedChatItems {
                    let (validItems, confirmation) = try await apiPlanForwardChatItems(
                        type: chat.chatInfo.chatType,
                        id: chat.chatInfo.apiId,
                        scope: chat.chatInfo.groupChatScope(),
                        itemIds: Array(selectedChatItems)
                    )
                    if let confirmation {
                        if validItems.count > 0 {
                            showAlert(
                                String.localizedStringWithFormat(
                                    NSLocalizedString("Forward %d message(s)?", comment: "alert title"),
                                    validItems.count
                                ),
                                message: forwardConfirmationText(confirmation) + "\n" +
                                    NSLocalizedString("Forward messages without files?", comment: "alert message")
                            ) {
                                switch confirmation {
                                case let .filesNotAccepted(fileIds):
                                    [forwardAction(validItems), downloadAction(fileIds), cancelAlertAction]
                                default:
                                    [forwardAction(validItems), cancelAlertAction]
                                }
                            }
                        } else {
                            showAlert(
                                NSLocalizedString("Nothing to forward!", comment: "alert title"),
                                message: forwardConfirmationText(confirmation)
                            ) {
                                switch confirmation {
                                case let .filesNotAccepted(fileIds):
                                    [downloadAction(fileIds), cancelAlertAction]
                                default:
                                    [okAlertAction]
                                }
                            }
                        }
                    } else {
                        await openForwardingSheet(validItems)
                    }
                }
            } catch {
                logger.error("Plan forward chat items failed: \(error.localizedDescription)")
            }
        }

        func forwardConfirmationText(_ fc: ForwardConfirmation) -> String {
            switch fc {
            case let .filesNotAccepted(fileIds):
                String.localizedStringWithFormat(
                    NSLocalizedString("%d file(s) were not downloaded.", comment: "forward confirmation reason"),
                    fileIds.count
                )
            case let .filesInProgress(filesCount):
                String.localizedStringWithFormat(
                    NSLocalizedString("%d file(s) are still being downloaded.", comment: "forward confirmation reason"),
                    filesCount
                )
            case let .filesMissing(filesCount):
                String.localizedStringWithFormat(
                    NSLocalizedString("%d file(s) were deleted.", comment: "forward confirmation reason"),
                    filesCount
                )
            case let .filesFailed(filesCount):
                String.localizedStringWithFormat(
                    NSLocalizedString("%d file(s) failed to download.", comment: "forward confirmation reason"),
                    filesCount
                )
            }
        }

        func forwardAction(_ items: [Int64]) -> UIAlertAction {
            UIAlertAction(
                title: NSLocalizedString("Forward messages", comment: "alert action"),
                style: .default,
                handler: { _ in Task { await openForwardingSheet(items) } }
            )
        }

        func downloadAction(_ fileIds: [Int64]) -> UIAlertAction {
            UIAlertAction(
                title: NSLocalizedString("Download files", comment: "alert action"),
                style: .default,
                handler: { _ in
                    Task {
                        if let user = ChatModel.shared.currentUser {
                            await receiveFiles(user: user, fileIds: fileIds)
                        }
                    }
                }
            )
        }

        func openForwardingSheet(_ items: [Int64]) async {
            var items = Set(items)
            var fci = [ChatItem]()
            for reversedChatItem in im.reversedChatItems {
                if items.contains(reversedChatItem.id) {
                    items.remove(reversedChatItem.id)
                    fci.insert(reversedChatItem, at: 0)
                }
                if items.isEmpty { break }
            }
            await MainActor.run { forwardedChatItems = fci }
        }
    }

    private func loadChatItems(_ chat: Chat, _ pagination: ChatPagination) async -> Bool {
        if loadingMoreItems { return false }
        await MainActor.run {
            loadingMoreItems = true
            if case .before = pagination {
                loadingTopItems = true
            } else if case .after = pagination {
                loadingBottomItems = true
            }
        }
        let triedToLoad = await loadChatItemsUnchecked(chat, pagination)
        await MainActor.run {
            loadingMoreItems = false
            if case .before = pagination {
                loadingTopItems = false
            } else if case .after = pagination {
                loadingBottomItems = false
            }
        }
        return triedToLoad
    }

    private func loadChatItemsUnchecked(_ chat: Chat, _ pagination: ChatPagination) async -> Bool {
        await apiLoadMessages(
            chat.chatInfo.id,
            im,
            pagination,
            searchText,
            nil,
            { visibleItemIndexesNonReversed(im, scrollView.listState, mergedItems.boxedValue) }
        )
        return true
    }

    func stopAudioPlayer() {
        VoiceItemState.chatView.values.forEach { $0.audioPlayer?.stop() }
        VoiceItemState.chatView = [:]
    }

    func onChatItemsUpdated() {
        if !mergedItems.boxedValue.isActualState() {
            //logger.debug("Items are not actual, waiting for the next update: \(String(describing: mergedItems.boxedValue.splits))  \(im.chatState.splits), \(mergedItems.boxedValue.indexInParentItems.count) vs \(im.reversedChatItems.count)")
            return
        }
        floatingButtonModel.updateOnListChange(scrollView.listState)
        preloadIfNeeded(
            im,
            $allowLoadMoreItems,
            $ignoreLoadingRequests,
            scrollView.listState,
            mergedItems,
            loadItems: { unchecked, pagination in
                if unchecked {
                    await loadChatItemsUnchecked(chat, pagination)
                } else {
                    await loadChatItems(chat, pagination)
                }
            },
            loadLastItems: {
                if !loadingMoreItems {
                    await loadLastItems($loadingMoreItems, loadingBottomItems: $loadingBottomItems, chat, im)
                }
            }
        )
    }

    private struct ChatItemWithMenu: View {
        @ObservedObject var im: ItemsModel
        @EnvironmentObject var m: ChatModel
        @EnvironmentObject var theme: AppTheme
        @AppStorage(DEFAULT_PROFILE_IMAGE_CORNER_RADIUS) private var profileRadius = defaultProfileImageCorner
        @Binding @ObservedObject var chat: Chat
        @ObservedObject var dummyModel: ChatItemDummyModel = .shared
        let index: Int
        let isLastItem: Bool
        let chatItem: ChatItem
        let scrollToItem: (ChatItem.ID) -> Void
        @Binding var scrollToItemId: ChatItem.ID?
        let merged: MergedItem
        let maxWidth: CGFloat
        @Binding var composeState: ComposeState
        @Binding var selectedMember: GMember?
        @Binding var showChatInfoSheet: Bool
        @Binding var revealedItems: Set<Int64>

        @State private var deletingItem: ChatItem? = nil
        @State private var showDeleteMessage = false
        @State private var deletingItems: [Int64] = []
        @State private var showDeleteMessages = false
        @State private var archivingReports: Set<Int64>? = nil
        @State private var showArchivingReports = false
        @State private var showChatItemInfoSheet: Bool = false
        @State private var chatItemInfo: ChatItemInfo?
        @State private var msgWidth: CGFloat = 0
        @State private var touchInProgress: Bool = false

        @Binding var selectedChatItems: Set<Int64>?
        @Binding var forwardedChatItems: [ChatItem]

        @Binding var searchText: String
        var closeKeyboardAndRun: (@escaping () -> Void) -> Void

        @State private var allowMenu: Bool = true
        @State private var markedRead = false
        @State private var markReadTask: Task<Void, Never>? = nil
        @State private var actionSheet: SomeActionSheet? = nil

        var revealed: Bool { revealedItems.contains(chatItem.id) }

        typealias ItemSeparation = (timestamp: Bool, largeGap: Bool, date: Date?)

        private func reveal(_ yes: Bool) -> Void {
            merged.revealItems(yes, $revealedItems)
        }

        func getItemSeparation(_ chatItem: ChatItem, _ prevItem: ChatItem?) -> ItemSeparation {
            guard let prevItem else {
                return ItemSeparation(timestamp: true, largeGap: true, date: nil)
            }

            let sameMemberAndDirection = if case .groupRcv(let prevGroupMember) = prevItem.chatDir, case .groupRcv(let groupMember) = chatItem.chatDir {
                groupMember.groupMemberId == prevGroupMember.groupMemberId
            } else {
                chatItem.chatDir.sent == prevItem.chatDir.sent
            }
            let largeGap = !sameMemberAndDirection || prevItem.meta.itemTs.timeIntervalSince(chatItem.meta.itemTs) > 60

            return ItemSeparation(
                timestamp: largeGap || formatTimestampMeta(chatItem.meta.itemTs) != formatTimestampMeta(prevItem.meta.itemTs),
                largeGap: largeGap,
                date: Calendar.current.isDate(chatItem.meta.itemTs, inSameDayAs: prevItem.meta.itemTs) ? nil : prevItem.meta.itemTs
            )
        }

        func shouldShowAvatar(_ current: ChatItem, _ older: ChatItem?) -> Bool {
            let oldIsGroupRcv = switch older?.chatDir {
            case .groupRcv: true
            default: false
            }
            let sameMember = switch (older?.chatDir, current.chatDir) {
            case (.groupRcv(let oldMember), .groupRcv(let member)):
                oldMember.memberId == member.memberId
            default:
                false
            }
            if case .groupRcv = current.chatDir, (older == nil || (!oldIsGroupRcv || !sameMember)) {
                return true
            } else {
                return false
            }
        }

        var body: some View {
            let last = isLastItem ? im.reversedChatItems.last : nil
            let listItem = merged.newest()
            let item = listItem.item
            let range: ClosedRange<Int>? = if case let .grouped(_, _, _, rangeInReversed, _, _, _, _) = merged {
                rangeInReversed.boxedValue
            } else {
                nil
            }
            let showAvatar = shouldShowAvatar(item, merged.oldest().nextItem)
            let single = switch merged {
            case .single: true
            default: false
            }
            let itemSeparation = getItemSeparation(item, single || revealed ? listItem.prevItem: nil)
            return VStack(spacing: 0) {
                if let last {
                    DateSeparator(date: last.meta.itemTs).padding(8)
                }
                chatItemListView(range, showAvatar, item, itemSeparation)
                    .overlay {
                        if let selected = selectedChatItems, chatItem.canBeDeletedForSelf {
                            Color.clear
                                .contentShape(Rectangle())
                                .simultaneousGesture(TapGesture().onEnded {
                                    let checked = selected.contains(chatItem.id)
                                    selectUnselectChatItem(select: !checked, chatItem)
                                })
                        }
                    }
                if let date = itemSeparation.date {
                    DateSeparator(date: date).padding(8)
                }
            }
            .onAppear {
                if markedRead {
                    return
                } else {
                    markedRead = true
                }
                if let range {
                    let (itemIds, unreadMentions) = unreadItemIds(range)
                    if !itemIds.isEmpty {
                        waitToMarkRead {
                            await apiMarkChatItemsRead(im, chat.chatInfo, itemIds, mentionsRead: unreadMentions)
                        }
                    }
                } else if chatItem.isRcvNew  {
                    waitToMarkRead {
                        await apiMarkChatItemsRead(im, chat.chatInfo, [chatItem.id], mentionsRead: chatItem.meta.userMention ? 1 : 0)
                    }
                }
            }
            .onDisappear {
                markReadTask?.cancel()
                markedRead = false
            }
            .actionSheet(item: $actionSheet) { $0.actionSheet }
            // skip updating struct on touch if no need to show GoTo button
            .if(touchInProgress || searchIsNotBlank || (chatItem.meta.itemForwarded != nil && chatItem.meta.itemForwarded != .unknown)) {
                // long press listener steals taps from top-level listener, so repeating it's logic here as well
                $0.onTapGesture {
                    hideKeyboard()
                }
                .onLongPressGesture(minimumDuration: .infinity, perform: {}, onPressingChanged: { pressing in
                    touchInProgress = pressing
                })
            }
        }

        private func unreadItemIds(_ range: ClosedRange<Int>) -> ([ChatItem.ID], Int) {
            var unreadItems: [ChatItem.ID] = []
            var unreadMentions: Int = 0

            for i in range {
                if i < 0 || i >= im.reversedChatItems.count {
                    break
                }
                let ci = im.reversedChatItems[i]
                if ci.isRcvNew {
                    unreadItems.append(ci.id)
                    if ci.meta.userMention {
                        unreadMentions += 1
                    }
                }
            }

            return (unreadItems, unreadMentions)
        }

        private func waitToMarkRead(_ op: @Sendable @escaping () async -> Void) {
            markReadTask = Task {
                do {
                    _ = try await Task.sleep(nanoseconds: 600_000000)
                    if m.chatId == chat.chatInfo.id {
                        await op()
                    }
                } catch {
                    // task was cancelled
                }
            }
        }

        private var searchIsNotBlank: Bool {
            get {
                searchText.count > 0 && !searchText.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
            }
        }

        @available(iOS 16.0, *)
        struct MemberLayout: Layout {
            let spacing: Double
            let msgWidth: Double

            private func sizes(subviews: Subviews, proposal: ProposedViewSize) -> (CGSize, CGSize) {
                assert(subviews.count == 2, "member layout must contain exactly two subviews")
                let roleSize = subviews[1].sizeThatFits(proposal)
                let memberSize = subviews[0].sizeThatFits(
                    ProposedViewSize(
                        width: (proposal.width ?? msgWidth) - roleSize.width,
                        height: proposal.height
                    )
                )
                return (memberSize, roleSize)
            }

            func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout Void) -> CGSize {
                let (memberSize, roleSize) = sizes(subviews: subviews, proposal: proposal)
                return CGSize(
                    width: min(
                        proposal.width ?? msgWidth,
                        max(msgWidth, roleSize.width + spacing + memberSize.width)
                    ),
                    height: max(memberSize.height, roleSize.height)
                )
            }

            func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout Void) {
                let (memberSize, roleSize) = sizes(subviews: subviews, proposal: proposal)
                subviews[0].place(
                    at: CGPoint(x: bounds.minX, y: bounds.midY - memberSize.height / 2),
                    proposal: ProposedViewSize(memberSize)
                )
                subviews[1].place(
                    at: CGPoint(
                        x: bounds.minX + max(memberSize.width + spacing, msgWidth - roleSize.width),
                        y: bounds.midY - roleSize.height / 2
                    ),
                    proposal: ProposedViewSize(roleSize)
                )
            }
        }

        @ViewBuilder func chatItemListView(
            _ range: ClosedRange<Int>?,
            _ showAvatar: Bool,
            _ ci: ChatItem,
            _ itemSeparation: ItemSeparation
        ) -> some View {
            let bottomPadding: Double = itemSeparation.largeGap ? 10 : 2
            if case let .groupRcv(member) = ci.chatDir,
               case let .group(groupInfo, _) = chat.chatInfo {
                if showAvatar {
                    VStack(alignment: .leading, spacing: 4) {
                        if ci.content.showMemberName {
                            Group {
                                let (prevMember, memCount): (GroupMember?, Int) =
                                if let range = range {
                                    m.getPrevHiddenMember(member, range)
                                } else {
                                    (nil, 1)
                                }
                                if memCount == 1 && (member.memberRole > .member || ci.meta.showGroupAsSender) {
                                    let (name, role) = if ci.meta.showGroupAsSender {
                                        (groupInfo.chatViewName, NSLocalizedString("group", comment: "shown on group welcome message"))
                                    } else {
                                        (member.chatViewName, member.memberRole.text)
                                    }
                                    Group {
                                        if #available(iOS 16.0, *) {
                                            MemberLayout(spacing: 16, msgWidth: msgWidth) {
                                                Text(name)
                                                    .lineLimit(1)
                                                Text(role)
                                                    .fontWeight(.semibold)
                                                    .lineLimit(1)
                                                    .padding(.trailing, 8)
                                            }
                                        } else {
                                            HStack(spacing: 16) {
                                                Text(name)
                                                    .lineLimit(1)
                                                Text(role)
                                                    .fontWeight(.semibold)
                                                    .lineLimit(1)
                                                    .layoutPriority(1)
                                            }
                                        }
                                    }
                                    .frame(
                                        maxWidth: maxWidth,
                                        alignment: chatItem.chatDir.sent ? .trailing : .leading
                                    )
                                } else {
                                    Text(memberNames(member, prevMember, memCount))
                                        .lineLimit(2)
                                }
                            }
                                .font(.caption)
                                .foregroundStyle(.secondary)
                                .padding(.leading, memberImageSize + 14 + (selectedChatItems != nil && ci.canBeDeletedForSelf ? 12 + 24 : 0))
                                .padding(.top, 3) // this is in addition to message sequence gap
                        }
                        HStack(alignment: .center, spacing: 0) {
                            if selectedChatItems != nil && ci.canBeDeletedForSelf {
                                SelectedChatItem(ciId: ci.id, selectedChatItems: $selectedChatItems)
                                    .padding(.trailing, 12)
                            }
                            HStack(alignment: .top, spacing: 10) {
                                if ci.meta.showGroupAsSender {
                                    ProfileImage(imageStr: groupInfo.image, iconName: groupInfo.chatIconName, size: memberImageSize, backgroundColor: theme.colors.background)
                                        .simultaneousGesture(TapGesture().onEnded {
                                            showChatInfoSheet = true
                                        })
                                } else {
                                    MemberProfileImage(member, size: memberImageSize, backgroundColor: theme.colors.background)
                                        .simultaneousGesture(TapGesture().onEnded {
                                            if let mem = m.getGroupMember(member.groupMemberId) {
                                                selectedMember = mem
                                            } else {
                                                let mem = GMember.init(member)
                                                m.groupMembers.append(mem)
                                                m.groupMembersIndexes[member.groupMemberId] = m.groupMembers.count - 1
                                                selectedMember = mem
                                            }
                                        })
                                }
                                chatItemWithMenu(ci, range, maxWidth, itemSeparation)
                                    .onPreferenceChange(DetermineWidth.Key.self) { msgWidth = $0 }
                            }
                        }
                    }
                    .padding(.bottom, bottomPadding)
                    .padding(.trailing)
                    .padding(.leading, 12)
                } else {
                    HStack(alignment: .center, spacing: 0) {
                        if selectedChatItems != nil && ci.canBeDeletedForSelf {
                            SelectedChatItem(ciId: ci.id, selectedChatItems: $selectedChatItems)
                                .padding(.leading, 12)
                        }
                        chatItemWithMenu(ci, range, maxWidth, itemSeparation)
                            .padding(.trailing)
                            .padding(.leading, 10 + memberImageSize + 12)
                    }
                    .padding(.bottom, bottomPadding)
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
                    chatItemWithMenu(ci, range, maxWidth, itemSeparation)
                        .padding(.horizontal)
                }
                .padding(.bottom, bottomPadding)
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

        func chatItemWithMenu(_ ci: ChatItem, _ range: ClosedRange<Int>?, _ maxWidth: CGFloat, _ itemSeparation: ItemSeparation) -> some View {
            let alignment: Alignment = ci.chatDir.sent ? .trailing : .leading
            return VStack(alignment: alignment.horizontal, spacing: 3) {
                HStack {
                    if ci.chatDir.sent {
                        goToItemButton(true)
                    }
                    ChatItemView(
                        chat: chat,
                        im: im,
                        chatItem: ci,
                        scrollToItem: scrollToItem,
                        scrollToItemId: $scrollToItemId,
                        maxWidth: maxWidth,
                        allowMenu: $allowMenu
                    )
                    .environment(\.revealed, revealed)
                    .environment(\.showTimestamp, itemSeparation.timestamp)
                    .modifier(ChatItemClipped(ci, tailVisible: itemSeparation.largeGap && (ci.meta.itemDeleted == nil || revealed)))
                    .contextMenu { menu(ci, range, live: composeState.liveMessage != nil) }
                    .accessibilityLabel("")
                    if !ci.chatDir.sent {
                        goToItemButton(false)
                    }
                }
                if ci.content.msgContent != nil && (ci.meta.itemDeleted == nil || revealed) && ci.reactions.count > 0 {
                    chatItemReactions(ci)
                        .padding(.bottom, 4)
                }
            }
                .confirmationDialog("Delete message?", isPresented: $showDeleteMessage, titleVisibility: .visible) {
                    Button("Delete for me", role: .destructive) {
                        deleteMessage(.cidmInternal, moderate: false)
                    }
                    if let di = deletingItem, di.meta.deletable && !di.localNote && !di.isReport {
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
                .confirmationDialog(archivingReports?.count == 1 ? "Archive report?" : "Archive \(archivingReports?.count ?? 0) reports?", isPresented: $showArchivingReports, titleVisibility: .visible) {
                    Button("For me", role: .destructive) {
                        if let reports = self.archivingReports {
                            archiveReports(chat, reports.sorted(), false)
                            self.archivingReports = []
                        }
                    }
                    if case let ChatInfo.group(groupInfo, _) = chat.chatInfo, groupInfo.membership.memberActive {
                        Button("For all moderators", role: .destructive) {
                            if let reports = self.archivingReports {
                                archiveReports(chat, reports.sorted(), true)
                                self.archivingReports = []
                            }
                        }
                    }
                }
                .frame(maxWidth: maxWidth, maxHeight: .infinity, alignment: alignment)
                .frame(minWidth: 0, maxWidth: .infinity, alignment: alignment)
                .sheet(isPresented: $showChatItemInfoSheet, onDismiss: {
                    chatItemInfo = nil
                }) {
                    ChatItemInfoView(ci: ci, userMemberId: chat.chatInfo.groupInfo?.membership.memberId, chatItemInfo: $chatItemInfo)
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
                    .if(chat.chatInfo.featureEnabled(.reactions) && (ci.allowAddReaction || r.userReacted)) { v in
                        v.simultaneousGesture(TapGesture().onEnded {
                            setReaction(ci, add: !r.userReacted, reaction: r.reaction)
                        })
                    }
                    switch chat.chatInfo {
                    case let .group(groupInfo, _):
                        v.contextMenu {
                            ReactionContextMenu(
                                groupInfo: groupInfo,
                                itemId: ci.id,
                                reactionCount: r,
                                selectedMember: $selectedMember,
                                profileRadius: profileRadius
                            )
                        }
                    case let .direct(contact):
                        v.contextMenu {
                            contactReactionMenu(contact, r)
                        }
                    default:
                        v
                    }
                }
            }
        }

        @ViewBuilder
        private func menu(_ ci: ChatItem, _ range: ClosedRange<Int>?, live: Bool) -> some View {
            if case let .group(gInfo, _) = chat.chatInfo, ci.isReport, ci.meta.itemDeleted == nil {
                if ci.chatDir != .groupSnd, gInfo.membership.memberRole >= .moderator {
                    archiveReportButton(ci)
                }
                deleteButton(ci, label: "Delete report")
            } else if let mc = ci.content.msgContent, !ci.isReport, ci.meta.itemDeleted == nil || revealed {
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
                if ci.chatDir != .groupSnd {
                    if let (groupInfo, _) = ci.memberToModerate(chat.chatInfo) {
                        moderateButton(ci, groupInfo)
                    } else if ci.meta.itemDeleted == nil && chat.groupFeatureEnabled(.reports),
                              case let .group(gInfo, _) = chat.chatInfo,
                              gInfo.membership.memberRole == .member
                                && !live
                                && composeState.voiceMessageRecordingState == .noRecording {
                        reportButton(ci)
                    }
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
                forwardedChatItems = [chatItem]
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
                        scope: cInfo.groupChatScope(),
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
                    systemImage: "square.and.arrow.down"
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
                        let ciInfo = try await apiGetChatItemInfo(type: cInfo.chatType, id: cInfo.apiId, scope: cInfo.groupChatScope(), itemId: ci.id)
                        await MainActor.run {
                            chatItemInfo = ciInfo
                        }
                        if case let .group(gInfo, _) = chat.chatInfo {
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
                    reveal(false)
                }
            } label: {
                Label(
                    NSLocalizedString("Hide", comment: "chat item action"),
                    systemImage: "eye.slash"
                )
            }
        }

        private func deleteButton(_ ci: ChatItem, label: LocalizedStringKey = "Delete") -> Button<some View> {
            Button(role: .destructive) {
                if !revealed,
                   let currIndex = m.getChatItemIndex(im, ci),
                   let ciCategory = ci.mergeCategory {
                    let (prevHidden, _) = m.getPrevShownChatItem(currIndex, ciCategory)
                    if let range = itemsRange(currIndex, prevHidden) {
                        var itemIds: [Int64] = []
                        for i in range {
                            itemIds.append(im.reversedChatItems[i].id)
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
                Label(label, systemImage: "trash")
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

        private func archiveReportButton(_ cItem: ChatItem) -> Button<some View> {
            Button {
                archivingReports = [cItem.id]
                showArchivingReports = true
            } label: {
                Label("Archive report", systemImage: "archivebox")
            }
        }

        private func revealButton(_ ci: ChatItem) -> Button<some View> {
            Button {
                withConditionalAnimation {
                    reveal(true)
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
                    reveal(true)
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
                    reveal(false)
                }
            } label: {
                Label (
                    NSLocalizedString("Hide", comment: "chat item action"),
                    systemImage: "arrow.down.and.line.horizontal.and.arrow.up"
                )
            }
        }

        private func reportButton(_ ci: ChatItem) -> Button<some View> {
            Button(role: .destructive) {
                var buttons: [ActionSheet.Button] = ReportReason.supportedReasons.map { reason in
                    .default(Text(reason.text)) {
                        withAnimation {
                            if composeState.editing {
                                composeState = ComposeState(preview: .noPreview, contextItem: .reportedItem(chatItem: chatItem, reason: reason))
                            } else {
                                composeState = composeState.copy(preview: .noPreview, contextItem: .reportedItem(chatItem: chatItem, reason: reason))
                            }
                        }
                    }
                }

                buttons.append(.cancel())

                actionSheet = SomeActionSheet(
                    actionSheet: ActionSheet(
                        title: Text("Report reason?"),
                        buttons: buttons
                    ),
                    id: "reportChatMessage"
                )
            } label: {
                Label (
                    NSLocalizedString("Report", comment: "chat item action"),
                    systemImage: "flag"
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
               let currIndex = m.getChatItemIndex(im, ci),
               let ciCategory = ci.mergeCategory {
                let (prevHidden, _) = m.getPrevShownChatItem(currIndex, ciCategory)
                if let range = itemsRange(currIndex, prevHidden) {
                    for i in range {
                        itemIds.append(im.reversedChatItems[i].id)
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
                                scope: chat.chatInfo.groupChatScope(),
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
                                let deletedItem = itemDeletion.deletedChatItem.chatItem
                                if deletedItem.isActiveReport {
                                    m.decreaseGroupReportsCounter(chat.chatInfo.id)
                                }
                                m.updateChatInfo(itemDeletion.deletedChatItem.chatInfo)
                            }
                        }
                    }
                } catch {
                    logger.error("ChatView.deleteMessage error: \(error)")
                }
            }
        }

        @ViewBuilder private func contactReactionMenu(_ contact: Contact, _ r: CIReactionCount) -> some View {
            if !r.userReacted || r.totalReacted > 1 {
                Button { showChatInfoSheet = true } label: {
                    profileMenuItem(Text(contact.displayName), contact.image, radius: profileRadius)
                }
            }
            if r.userReacted {
                Button {} label: {
                    profileMenuItem(Text("you"), m.currentUser?.profile.image, radius: profileRadius)
                }
                .disabled(true)
            }
        }

        func goToItemInnerButton(_ alignStart: Bool, _ image: String, touchInProgress: Bool, _ onClick: @escaping () -> Void) -> some View {
            Image(systemName: image)
                .resizable()
                .frame(width: 13, height: 13)
                .padding([alignStart ? .trailing : .leading], 10)
                .tint(theme.colors.secondary.opacity(touchInProgress ? 1.0 : 0.4))
                .simultaneousGesture(TapGesture().onEnded(onClick))
        }

        @ViewBuilder
        func goToItemButton(_ alignStart: Bool) -> some View {
            let chatTypeApiIdMsgId = chatItem.meta.itemForwarded?.chatTypeApiIdMsgId
            if searchIsNotBlank {
                goToItemInnerButton(alignStart, "magnifyingglass", touchInProgress: touchInProgress) {
                    closeKeyboardAndRun {
                        im.loadOpenChatNoWait(chat.id, chatItem.id)
                    }
                }
            } else if let chatTypeApiIdMsgId {
                goToItemInnerButton(alignStart, "arrow.right", touchInProgress: touchInProgress) {
                    closeKeyboardAndRun {
                        let (chatType, apiId, msgId) = chatTypeApiIdMsgId
                        im.loadOpenChatNoWait("\(chatType.rawValue)\(apiId)", msgId)
                    }
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

class FloatingButtonModel: ObservableObject {
    @ObservedObject var im: ItemsModel

    public init(im: ItemsModel) {
        self.im = im
    }

    @Published var unreadAbove: Int = 0
    @Published var unreadBelow: Int = 0
    @Published var isNearBottom: Bool = true
    @Published var date: Date? = nil
    @Published var isDateVisible: Bool = false
    var hideDateWorkItem: DispatchWorkItem? = nil

    func updateOnListChange(_ listState: EndlessScrollView<MergedItem>.ListState) {
        let lastVisibleItem = oldestPartiallyVisibleListItemInListStateOrNull(listState)
        let unreadBelow = if let lastVisibleItem {
            max(0, im.chatState.unreadTotal - lastVisibleItem.unreadBefore)
        } else {
            0
        }
        let unreadAbove = im.chatState.unreadTotal - unreadBelow
        let date: Date? =
        if let lastVisible = listState.visibleItems.last {
            Calendar.current.startOfDay(for: lastVisible.item.oldest().item.meta.itemTs)
            } else {
                nil
            }

        // set the counters and date indicator
        DispatchQueue.main.async { [weak self] in
            guard let it = self else { return }
            it.setDate(visibility: true)
            it.unreadAbove = unreadAbove
            it.unreadBelow = unreadBelow
            it.date = date
        }

        // set floating button indication mode
        let nearBottom = listState.firstVisibleItemIndex < 1
        if nearBottom != self.isNearBottom {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) { [weak self] in
                self?.isNearBottom = nearBottom
            }
        }

        // hide Date indicator after 1 second of no scrolling
        hideDateWorkItem?.cancel()
        let workItem = DispatchWorkItem { [weak self] in
            guard let it = self else { return }
            it.setDate(visibility: false)
            it.hideDateWorkItem = nil
        }
        DispatchQueue.main.async { [weak self] in
            self?.hideDateWorkItem = workItem
            DispatchQueue.main.asyncAfter(deadline: .now() + 1, execute: workItem)
        }
    }

    func resetDate() {
        date = nil
        isDateVisible = false
    }

    private func setDate(visibility isVisible: Bool) {
        if isVisible {
            if !isNearBottom,
               !isDateVisible,
               let date, !Calendar.current.isDateInToday(date) {
                withAnimation { self.isDateVisible = true }
            }
        } else if isDateVisible {
            withAnimation { self.isDateVisible = false }
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
                        scope: chatInfo.groupChatScope(),
                        itemIds: itemIds,
                        mode: mode
                    )
                }

                await MainActor.run {
                    for di in deletedItems {
                        if let toItem = di.toChatItem {
                            _ = ChatModel.shared.upsertChatItem(chatInfo, toItem.chatItem)
                        } else {
                            ChatModel.shared.removeChatItem(chatInfo, di.deletedChatItem.chatItem)
                        }
                        let deletedItem = di.deletedChatItem.chatItem
                        if deletedItem.isActiveReport {
                            ChatModel.shared.decreaseGroupReportsCounter(chatInfo.id)
                        }
                    }
                    if let updatedChatInfo = deletedItems.last?.deletedChatItem.chatInfo {
                        ChatModel.shared.updateChatInfo(updatedChatInfo)
                    }
                }
                await onSuccess()
            } catch {
                logger.error("ChatView.deleteMessages error: \(error.localizedDescription)")
            }
        }
    }
}

func archiveReports(_ chat: Chat, _ itemIds: [Int64], _ forAll: Bool, _ onSuccess: @escaping () async -> Void = {}) {
    if itemIds.count > 0 {
        let chatInfo = chat.chatInfo
        Task {
            do {
                let deleted = try await apiDeleteReceivedReports(
                    groupId: chatInfo.apiId,
                    itemIds: itemIds,
                    mode: forAll ? CIDeleteMode.cidmBroadcast : CIDeleteMode.cidmInternalMark
                )

                await MainActor.run {
                    for di in deleted {
                        if let toItem = di.toChatItem {
                            _ = ChatModel.shared.upsertChatItem(chatInfo, toItem.chatItem)
                        } else {
                            ChatModel.shared.removeChatItem(chatInfo, di.deletedChatItem.chatItem)
                        }
                        let deletedItem = di.deletedChatItem.chatItem
                        if deletedItem.isActiveReport {
                            ChatModel.shared.decreaseGroupReportsCounter(chatInfo.id)
                        }
                    }
                    if let updatedChatInfo = deleted.last?.deletedChatItem.chatInfo {
                        ChatModel.shared.updateChatInfo(updatedChatInfo)
                    }
                }
                await onSuccess()
            } catch {
                logger.error("ChatView.archiveReports error: \(error.localizedDescription)")
            }
        }
    }
}

private func buildTheme() -> AppTheme {
    if let cId = ChatModel.shared.chatId, let chat = ChatModel.shared.getChat(cId) {
        let perChatTheme = if case let .direct(contact) = chat.chatInfo {
            contact.uiThemes?.preferredMode(!AppTheme.shared.colors.isLight)
        } else if case let .group(groupInfo, _) = chat.chatInfo {
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

struct ReactionContextMenu: View {
    @EnvironmentObject var m: ChatModel
    let groupInfo: GroupInfo
    var itemId: Int64
    var reactionCount: CIReactionCount
    @Binding var selectedMember: GMember?
    var profileRadius: CGFloat
    @State private var memberReactions: [MemberReaction] = []

    var body: some View {
        groupMemberReactionList()
            .task {
                await loadChatItemReaction()
            }
    }

    @ViewBuilder private func groupMemberReactionList() -> some View {
        if memberReactions.isEmpty {
            ForEach(Array(repeating: 0, count: reactionCount.totalReacted), id: \.self) { _ in
                textSpace
            }
        } else {
            ForEach(memberReactions, id: \.groupMember.groupMemberId) { mr in
                let mem = mr.groupMember
                let userMember = mem.groupMemberId == groupInfo.membership.groupMemberId
                Button {
                    if let member = m.getGroupMember(mem.groupMemberId) {
                        selectedMember = member
                    } else {
                        let member = GMember.init(mem)
                        m.groupMembers.append(member)
                        m.groupMembersIndexes[member.groupMemberId] = m.groupMembers.count - 1
                        selectedMember = member
                    }
                } label: {
                    profileMenuItem(Text(mem.displayName), mem.image, radius: profileRadius)
                }
                .disabled(userMember)
            }
        }
    }

    private func loadChatItemReaction() async {
        do {
            let memberReactions = try await apiGetReactionMembers(
                groupId: groupInfo.groupId,
                itemId: itemId,
                reaction: reactionCount.reaction
            )
            await MainActor.run {
                self.memberReactions = memberReactions
            }
        } catch let error {
            logger.error("apiGetReactionMembers error: \(responseError(error))")
        }
    }
}

func profileMenuItem(_ nameText: Text, _ image: String?, radius: CGFloat) -> some View {
    HStack {
        nameText
        if let image, let img = imageFromBase64(image) {
            Image(uiImage: maskToCustomShape(img, size: 30, radius: radius))
        } else {
            Image(systemName: "person.crop.circle")
        }
    }
}

func maskToCustomShape(_ image: UIImage, size: CGFloat, radius: CGFloat) -> UIImage {
    let path = Path { path in
        if radius >= 50 {
            path.addEllipse(in: CGRect(x: 0, y: 0, width: size, height: size))
        } else if radius <= 0 {
            path.addRect(CGRect(x: 0, y: 0, width: size, height: size))
        } else {
            let cornerRadius = size * CGFloat(radius) / 100
            path.addRoundedRect(
                in: CGRect(x: 0, y: 0, width: size, height: size),
                cornerSize: CGSize(width: cornerRadius, height: cornerRadius),
                style: .continuous
            )
        }
    }

    return UIGraphicsImageRenderer(size: CGSize(width: size, height: size)).image { context in
        context.cgContext.addPath(path.cgPath)
        context.cgContext.clip()
        let scale = size / max(image.size.width, image.size.height)
        let imageSize = CGSize(width: image.size.width * scale, height: image.size.height * scale)
        let imageOrigin = CGPoint(
            x: (size - imageSize.width) / 2,
            y: (size - imageSize.height) / 2
        )
        image.draw(in: CGRect(origin: imageOrigin, size: imageSize))
    }
}

struct ToggleNtfsButton: View {
    @ObservedObject var chat: Chat

    var body: some View {
        if let nextMode = chat.chatInfo.nextNtfMode {
            Button {
                toggleNotifications(chat, enableNtfs: nextMode)
            } label: {
                Label(nextMode.text(mentions: chat.chatInfo.hasMentions), systemImage: nextMode.icon)
            }
        }
    }
}

func toggleNotifications(_ chat: Chat, enableNtfs: MsgFilter) {
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
                let wasFavorite = chat.chatInfo.chatSettings?.favorite ?? false
                ChatTagsModel.shared.updateChatFavorite(favorite: chatSettings.favorite, wasFavorite: wasFavorite)
                let wasUnread = chat.unreadTag
                switch chat.chatInfo {
                case var .direct(contact):
                    contact.chatSettings = chatSettings
                    ChatModel.shared.updateContact(contact)
                case var .group(groupInfo, _):
                    groupInfo.chatSettings = chatSettings
                    ChatModel.shared.updateGroup(groupInfo)
                default: ()
                }
                ChatTagsModel.shared.updateChatTagRead(chat, wasUnread: wasUnread)
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
        let im = ItemsModel.shared
        im.reversedChatItems = [
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
        return ChatView(
            chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []),
            im: im,
            mergedItems: BoxedValue(MergedItems.create(im, [])),
            floatingButtonModel: FloatingButtonModel(im: im),
            scrollToItemId: Binding.constant(nil)
        )
        .environmentObject(chatModel)
    }
}
