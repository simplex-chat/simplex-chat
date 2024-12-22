//
//  ChatListNavLink.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 01/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import ElegantEmojiPicker

typealias DynamicSizes = (
    rowHeight: CGFloat,
    profileImageSize: CGFloat,
    mediaSize: CGFloat,
    incognitoSize: CGFloat,
    chatInfoSize: CGFloat,
    unreadCorner: CGFloat,
    unreadPadding: CGFloat
)

private let dynamicSizes: [DynamicTypeSize: DynamicSizes] = [
    .xSmall: (68, 55, 33, 22, 18, 9, 3),
    .small: (72, 57, 34, 22, 18, 9, 3),
    .medium: (76, 60, 36, 22, 18, 10, 4),
    .large: (80, 63, 38, 24, 20, 10, 4),
    .xLarge: (88, 67, 41, 24, 20, 10, 4),
    .xxLarge: (100, 71, 44, 27, 22, 11, 4),
    .xxxLarge: (110, 75, 48, 30, 24, 12, 5),
    .accessibility1: (110, 75, 48, 30, 24, 12, 5),
    .accessibility2: (114, 75, 48, 30, 24, 12, 5),
    .accessibility3: (124, 75, 48, 30, 24, 12, 5),
    .accessibility4: (134, 75, 48, 30, 24, 12, 5),
    .accessibility5: (144, 75, 48, 30, 24, 12, 5)
]

private let defaultDynamicSizes: DynamicSizes = dynamicSizes[.large]!

func dynamicSize(_ font: DynamicTypeSize) -> DynamicSizes {
    dynamicSizes[font] ?? defaultDynamicSizes
}

struct ChatListNavLink: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatTagsModel: ChatTagsModel
    @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = false
    @ObservedObject var chat: Chat
    @Binding var parentSheet: SomeSheet<AnyView>?
    @State private var showContactRequestDialog = false
    @State private var showJoinGroupDialog = false
    @State private var showContactConnectionInfo = false
    @State private var showInvalidJSON = false
    @State private var alert: SomeAlert? = nil
    @State private var actionSheet: SomeActionSheet? = nil
    @State private var sheet: SomeSheet<AnyView>? = nil
    @State private var showConnectContactViaAddressDialog = false
    @State private var inProgress = false
    @State private var progressByTimeout = false

    var dynamicRowHeight: CGFloat { dynamicSize(userFont).rowHeight }

    var body: some View {
        Group {
            switch chat.chatInfo {
            case let .direct(contact):
                contactNavLink(contact)
            case let .group(groupInfo):
                groupNavLink(groupInfo)
            case let .local(noteFolder):
                noteFolderNavLink(noteFolder)
            case let .contactRequest(cReq):
                contactRequestNavLink(cReq)
            case let .contactConnection(cConn):
                contactConnectionNavLink(cConn)
            case let .invalidJSON(json):
                invalidJSONPreview(json)
            }
        }
        .onChange(of: inProgress) { inProgress in
            if inProgress {
                DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                    progressByTimeout = inProgress
                }
            } else {
                progressByTimeout = false
            }
        }
        .actionSheet(item: $actionSheet) { $0.actionSheet }
    }
    
    @ViewBuilder private func contactNavLink(_ contact: Contact) -> some View {
        Group {
            if contact.activeConn == nil && contact.profile.contactLink != nil && contact.active {
                ChatPreviewView(chat: chat, progressByTimeout: Binding.constant(false))
                    .frame(height: dynamicRowHeight)
                    .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                        Button {
                            deleteContactDialog(
                                chat,
                                contact,
                                dismissToChatList: false,
                                showAlert: { alert = $0 },
                                showActionSheet: { actionSheet = $0 },
                                showSheetContent: { sheet = $0 }
                            )
                        } label: {
                            deleteLabel
                        }
                        .tint(.red)
                    }
                    .onTapGesture { showConnectContactViaAddressDialog = true }
                    .confirmationDialog("Connect with \(contact.chatViewName)", isPresented: $showConnectContactViaAddressDialog, titleVisibility: .visible) {
                        Button("Use current profile") { connectContactViaAddress_(contact, false) }
                        Button("Use new incognito profile") { connectContactViaAddress_(contact, true) }
                    }
            } else {
                NavLinkPlain(
                    chatId: chat.chatInfo.id,
                    selection: $chatModel.chatId,
                    label: { ChatPreviewView(chat: chat, progressByTimeout: Binding.constant(false)) }
                )
                .swipeActions(edge: .leading, allowsFullSwipe: true) {
                    markReadButton()
                    toggleFavoriteButton()
                    toggleNtfsButton(chat: chat)
                }
                .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                    tagChatButton(chat)
                    if !chat.chatItems.isEmpty {
                        clearChatButton()
                    }
                    Button {
                        deleteContactDialog(
                            chat,
                            contact,
                            dismissToChatList: false,
                            showAlert: { alert = $0 },
                            showActionSheet: { actionSheet = $0 },
                            showSheetContent: { sheet = $0 }
                        )
                    } label: {
                        deleteLabel
                    }
                    .tint(.red)
                }
                .frame(height: dynamicRowHeight)
            }
        }
        .alert(item: $alert) { $0.alert }
        .sheet(item: $sheet) {
            if #available(iOS 16.0, *) {
                $0.content
                    .presentationDetents([.fraction($0.fraction)])
            } else {
                $0.content
            }
        }
    }

    @ViewBuilder private func groupNavLink(_ groupInfo: GroupInfo) -> some View {
        switch (groupInfo.membership.memberStatus) {
        case .memInvited:
            ChatPreviewView(chat: chat, progressByTimeout: $progressByTimeout)
                .frame(height: dynamicRowHeight)
                .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                    joinGroupButton()
                    if groupInfo.canDelete {
                        deleteGroupChatButton(groupInfo)
                    }
                }
                .onTapGesture { showJoinGroupDialog = true }
                .confirmationDialog("Group invitation", isPresented: $showJoinGroupDialog, titleVisibility: .visible) {
                    Button(chat.chatInfo.incognito ? "Join incognito" : "Join group") {
                        inProgress = true
                        joinGroup(groupInfo.groupId) {
                            await MainActor.run { inProgress = false }
                        }
                    }
                    Button("Delete invitation", role: .destructive) { Task { await deleteChat(chat) } }
                }
                .disabled(inProgress)
        case .memAccepted:
            ChatPreviewView(chat: chat, progressByTimeout: Binding.constant(false))
                .frame(height: dynamicRowHeight)
                .onTapGesture {
                    AlertManager.shared.showAlert(groupInvitationAcceptedAlert())
                }
                .swipeActions(edge: .trailing) {
                    tagChatButton(chat)
                    if (groupInfo.membership.memberCurrent) {
                        leaveGroupChatButton(groupInfo)
                    }
                    if groupInfo.canDelete {
                        deleteGroupChatButton(groupInfo)
                    }
                }
        default:
            NavLinkPlain(
                chatId: chat.chatInfo.id,
                selection: $chatModel.chatId,
                label: { ChatPreviewView(chat: chat, progressByTimeout: Binding.constant(false)) },
                disabled: !groupInfo.ready
            )
            .frame(height: dynamicRowHeight)
            .swipeActions(edge: .leading, allowsFullSwipe: true) {
                markReadButton()
                toggleFavoriteButton()
                toggleNtfsButton(chat: chat)
            }
            .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                tagChatButton(chat)
                let showClearButton = !chat.chatItems.isEmpty
                let showDeleteGroup = groupInfo.canDelete
                let showLeaveGroup = groupInfo.membership.memberCurrent
                let totalNumberOfButtons = 1 + (showClearButton ? 1 : 0) + (showDeleteGroup ? 1 : 0) + (showLeaveGroup ? 1 : 0)

                if showClearButton, totalNumberOfButtons <= 3 {
                    clearChatButton()
                }
                if (showLeaveGroup) {
                    leaveGroupChatButton(groupInfo)
                }
                
                if showDeleteGroup {
                    if totalNumberOfButtons <= 3 {
                        deleteGroupChatButton(groupInfo)
                    } else {
                        moreOptionsButton(chat, groupInfo)
                    }
                }
            }
        }
    }

    @ViewBuilder private func noteFolderNavLink(_ noteFolder: NoteFolder) -> some View {
        NavLinkPlain(
            chatId: chat.chatInfo.id,
            selection: $chatModel.chatId,
            label: { ChatPreviewView(chat: chat, progressByTimeout: Binding.constant(false)) },
            disabled: !noteFolder.ready
        )
        .frame(height: dynamicRowHeight)
        .swipeActions(edge: .leading, allowsFullSwipe: true) {
            markReadButton()
        }
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            if !chat.chatItems.isEmpty {
                clearNoteFolderButton()
            }
        }
    }

    private func joinGroupButton() -> some View {
        Button {
            inProgress = true
            joinGroup(chat.chatInfo.apiId) {
                await MainActor.run { inProgress = false }
            }
        } label: {
            SwipeLabel(NSLocalizedString("Join", comment: "swipe action"), systemImage: chat.chatInfo.incognito ? "theatermasks" : "ipad.and.arrow.forward", inverted: oneHandUI)
        }
        .tint(chat.chatInfo.incognito ? .indigo : theme.colors.primary)
    }

    @ViewBuilder private func markReadButton() -> some View {
        if chat.chatStats.unreadCount > 0 || chat.chatStats.unreadChat {
            Button {
                Task { await markChatRead(chat) }
            } label: {
                SwipeLabel(NSLocalizedString("Read", comment: "swipe action"), systemImage: "checkmark", inverted: oneHandUI)
            }
            .tint(theme.colors.primary)
        } else {
            Button {
                Task { await markChatUnread(chat) }
            } label: {
                SwipeLabel(NSLocalizedString("Unread", comment: "swipe action"), systemImage: "circlebadge.fill", inverted: oneHandUI)
            }
            .tint(theme.colors.primary)
        }

    }

    @ViewBuilder private func toggleFavoriteButton() -> some View {
        if chat.chatInfo.chatSettings?.favorite == true {
            Button {
                toggleChatFavorite(chat, favorite: false)
            } label: {
                SwipeLabel(NSLocalizedString("Unfav.", comment: "swipe action"), systemImage: "star.slash.fill", inverted: oneHandUI)
            }
            .tint(.green)
        } else {
            Button {
                toggleChatFavorite(chat, favorite: true)
            } label: {
                SwipeLabel(NSLocalizedString("Favorite", comment: "swipe action"), systemImage: "star.fill", inverted: oneHandUI)
            }
            .tint(.green)
        }
    }

    @ViewBuilder private func toggleNtfsButton(chat: Chat) -> some View {
        Button {
            toggleNotifications(chat, enableNtfs: !chat.chatInfo.ntfsEnabled)
        } label: {
            if chat.chatInfo.ntfsEnabled {
                SwipeLabel(NSLocalizedString("Mute", comment: "swipe action"), systemImage: "speaker.slash.fill", inverted: oneHandUI)
            } else {
                SwipeLabel(NSLocalizedString("Unmute", comment: "swipe action"), systemImage: "speaker.wave.2.fill", inverted: oneHandUI)
            }
        }
    }

    private func clearChatButton() -> some View {
        Button {
            AlertManager.shared.showAlert(clearChatAlert())
        } label: {
            SwipeLabel(NSLocalizedString("Clear", comment: "swipe action"), systemImage: "gobackward", inverted: oneHandUI)
        }
        .tint(Color.orange)
    }
    
    private func tagChatButton(_ chat: Chat) -> some View {
        Button {
            setTagChatSheet(chat)
        } label: {
            SwipeLabel(NSLocalizedString("List", comment: "swipe action"), systemImage: "tag.fill", inverted: oneHandUI)
        }
        .tint(.mint)
    }
    
    private func setTagChatSheet(_ chat: Chat) {
        let screenHeight = UIScreen.main.bounds.height
        let reservedSpace: Double = 4 * 44 // 2 for padding, 1 for "Create list" and another for extra tag
        let tagsSpace = Double(max(chatTagsModel.userTags.count, 3)) * 44
        let fraction = min((reservedSpace + tagsSpace) / screenHeight, 0.62)
        
        parentSheet = SomeSheet(
            content: {
                AnyView(
                    NavigationView {
                        if chatTagsModel.userTags.isEmpty {
                            ChatListTagEditor(chat: chat)
                        } else {
                            ChatListTag(chat: chat)
                        }
                    }
                )
            },
            id: "lists sheet",
            fraction: fraction
        )
    }
    
    private func moreOptionsButton(_ chat: Chat, _ groupInfo: GroupInfo?) -> some View {
        Button {
            var buttons: [Alert.Button] = [
                .default(Text("Clear")) {
                    AlertManager.shared.showAlert(clearChatAlert())
                }
            ]
            
            if let gi = groupInfo, gi.canDelete {
                buttons.append(.destructive(Text("Delete")) {
                    AlertManager.shared.showAlert(deleteGroupAlert(gi))
                })
            }
            
            buttons.append(.cancel())
                               
            actionSheet = SomeActionSheet(
                actionSheet: ActionSheet(
                    title: Text("Clear or delete group?"),
                    buttons: buttons
                ),
                id: "other options"
            )
        } label: {
            SwipeLabel(NSLocalizedString("More", comment: "swipe action"), systemImage: "ellipsis", inverted: oneHandUI)
        }
    }
    
    private func clearNoteFolderButton() -> some View {
        Button {
            AlertManager.shared.showAlert(clearNoteFolderAlert())
        } label: {
            SwipeLabel(NSLocalizedString("Clear", comment: "swipe action"), systemImage: "gobackward", inverted: oneHandUI)
        }
        .tint(Color.orange)
    }

    private func leaveGroupChatButton(_ groupInfo: GroupInfo) -> some View {
        Button {
            AlertManager.shared.showAlert(leaveGroupAlert(groupInfo))
        } label: {
            SwipeLabel(NSLocalizedString("Leave", comment: "swipe action"), systemImage: "rectangle.portrait.and.arrow.right.fill", inverted: oneHandUI)
        }
        .tint(Color.yellow)
    }

    private func deleteGroupChatButton(_ groupInfo: GroupInfo) -> some View {
        Button {
            AlertManager.shared.showAlert(deleteGroupAlert(groupInfo))
        } label: {
            deleteLabel
        }
        .tint(.red)
    }

    private func contactRequestNavLink(_ contactRequest: UserContactRequest) -> some View {
        ContactRequestView(contactRequest: contactRequest, chat: chat)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button {
                Task { await acceptContactRequest(incognito: false, contactRequest: contactRequest) }
            } label: { SwipeLabel(NSLocalizedString("Accept", comment: "swipe action"), systemImage: "checkmark", inverted: oneHandUI) }
                .tint(theme.colors.primary)
            Button {
                Task { await acceptContactRequest(incognito: true, contactRequest: contactRequest) }
            } label: {
                SwipeLabel(NSLocalizedString("Accept incognito", comment: "swipe action"), systemImage: "theatermasks.fill", inverted: oneHandUI)
            }
            .tint(.indigo)
            Button {
                AlertManager.shared.showAlert(rejectContactRequestAlert(contactRequest))
            } label: {
                SwipeLabel(NSLocalizedString("Reject", comment: "swipe action"), systemImage: "multiply.fill", inverted: oneHandUI)
            }
            .tint(.red)
        }
        .frame(height: dynamicRowHeight)
        .contentShape(Rectangle())
        .onTapGesture { showContactRequestDialog = true }
        .confirmationDialog("Accept connection request?", isPresented: $showContactRequestDialog, titleVisibility: .visible) {
            Button("Accept") { Task { await acceptContactRequest(incognito: false, contactRequest: contactRequest) } }
            Button("Accept incognito") { Task { await acceptContactRequest(incognito: true, contactRequest: contactRequest) } }
            Button("Reject (sender NOT notified)", role: .destructive) { Task { await rejectContactRequest(contactRequest) } }
        }
    }

    private func contactConnectionNavLink(_ contactConnection: PendingContactConnection) -> some View {
        ContactConnectionView(chat: chat)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button {
                AlertManager.shared.showAlert(deleteContactConnectionAlert(contactConnection) { a in
                    AlertManager.shared.showAlertMsg(title: a.title, message: a.message)
                })
            } label: {
                deleteLabel
            }
            .tint(.red)

            Button {
                showContactConnectionInfo = true
            } label: {
                SwipeLabel(NSLocalizedString("Name", comment: "swipe action"), systemImage: "pencil", inverted: oneHandUI)
            }
            .tint(theme.colors.primary)
        }
        .frame(height: dynamicRowHeight)
        .appSheet(isPresented: $showContactConnectionInfo) {
            Group {
                if case let .contactConnection(contactConnection) = chat.chatInfo {
                    ContactConnectionInfo(contactConnection: contactConnection)
                        .environment(\EnvironmentValues.refresh as! WritableKeyPath<EnvironmentValues, RefreshAction?>, nil)
                        .modifier(ThemedBackground(grouped: true))
                }
            }
        }
        .contentShape(Rectangle())
        .onTapGesture {
            showContactConnectionInfo = true
        }
    }

    private var deleteLabel: some View {
        SwipeLabel(NSLocalizedString("Delete", comment: "swipe action"), systemImage: "trash.fill", inverted: oneHandUI)
    }

    private func deleteGroupAlert(_ groupInfo: GroupInfo) -> Alert {
        let label: LocalizedStringKey = groupInfo.businessChat == nil ? "Delete group?" : "Delete chat?"
        return Alert(
            title: Text(label),
            message: deleteGroupAlertMessage(groupInfo),
            primaryButton: .destructive(Text("Delete")) {
                Task { await deleteChat(chat) }
            },
            secondaryButton: .cancel()
        )
    }

    private func clearChatAlert() -> Alert {
        Alert(
            title: Text("Clear conversation?"),
            message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
            primaryButton: .destructive(Text("Clear")) {
                Task { await clearChat(chat) }
            },
            secondaryButton: .cancel()
        )
    }

    private func clearNoteFolderAlert() -> Alert {
        Alert(
            title: Text("Clear private notes?"),
            message: Text("All messages will be deleted - this cannot be undone!"),
            primaryButton: .destructive(Text("Clear")) {
                Task { await clearChat(chat) }
            },
            secondaryButton: .cancel()
        )
    }

    private func leaveGroupAlert(_ groupInfo: GroupInfo) -> Alert {
        let titleLabel: LocalizedStringKey = groupInfo.businessChat == nil ? "Leave group?" : "Leave chat?"
        let messageLabel: LocalizedStringKey = (
            groupInfo.businessChat == nil
            ? "You will stop receiving messages from this group. Chat history will be preserved."
            : "You will stop receiving messages from this chat. Chat history will be preserved."
        )
        return Alert(
            title: Text(titleLabel),
            message: Text(messageLabel),
            primaryButton: .destructive(Text("Leave")) {
                Task { await leaveGroup(groupInfo.groupId) }
            },
            secondaryButton: .cancel()
        )
    }

    private func groupInvitationAcceptedAlert() -> Alert {
        Alert(
            title: Text("Joining group"),
            message: Text("You joined this group. Connecting to inviting group member.")
        )
    }

    private func invalidJSONPreview(_ json: String) -> some View {
        Text("invalid chat data")
            .foregroundColor(.red)
            .padding(4)
            .frame(height: dynamicRowHeight)
            .onTapGesture { showInvalidJSON = true }
            .appSheet(isPresented: $showInvalidJSON) {
                invalidJSONView(json)
                    .environment(\EnvironmentValues.refresh as! WritableKeyPath<EnvironmentValues, RefreshAction?>, nil)
            }
    }

    private func connectContactViaAddress_(_ contact: Contact, _ incognito: Bool) {
        Task {
            let ok = await connectContactViaAddress(contact.contactId, incognito, showAlert: { AlertManager.shared.showAlert($0) })
            if ok {
                ItemsModel.shared.loadOpenChat(contact.id)
                AlertManager.shared.showAlert(connReqSentAlert(.contact))
            }
        }
    }
}

struct TagEditorNavParams {
    let chat: Chat?
    let chatListTag: ChatTagData?
    let tagId: Int64?
}

struct ChatListTag: View {
    var chat: Chat? = nil
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatTagsModel: ChatTagsModel
    @EnvironmentObject var m: ChatModel
    @State private var editMode = EditMode.inactive
    @State private var tagEditorNavParams: TagEditorNavParams? = nil
    
    var chatTagsIds: [Int64] { chat?.chatInfo.contact?.chatTags ?? chat?.chatInfo.groupInfo?.chatTags ?? [] }
    
    var body: some View {
        List {
            Section {
                ForEach(chatTagsModel.userTags, id: \.id) { tag in
                    let text = tag.chatTagText
                    let emoji = tag.chatTagEmoji
                    let tagId = tag.chatTagId
                    let selected = chatTagsIds.contains(tagId)
                    
                    HStack {
                        if let emoji {
                            Text(emoji)
                        } else {
                            Image(systemName: "tag")
                        }
                        Text(text)
                            .padding(.leading, 12)
                        Spacer()
                        if chat != nil {
                            radioButton(selected: selected)
                        }
                    }
                    .contentShape(Rectangle())
                    .onTapGesture {
                        if let c = chat {
                            setChatTag(tagId: selected ? nil : tagId, chat: c) { dismiss() }
                        } else {
                            tagEditorNavParams = TagEditorNavParams(chat: nil, chatListTag: ChatTagData(emoji: emoji, text: text), tagId: tagId)
                        }
                    }
                    .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                        Button {
                            showAlert(
                                NSLocalizedString("Delete list?", comment: "alert title"),
                                message: NSLocalizedString("All chats will be removed from the list \(text), and the list deleted.", comment: "alert message"),
                                actions: {[
                                    UIAlertAction(
                                        title: NSLocalizedString("Cancel", comment: "alert action"),
                                        style: .default
                                    ),
                                    UIAlertAction(
                                        title: NSLocalizedString("Delete", comment: "alert action"),
                                        style: .destructive,
                                        handler: { _ in
                                            deleteTag(tagId)
                                        }
                                    )
                                ]}
                            )
                        } label: {
                            Label("Delete", systemImage: "trash.fill")
                        }
                        .tint(.red)
                    }
                    .swipeActions(edge: .leading, allowsFullSwipe: true) {
                        Button {
                            tagEditorNavParams = TagEditorNavParams(chat: nil, chatListTag: ChatTagData(emoji: emoji, text: text), tagId: tagId)
                        } label: {
                            Label("Edit", systemImage: "pencil")
                        }
                        .tint(theme.colors.primary)
                    }
                    .background(
                        // isActive required to navigate to edit view from any possible tag edited in swipe action
                        NavigationLink(isActive: Binding(get: { tagEditorNavParams != nil }, set: { _ in tagEditorNavParams = nil })) {
                            if let params = tagEditorNavParams {
                                ChatListTagEditor(
                                    chat: params.chat,
                                    tagId: params.tagId,
                                    emoji: params.chatListTag?.emoji,
                                    name: params.chatListTag?.text ?? ""
                                )
                            }
                        } label: {
                            EmptyView()
                        }
                            .opacity(0)
                    )
                }
                .onMove(perform: moveItem)
                
                NavigationLink {
                    ChatListTagEditor(chat: chat)
                } label: {
                    Label("Create list", systemImage: "plus")
                }
            } header: {
                if chat == nil {
                    editTagsButton()
                    .textCase(nil)
                    .frame(maxWidth: .infinity, alignment: .trailing)
                }
            }
        }
        .modifier(ThemedBackground(grouped: true))
        .environment(\.editMode, $editMode)
    }
    
    private func editTagsButton() -> some View {
        if editMode.isEditing {
            Button("Done") {
                editMode = .inactive
                dismiss()
            }
        } else {
            Button("Edit") {
                editMode = .active
            }
        }
    }
    
    @ViewBuilder private func radioButton(selected: Bool) -> some View {
        Image(systemName: selected ? "checkmark.circle.fill" : "circle")
            .imageScale(.large)
            .foregroundStyle(selected ? Color.accentColor : Color(.tertiaryLabel))
    }

    private func moveItem(from source: IndexSet, to destination: Int) {
        Task {
            do {
                var tags = chatTagsModel.userTags
                tags.move(fromOffsets: source, toOffset: destination)
                try await apiReorderChatTags(tagIds: tags.map { $0.chatTagId })
                
                await MainActor.run {
                    chatTagsModel.userTags = tags
                }
            } catch let error {
                showAlert(
                    NSLocalizedString("Error reordering lists", comment: "alert title"),
                    message: responseError(error)
                )
            }
        }
    }
    
    private func deleteTag(_ tagId: Int64) {
        Task {
            try await apiDeleteChatTag(tagId: tagId)

            await MainActor.run {
                chatTagsModel.userTags = chatTagsModel.userTags.filter { $0.chatTagId != tagId }
                if case let .userTag(tag) = chatTagsModel.activeFilter, tagId == tag.chatTagId {
                    chatTagsModel.activeFilter = nil
                }
                m.chats.forEach { c in
                    if var contact = c.chatInfo.contact, contact.chatTags.contains(tagId) {
                        contact.chatTags = contact.chatTags.filter({ $0 != tagId })
                        m.updateContact(contact)
                    } else if var group = c.chatInfo.groupInfo, group.chatTags.contains(tagId) {
                        group.chatTags = group.chatTags.filter({ $0 != tagId })
                        m.updateGroup(group)
                    }
                }
            }
        }
    }
}

private func setChatTag(tagId: Int64?, chat: Chat, closeSheet: @escaping () -> Void) {
    Task {
        do {
            let tagIds: [Int64] = if let t = tagId { [t] } else {[]}
            let (userTags, chatTags) = try await apiSetChatTags(
                type: chat.chatInfo.chatType,
                id: chat.chatInfo.apiId,
                tagIds: tagIds
            )
            
            await MainActor.run {
                let m = ChatModel.shared
                ChatTagsModel.shared.userTags = userTags
                if var contact = chat.chatInfo.contact {
                    contact.chatTags = chatTags
                    m.updateContact(contact)
                } else if var group = chat.chatInfo.groupInfo {
                    group.chatTags = chatTags
                    m.updateGroup(group)
                }
                closeSheet()
            }
        } catch let error {
            showAlert(
                NSLocalizedString("Error saving chat list", comment: "alert title"),
                message: responseError(error)
            )
        }
    }
}

struct EmojiPickerView: UIViewControllerRepresentable {
    @Binding var selectedEmoji: String?
    @Binding var showingPicker: Bool
    @Environment(\.presentationMode) var presentationMode

    class Coordinator: NSObject, ElegantEmojiPickerDelegate, UIAdaptivePresentationControllerDelegate {
        var parent: EmojiPickerView
        
        init(parent: EmojiPickerView) {
            self.parent = parent
        }
        
        func emojiPicker(_ picker: ElegantEmojiPicker, didSelectEmoji emoji: Emoji?) {
            parent.selectedEmoji = emoji?.emoji
            parent.showingPicker = false
            picker.dismiss(animated: true)
        }
        
        // Called when the picker is dismissed manually (without selection)
        func presentationControllerWillDismiss(_ presentationController: UIPresentationController) {
            parent.showingPicker = false
        }
    }
    
    func makeCoordinator() -> Coordinator {
        return Coordinator(parent: self)
    }
    
    func makeUIViewController(context: Context) -> UIViewController {
        let config = ElegantConfiguration(showRandom: false, showReset: true, showClose: false)
        let picker = ElegantEmojiPicker(delegate: context.coordinator, configuration: config)
        
        picker.presentationController?.delegate = context.coordinator

        let viewController = UIViewController()
        DispatchQueue.main.async {
            if let topVC = getTopViewController() {
                topVC.present(picker, animated: true)
            }
        }
        
        return viewController
    }
    
    func updateUIViewController(_ uiViewController: UIViewController, context: Context) {
        // No need to update the controller after creation
    }
}

struct ChatListTagEditor: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var chatTagsModel: ChatTagsModel
    @EnvironmentObject var theme: AppTheme
    var chat: Chat? = nil
    var tagId: Int64? = nil
    var emoji: String?
    var name: String = ""
    @State private var newEmoji: String?
    @State private var newName: String = ""
    @State private var isPickerPresented = false
    @State private var saving: Bool?

    var body: some View {
        VStack {
            List {
                let isDuplicateEmojiOrName = chatTagsModel.userTags.contains { tag in
                    tag.chatTagId != tagId &&
                    ((newEmoji != nil && tag.chatTagEmoji == newEmoji) || tag.chatTagText == trimmedName)
                }
                
                Section {
                    HStack {
                        Button {
                            isPickerPresented = true
                        } label: {
                            if let newEmoji {
                                Text(newEmoji)
                            } else {
                                Image(systemName: "face.smiling")
                                    .foregroundColor(.secondary)
                            }
                        }
                        TextField("List name...", text: $newName)
                    }
                    
                    Button {
                        saving = true
                        if let tId = tagId {
                            updateChatTag(tagId: tId, chatTagData: ChatTagData(emoji: newEmoji, text: trimmedName))
                        } else {
                            createChatTag()
                        }
                    } label: {
                        Text(
                            chat != nil
                            ? "Add to list"
                            : tagId == nil
                            ? "Create list"
                            : "Save list"
                        )
                    }
                    .disabled(saving != nil || (trimmedName == name && newEmoji == emoji) || trimmedName.isEmpty || isDuplicateEmojiOrName)
                } footer: {
                    if isDuplicateEmojiOrName && saving != false { // if not saved already, to prevent flickering
                        HStack {
                            Image(systemName: "exclamationmark.circle")
                                .foregroundColor(.red)
                            Text("List name and emoji should be different for all lists.")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }
                }
            }

            if isPickerPresented {
                EmojiPickerView(selectedEmoji: $newEmoji, showingPicker: $isPickerPresented)
            }
        }
        .modifier(ThemedBackground(grouped: true))
        .onAppear {
            newEmoji = emoji
            newName = name
        }
    }
    
    var trimmedName: String {
        newName.trimmingCharacters(in: .whitespaces)
    }
    
    private func createChatTag() {
        Task {
            do {
                let text = trimmedName
                let userTags = try await apiCreateChatTag(
                    tag: ChatTagData(emoji: newEmoji , text: text)
                )
                await MainActor.run {
                    saving = false
                    chatTagsModel.userTags = userTags
                }
                if let chat, let tag = userTags.first(where: { $0.chatTagText == text && $0.chatTagEmoji == newEmoji}) {
                    setChatTag(tagId: tag.chatTagId, chat: chat) { dismiss() }
                } else {
                    await MainActor.run { dismiss() }
                }
            } catch let error {
                await MainActor.run {
                    saving = nil
                    showAlert(
                        NSLocalizedString("Error creating list", comment: "alert title"),
                        message: responseError(error)
                    )
                }
            }
        }
    }
    
    private func updateChatTag(tagId: Int64, chatTagData: ChatTagData) {
        Task {
            do {
                try await apiUpdateChatTag(tagId: tagId, tag: chatTagData)
                await MainActor.run {
                    saving = false
                    for i in 0..<chatTagsModel.userTags.count {
                        if chatTagsModel.userTags[i].chatTagId == tagId {
                            chatTagsModel.userTags[i] = ChatTag(
                                chatTagId: tagId,
                                chatTagText: chatTagData.text,
                                chatTagEmoji: chatTagData.emoji
                            )
                        }
                    }
                    dismiss()
                }
            } catch let error {
                await MainActor.run {
                    saving = nil
                    showAlert(
                        NSLocalizedString("Error creating list", comment: "alert title"),
                        message: responseError(error)
                    )
                }
            }
        }
    }
}

func rejectContactRequestAlert(_ contactRequest: UserContactRequest) -> Alert {
    Alert(
        title: Text("Reject contact request"),
        message: Text("The sender will NOT be notified"),
        primaryButton: .destructive(Text("Reject")) {
            Task { await rejectContactRequest(contactRequest) }
        },
        secondaryButton: .cancel()
    )
}

func deleteContactConnectionAlert(_ contactConnection: PendingContactConnection, showError: @escaping (ErrorAlert) -> Void, success: @escaping () -> Void = {}) -> Alert {
    Alert(
        title: Text("Delete pending connection?"),
        message:
            contactConnection.initiated
            ? Text("The contact you shared this link with will NOT be able to connect!")
            : Text("The connection you accepted will be cancelled!"),
        primaryButton: .destructive(Text("Delete")) {
            Task {
                do {
                    try await apiDeleteChat(type: .contactConnection, id: contactConnection.apiId)
                    await MainActor.run {
                        ChatModel.shared.removeChat(contactConnection.id)
                        success()
                    }
                } catch let error {
                    await MainActor.run {
                        showError(getErrorAlert(error, "Error deleting connection"))
                    }
                }
            }
        },
        secondaryButton: .cancel()
    )
}

func connectContactViaAddress(_ contactId: Int64, _ incognito: Bool, showAlert: (Alert) -> Void) async -> Bool {
    let (contact, alert) = await apiConnectContactViaAddress(incognito: incognito, contactId: contactId)
    if let alert = alert {
        showAlert(alert)
        return false
    } else if let contact = contact {
        await MainActor.run {
            ChatModel.shared.updateContact(contact)
        }
        return true
    }
    return false
}

func joinGroup(_ groupId: Int64, _ onComplete: @escaping () async -> Void) {
    Task {
        logger.debug("joinGroup")
        do {
            let r = try await apiJoinGroup(groupId)
            switch r {
            case let .joined(groupInfo):
                await MainActor.run { ChatModel.shared.updateGroup(groupInfo) }
            case .invitationRemoved:
                AlertManager.shared.showAlertMsg(title: "Invitation expired!", message: "Group invitation is no longer valid, it was removed by sender.")
                await deleteGroup()
            case .groupNotFound:
                AlertManager.shared.showAlertMsg(title: "No group!", message: "This group no longer exists.")
                await deleteGroup()
            }
            await onComplete()
        } catch let error {
            await onComplete()
            let a = getErrorAlert(error, "Error joining group")
            AlertManager.shared.showAlertMsg(title: a.title, message: a.message)
        }

        func deleteGroup() async {
            do {
                // TODO this API should update chat item with the invitation as well
                try await apiDeleteChat(type: .group, id: groupId)
                await MainActor.run { ChatModel.shared.removeChat("#\(groupId)") }
            } catch {
                logger.error("apiDeleteChat error: \(responseError(error))")
            }
        }
    }
}

func getErrorAlert(_ error: Error, _ title: LocalizedStringKey) -> ErrorAlert {
    if let r = error as? ChatResponse,
       let alert = getNetworkErrorAlert(r) {
        return alert
    } else {
        return ErrorAlert(title: title, message: "Error: \(responseError(error))")
    }
}

struct ChatListNavLink_Previews: PreviewProvider {
    static var previews: some View {
        @State var chatId: String? = "@1"
        return Group {
            ChatListNavLink(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello")]
            ), parentSheet: .constant(nil))
            ChatListNavLink(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello")]
            ), parentSheet: .constant(nil))
            ChatListNavLink(chat: Chat(
                chatInfo: ChatInfo.sampleData.contactRequest,
                chatItems: []
            ), parentSheet: .constant(nil))
        }
        .previewLayout(.fixed(width: 360, height: 82))
    }
}
