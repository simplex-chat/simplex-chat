//
//  TagListView.swift
//  SimpleX (iOS)
//
//  Created by Diogo Cunha on 31/12/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import ElegantEmojiPicker

struct TagEditorNavParams {
    let chat: Chat?
    let chatListTag: ChatTagData?
    let tagId: Int64?
}

struct TagListView: View {
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
                                message: String.localizedStringWithFormat(NSLocalizedString("All chats will be removed from the list %@, and the list deleted.", comment: "alert message"), text),
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
                                TagListEditor(
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
                    TagListEditor(chat: chat)
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
    
    private func radioButton(selected: Bool) -> some View {
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
                let tm = ChatTagsModel.shared
                tm.userTags = userTags
                if chat.unreadTag, let tags = chat.chatInfo.chatTags {
                    tm.decTagsReadCount(tags)
                }
                if var contact = chat.chatInfo.contact {
                    contact.chatTags = chatTags
                    m.updateContact(contact)
                } else if var group = chat.chatInfo.groupInfo {
                    group.chatTags = chatTags
                    m.updateGroup(group)
                }
                ChatTagsModel.shared.updateChatTagRead(chat, wasUnread: false)
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

struct TagListEditor: View {
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
