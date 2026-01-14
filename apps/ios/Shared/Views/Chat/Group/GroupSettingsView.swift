//
//  GroupSettingsView.swift
//  SimpleX
//
//  Created by Suren Poghosyan on 12.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupSettingsView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    @Binding var groupInfo: GroupInfo
    @Binding var sendReceipts: SendReceipts
    var sendReceiptsUserDefault: Bool
    @Binding var progressIndicator: Bool
    var setSendReceipts: () -> Void
    var dismiss: DismissAction
    @State private var alert: GroupSettingsViewAlert? = nil
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    
    enum GroupSettingsViewAlert: Identifiable {
        case deleteGroupAlert
        case clearChatAlert
        case leaveGroupAlert
        case largeGroupReceiptsDisabled

        var id: String {
            switch self {
            case .deleteGroupAlert: return "deleteGroupAlert"
            case .clearChatAlert: return "clearChatAlert"
            case .leaveGroupAlert: return "leaveGroupAlert"
            case .largeGroupReceiptsDisabled: return "largeGroupReceiptsDisabled"
            }
        }
    }
    
    var body: some View {
        let members = chatModel.groupMembers
            .filter { m in let status = m.wrapped.memberStatus; return status != .memLeft && status != .memRemoved }
        
        List {
            Section {
                if groupInfo.isOwner && groupInfo.businessChat == nil {
                    editGroupButton()
                }
                if groupInfo.groupProfile.description != nil || (groupInfo.isOwner && groupInfo.businessChat == nil) {
                    addOrEditWelcomeMessage()
                }
                GroupPreferencesButton(groupInfo: $groupInfo, preferences: groupInfo.fullGroupPreferences, currentPreferences: groupInfo.fullGroupPreferences)
            } footer: {
                let label: LocalizedStringKey = (
                    groupInfo.businessChat == nil
                    ? "Only group owners can change group preferences."
                    : "Only chat owners can change preferences."
                )
                Text(label)
                    .foregroundColor(theme.colors.secondary)
            }
            
            Section {
                if members.filter({ $0.wrapped.memberCurrent }).count <= SMALL_GROUPS_RCPS_MEM_LIMIT {
                    sendReceiptsOption()
                } else {
                    sendReceiptsOptionDisabled()
                }
                NavigationLink {
                    ChatWallpaperEditorSheet(chat: chat)
                } label: {
                    Label("Chat theme", systemImage: "photo")
                }
                ChatTTLOption(chat: chat, progressIndicator: $progressIndicator)
            } footer: {
                Text("Delete chat messages from your device.")
            }
            
            Section {
                clearChatButton()
                if groupInfo.canDelete {
                    deleteGroupButton()
                }
                if groupInfo.membership.memberCurrentOrPending {
                    leaveGroupButton()
                }
            }
            
            if developerTools {
                Section(header: Text("For console").foregroundColor(theme.colors.secondary)) {
                    infoRow("Local name", chat.chatInfo.localDisplayName)
                    infoRow("Database ID", "\(chat.chatInfo.apiId)")
                }
            }
        }
        .navigationTitle("Chat Settings")
        .modifier(ThemedBackground(grouped: true))
        .navigationBarTitleDisplayMode(.large)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .deleteGroupAlert: return deleteGroupAlert()
            case .clearChatAlert: return clearChatAlert()
            case .leaveGroupAlert: return leaveGroupAlert()
            case .largeGroupReceiptsDisabled: return largeGroupReceiptsDisabledAlert()
            }
        }
    }
    
    private func editGroupButton() -> some View {
        NavigationLink {
            GroupProfileView(
                groupInfo: $groupInfo,
                groupProfile: groupInfo.groupProfile
            )
        } label: {
            Label("Edit group profile", systemImage: "pencil")
        }
    }
    
    private func addOrEditWelcomeMessage() -> some View {
        NavigationLink {
            GroupWelcomeView(
                groupInfo: $groupInfo,
                groupProfile: groupInfo.groupProfile,
                welcomeText: groupInfo.groupProfile.description ?? ""
            )
            .navigationTitle("Welcome message")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            groupInfo.groupProfile.description == nil
            ? Label("Add welcome message", systemImage: "plus.message")
            : Label("Welcome message", systemImage: "message")
        }
    }
    
    private func sendReceiptsOption() -> some View {
        WrappedPicker(selection: $sendReceipts) {
            ForEach([.yes, .no, .userDefault(sendReceiptsUserDefault)]) { (opt: SendReceipts) in
                Text(opt.text)
            }
        } label: {
            Label("Send receipts", systemImage: "checkmark.message")
        }
        .onChange(of: sendReceipts) { _ in
            setSendReceipts()
        }
    }
    
    private func sendReceiptsOptionDisabled() -> some View {
        HStack {
            Label("Send receipts", systemImage: "checkmark.message")
            Spacer()
            Text("disabled")
                .foregroundStyle(.secondary)
        }
        .onTapGesture {
            alert = .largeGroupReceiptsDisabled
        }
    }
    
    @ViewBuilder private func deleteGroupButton() -> some View {
        let label: LocalizedStringKey = groupInfo.businessChat == nil ? "Delete group" : "Delete chat"
        Button(role: .destructive) {
            alert = .deleteGroupAlert
        } label: {
            Label(label, systemImage: "trash")
                .foregroundColor(Color.red)
        }
    }
    
    private func clearChatButton() -> some View {
        Button() {
            alert = .clearChatAlert
        } label: {
            Label("Clear conversation", systemImage: "gobackward")
                .foregroundColor(Color.orange)
        }
    }
    
    private func leaveGroupButton() -> some View {
        let label: LocalizedStringKey = groupInfo.businessChat == nil ? "Leave group" : "Leave chat"
        return Button(role: .destructive) {
            alert = .leaveGroupAlert
        } label: {
            Label(label, systemImage: "rectangle.portrait.and.arrow.right")
                .foregroundColor(Color.red)
        }
    }
    
    private func deleteGroupAlert() -> Alert {
        let label: LocalizedStringKey = groupInfo.businessChat == nil ? "Delete group?" : "Delete chat?"
        return Alert(
            title: Text(label),
            message: deleteGroupAlertMessage(groupInfo),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                        await MainActor.run {
                            dismiss()
                            chatModel.chatId = nil
                            chatModel.removeChat(chat.chatInfo.id)
                        }
                    } catch let error {
                        logger.error("deleteGroupAlert apiDeleteChat error: \(error.localizedDescription)")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }
    
    private func clearChatAlert() -> Alert {
        Alert(
            title: Text("Clear conversation?"),
            message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
            primaryButton: .destructive(Text("Clear")) {
                Task {
                    await clearChat(chat)
                    await MainActor.run { dismiss() }
                }
            },
            secondaryButton: .cancel()
        )
    }
    
    private func leaveGroupAlert() -> Alert {
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
                Task {
                    await leaveGroup(chat.chatInfo.apiId)
                    await MainActor.run { dismiss() }
                }
            },
            secondaryButton: .cancel()
        )
    }
    
    private func largeGroupReceiptsDisabledAlert() -> Alert {
        Alert(
            title: Text("Receipts are disabled"),
            message: Text("This group has over \(SMALL_GROUPS_RCPS_MEM_LIMIT) members, delivery receipts are not sent.")
        )
    }
}

struct GroupPreferencesButton: View {
    @Binding var groupInfo: GroupInfo
    @State var preferences: FullGroupPreferences
    @State var currentPreferences: FullGroupPreferences
    var creatingGroup: Bool = false
    
    private var label: LocalizedStringKey {
        groupInfo.businessChat == nil ? "Group preferences" : "Chat preferences"
    }
    
    var body: some View {
        NavigationLink {
            GroupPreferencesView(
                groupInfo: $groupInfo,
                preferences: $preferences,
                currentPreferences: currentPreferences,
                creatingGroup: creatingGroup,
                savePreferences: savePreferences
            )
            .navigationBarTitle(label)
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
            .onDisappear {
                let saveText = NSLocalizedString(
                    creatingGroup ? "Save" : "Save and notify group members",
                    comment: "alert button"
                )
                
                if groupInfo.fullGroupPreferences != preferences {
                    showAlert(
                        title: NSLocalizedString("Save preferences?", comment: "alert title"),
                        buttonTitle: saveText,
                        buttonAction: { savePreferences() },
                        cancelButton: true
                    )
                }
            }
        } label: {
            if creatingGroup {
                Text("Set group preferences")
            } else {
                Label(label, systemImage: "switch.2")
            }
        }
    }
    
    private func savePreferences() {
        Task {
            do {
                var gp = groupInfo.groupProfile
                gp.groupPreferences = toGroupPreferences(preferences)
                let gInfo = try await apiUpdateGroup(groupInfo.groupId, gp)
                await MainActor.run {
                    groupInfo = gInfo
                    ChatModel.shared.updateGroup(gInfo)
                    currentPreferences = preferences
                }
            } catch {
                logger.error("GroupPreferencesView apiUpdateGroup error: \(responseError(error))")
            }
        }
    }
}
