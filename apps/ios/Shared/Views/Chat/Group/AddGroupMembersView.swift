//
//  AddGroupMembersView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 22.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct AddGroupMembersView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    var chat: Chat
    var groupInfo: GroupInfo
    var showSkip: Bool = false
    var addedMembersCb: ((Set<Int64>) -> Void)? = nil
    @State private var selectedContacts = Set<Int64>()
    @State private var selectedRole: GroupMemberRole = .admin
    @State private var alert: AddGroupMembersAlert?

    private enum AddGroupMembersAlert: Identifiable {
        case prohibitedToInviteIncognito
        case warnUnsafeToInviteIncognito
        case error(title: LocalizedStringKey, error: String = "")

        var id: String {
            switch self {
            case .prohibitedToInviteIncognito: return "prohibitedToInviteIncognito"
            case .warnUnsafeToInviteIncognito: return "warnUnsafeToInviteIncognito"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        NavigationView {
            let membersToAdd = filterMembersToAdd(chatModel.groupMembers)
            let nonIncognitoConnectionsSelected = membersToAdd
                .filter{ selectedContacts.contains($0.apiId) }
                .contains(where: { !$0.contactConnIncognito })
            let unsafeToInviteIncognito = chat.chatInfo.incognito && nonIncognitoConnectionsSelected

            let v = List {
                ChatInfoToolbar(chat: chat, imageSize: 48)
                .frame(maxWidth: .infinity, alignment: .center)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)

                if (membersToAdd.isEmpty) {
                    Text("No contacts to add")
                        .foregroundColor(.secondary)
                        .padding()
                        .frame(maxWidth: .infinity, alignment: .center)
                        .listRowBackground(Color.clear)
                } else {
                    let count = selectedContacts.count
                    Section {
                        rolePicker()
                        inviteMembersButton(unsafeToInviteIncognito)
                            .disabled(count < 1)
                    } footer: {
                        if (count >= 1) {
                            HStack {
                                Button { selectedContacts.removeAll() } label: { Text("Clear") }
                                Spacer()
                                Text("\(count) contact(s) selected")
                            }
                        } else {
                            Text("No contacts selected")
                                .frame(maxWidth: .infinity, alignment: .trailing)
                        }
                    }

                    Section {
                        ForEach(membersToAdd) { contact in
                            contactCheckView(contact)
                        }
                    }
                }
            }

            if (showSkip) {
                v.toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        if showSkip {
                            Button ("Skip") {
                                if let cb = addedMembersCb { cb(selectedContacts) }
                            }
                        }
                    }
                }
            } else {
                v.navigationBarHidden(true)
            }
        }
        .frame(maxHeight: .infinity, alignment: .top)
        .alert(item: $alert) { alert in
            switch alert {
            case .prohibitedToInviteIncognito:
                return Alert(title: Text("Can't invite contact to this group!"), message: Text("You're trying to invite contact with whom you've shared an incognito profile to the group in which you're using your main profile"))
            case .warnUnsafeToInviteIncognito:
                return Alert(
                    title: Text("Your main profile may be shared"),
                    message: Text("Some contacts you selected have your main profile. If they use SimpleX app older than v3.2 or some other client, they may share your main profile instead of a random incognito profile with other members."),
                    primaryButton: .destructive(Text("Invite anyway")) {
                        inviteMembers()
                    }, secondaryButton: .cancel()
                )
            case let .error(title, error):
                return Alert(title: Text(title), message: Text("\(error)"))
            }
        }
    }

    private func inviteMembersButton(_ unsafeToInviteIncognito: Bool) -> some View {
        Button {
            if unsafeToInviteIncognito {
                alert = .warnUnsafeToInviteIncognito
            } else {
                inviteMembers()
            }
        } label: {
            HStack {
                Text("Invite to group")
                Image(systemName: "checkmark")
            }
        }
        .frame(maxWidth: .infinity, alignment: .trailing)
    }

    private func inviteMembers() {
        Task {
            do {
                for contactId in selectedContacts {
                    let member = try await apiAddMember(groupInfo.groupId, contactId, selectedRole)
                    await MainActor.run { _ = ChatModel.shared.upsertGroupMember(groupInfo, member) }
                }
                await MainActor.run { dismiss() }
                if let cb = addedMembersCb { cb(selectedContacts) }
            } catch {
                alert = .error(title: "Error adding member(s)", error: responseError(error))
            }
        }
    }

    private func rolePicker() -> some View {
        Picker("New member role", selection: $selectedRole) {
            ForEach(GroupMemberRole.allCases) { role in
                if role <= groupInfo.membership.memberRole {
                    Text(role.text)
                }
            }
        }
    }

    private func contactCheckView(_ contact: Contact) -> some View {
        let checked = selectedContacts.contains(contact.apiId)
        let prohibitedToInviteIncognito = !chat.chatInfo.incognito && contact.contactConnIncognito
        let safeToInviteIncognito = chat.chatInfo.incognito && contact.contactConnIncognito
        var icon: String
        var iconColor: Color
        if prohibitedToInviteIncognito {
            icon = "theatermasks.circle.fill"
            iconColor = Color(uiColor: .tertiaryLabel)
        } else {
            if checked {
                icon = "checkmark.circle.fill"
                iconColor = .accentColor
            } else {
                icon = "circle"
                iconColor = Color(uiColor: .tertiaryLabel)
            }
        }
        return Button {
            if prohibitedToInviteIncognito {
                alert = .prohibitedToInviteIncognito
            } else {
                if checked {
                    selectedContacts.remove(contact.apiId)
                } else {
                    selectedContacts.insert(contact.apiId)
                }
            }
        } label: {
            HStack{
                ProfileImage(imageStr: contact.image)
                    .frame(width: 30, height: 30)
                    .padding(.trailing, 2)
                Text(ChatInfo.direct(contact: contact).chatViewName)
                    .foregroundColor(prohibitedToInviteIncognito ? .secondary : .primary)
                    .lineLimit(1)
                Spacer()
                if safeToInviteIncognito {
                    Image(systemName: "theatermasks")
                        .foregroundColor(.indigo)
                        .font(.footnote)
                }
                Image(systemName: icon)
                    .foregroundColor(iconColor)
            }
        }
    }
}

struct AddGroupMembersView_Previews: PreviewProvider {
    static var previews: some View {
        AddGroupMembersView(chat: Chat(chatInfo: ChatInfo.sampleData.group), groupInfo: GroupInfo.sampleData)
    }
}
