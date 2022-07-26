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
    var chat: Chat
    var groupInfo: GroupInfo
    @Binding var showSheet: Bool
    @State private var contactsToAdd: [Contact] = []
    @State private var selectedContacts = Set<Int64>()
    @State private var selectedRole: GroupMemberRole = .admin

    var body: some View {
        NavigationView {
            List {
                ChatInfoToolbar(chat: chat, imageSize: 48)
                .frame(maxWidth: .infinity, alignment: .center)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)

                if (contactsToAdd.isEmpty) {
                    Text("No contacts to add")
                        .foregroundColor(.secondary)
                        .padding()
                        .frame(maxWidth: .infinity, alignment: .center)
                        .listRowBackground(Color.clear)
                } else {
                    let count = selectedContacts.count
                    Section {
                        rolePicker()
                        inviteMembersButton()
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
                        ForEach(contactsToAdd) { contact in
                            contactCheckView(contact)
                        }
                    }
                }
            }
            .navigationBarHidden(true)
        }
        .frame(maxHeight: .infinity, alignment: .top)
        .task {
            contactsToAdd = await getContactsToAdd()
        }
    }

    func getContactsToAdd() async -> [Contact] {
        let memberContactIds = await apiListMembers(chat.chatInfo.apiId)
            .compactMap{ $0.memberContactId }
        return chatModel.chats
            .compactMap{ $0.chatInfo.contact }
            .filter{ !memberContactIds.contains($0.apiId) }
            .sorted{ $0.displayName.lowercased() < $1.displayName.lowercased() }
    }

    func inviteMembersButton() -> some View {
        Button {
            Task {
                for contactId in selectedContacts {
                    await addMember(groupId: chat.chatInfo.apiId, contactId: contactId, memberRole: selectedRole)
                }
                showSheet = false
            }
        } label: {
            HStack {
                Text("Invite members")
                Image(systemName: "checkmark")
            }
        }
        .frame(maxWidth: .infinity, alignment: .trailing)
    }

    func rolePicker() -> some View {
        Picker("New member role", selection: $selectedRole) {
            ForEach(GroupMemberRole.allCases) { role in
                if role <= groupInfo.membership.memberRole {
                    Text(role.text)
                }
            }
        }
    }

    func contactCheckView(_ contact: Contact) -> some View {
        let checked = selectedContacts.contains(contact.apiId)
        return Button {
            if checked {
                selectedContacts.remove(contact.apiId)
            } else {
                selectedContacts.insert(contact.apiId)
            }
        } label: {
            HStack{
                ProfileImage(imageStr: contact.image)
                    .frame(width: 30, height: 30)
                    .padding(.trailing, 2)
                Text(ChatInfo.direct(contact: contact).chatViewName)
                    .foregroundColor(.primary)
                    .lineLimit(1)
                Spacer()
                Image(systemName: checked ? "checkmark.circle.fill": "circle")
                    .foregroundColor(checked ? .accentColor : Color(uiColor: .tertiaryLabel))
            }
        }
    }
}

struct AddGroupMembersView_Previews: PreviewProvider {
    static var previews: some View {
        @State var showSheet = true
        return AddGroupMembersView(chat: Chat(chatInfo: ChatInfo.sampleData.group), groupInfo: GroupInfo.sampleData, showSheet: $showSheet)
    }
}
