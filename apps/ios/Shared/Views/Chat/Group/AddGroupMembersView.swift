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
    var groupId: Int64
    @Binding var chatViewSheet: ChatViewSheet?
    @State private var contactsToAdd: [Contact] = []
    @State private var selectedContacts = Set<Int64>()

    var body: some View {
        VStack {
            Text("Invite to group")
                .font(.headline)
                .padding()
            if (contactsToAdd.isEmpty) {
                Text("No contacts to add")
                    .foregroundColor(.secondary)
            } else {
                HStack {
                    let count = selectedContacts.count
                    if count == 0 {
                        Text("Select new member(s)")
                    } else {
                        Button {
                            Task {
                                for contactId in selectedContacts {
                                    await addMember(groupId: groupId, contactId: contactId)
                                }
                                chatViewSheet = nil
                            }
                        } label: {
                            Label("Invite \(count) member(s)", systemImage: "checkmark")
                        }
                        Spacer()
                        Button {
                            selectedContacts.removeAll()
                        } label: {
                            Label("Clear", systemImage: "multiply")
                        }
                    }
                }
                .padding()
                .frame(maxWidth: .infinity, maxHeight: 36, alignment: .leading)
                List(contactsToAdd) { contact in
                    contactCheckView(contact)
                        .listRowBackground(Color.clear)
                }
                .listStyle(.plain)
            }
        }
        .frame(maxHeight: .infinity, alignment: .top)
        .task {
            contactsToAdd = await getContactsToAdd()
        }
    }

    func getContactsToAdd() async -> [Contact] {
        let memberContactIds = await apiListMembers(groupId: groupId)
            .compactMap{ $0.memberContactId }
        return chatModel.chats
            .compactMap{ $0.chatInfo.contact }
            .filter{ !memberContactIds.contains($0.apiId) }
            .sorted{ $0.displayName.lowercased() < $1.displayName.lowercased() }
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
                    .lineLimit(1)
                Spacer()
                Image(systemName: checked ? "checkmark.circle.fill": "circle")
                    .foregroundColor(checked ? .accentColor : .secondary)
            }
        }
    }
}

struct AddGroupMembersView_Previews: PreviewProvider {
    static var previews: some View {
        @State var chatViewSheet = ChatViewSheet.chatInfo
        return AddGroupMembersView(groupId: 1, chatViewSheet: Binding($chatViewSheet))
    }
}
