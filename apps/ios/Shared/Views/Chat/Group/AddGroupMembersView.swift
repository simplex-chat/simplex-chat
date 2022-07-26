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
    @Binding var chatViewSheet: ChatViewSheet?
    @State private var contactsToAdd: [Contact] = []
    @State private var selectedContacts = Set<Int64>()

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            ChatInfoToolbar(chat: chat, imageSize: 48)
            .padding()
            .frame(maxWidth: .infinity, alignment: .center)
            .background(Color(uiColor: .quaternarySystemFill))
            if (contactsToAdd.isEmpty) {
                Text("No contacts to add")
                    .foregroundColor(.secondary)
                    .padding()
                    .frame(maxWidth: .infinity, alignment: .center)
            } else {
                HStack {
                    let count = selectedContacts.count
                    Button {
                        Task {
                            for contactId in selectedContacts {
                                await addMember(groupId: chat.chatInfo.apiId, contactId: contactId)
                            }
                            chatViewSheet = nil
                        }
                    } label: {
                        Label(
                            count > 0 ? "Invite \(count) member(s)" : "Invite new members",
                            systemImage: "plus")
                    }
                    .disabled(count < 1)
                    Spacer()
                    if count > 0 {
                        Button {
                            selectedContacts.removeAll()
                        } label: {
                            Label("Clear", systemImage: "multiply")
                        }
                    }
                }
                .padding(.horizontal)
                .padding(.bottom, 12)
                .frame(maxWidth: .infinity, alignment: .leading)
                .background(Color(uiColor: .quaternarySystemFill))
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
        let memberContactIds = await apiListMembers(chat.chatInfo.apiId)
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
        return AddGroupMembersView(chat: Chat(chatInfo: ChatInfo.sampleData.group), chatViewSheet: Binding($chatViewSheet))
    }
}
