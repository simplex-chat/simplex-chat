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
    @State private var unaddedContacts: [Contact] = []
    @State private var selectedContactIds = Set<Int64>()

    var body: some View {
        VStack {
            Text("Contacts")
                .font(.headline)
                .padding()
            if (unaddedContacts.isEmpty) {
                Text("No unadded contacts")
                    .foregroundColor(.secondary)
            } else {
                HStack {
                    Button {
                        selectedContactIds.forEach { contactId in
                            Task {
                                await addMember(groupId: groupId, contactId: contactId)
                            }
                            chatViewSheet = nil
                        }
                    } label: {
                        Label("Add members", systemImage: "plus.circle.fill")
                    }
                    .padding()
                    Spacer()
                }
                .disabled(selectedContactIds.count < 1)
                List(unaddedContacts) { contact in
                    ContactCheckView(contact: contact, selectedContactIds: $selectedContactIds)
                        .listRowBackground(Color.clear)
                }
                .listStyle(.plain)
            }
        }
        .frame(maxHeight: .infinity, alignment: .top)
        .onAppear() {
            unaddedContacts = prepareUnaddedContactsList()
        }
    }

    func prepareUnaddedContactsList() -> [Contact] {
        let memberContactIds = apiListMembers(groupId: groupId)
            .compactMap{ $0.memberContactId }
        let unaddedContacts = chatModel.chats
            .compactMap{ $0.chatInfo.contact }
            .filter{ !memberContactIds.contains($0.apiId) }
            .sorted{ $0.displayName < $1.displayName }
        return unaddedContacts
    }

    struct ContactCheckView: View {
        var contact: Contact
        @State var checked: Bool = false
        @Binding var selectedContactIds: Set<Int64>

        func toggle() {
            checked.toggle()
            if checked {
                selectedContactIds.insert(contact.apiId)
            } else {
                selectedContactIds.remove(contact.apiId)
            }
        }

        var body: some View {
            Button(action: toggle){
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
}

struct AddGroupMembersView_Previews: PreviewProvider {
    static var previews: some View {
        @State var chatViewSheet = ChatViewSheet.chatInfo
        return AddGroupMembersView(groupId: 1, chatViewSheet: Binding($chatViewSheet))
    }
}
