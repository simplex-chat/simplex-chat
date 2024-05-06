//
//  ContactsView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 06.05.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactsView: View {
    @EnvironmentObject var chatModel: ChatModel

    @State private var searchMode = false
    @FocusState private var searchFocussed
    @State private var searchText = ""

    @AppStorage(DEFAULT_ONE_HAND_UI) private var oneHandUI = true

    var body: some View {
        if #available(iOS 16.0, *) {
            viewBody.scrollDismissesKeyboard(.immediately)
        } else {
            viewBody
        }
    }

    private var viewBody: some View {
        VStack {
            contactList
        }
        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
        .listStyle(.plain)
    }

    @ViewBuilder private var contactList: some View {
        let contacts = chatModel.chats.compactMap{ $0.chatInfo.contact }
        let filteredContacts = oneHandUI ? filteredContacts(contacts).reversed() : filteredContacts(contacts)
        ZStack {
            VStack {
                List {
                    if !contacts.isEmpty {
                        ContactsSearchBar(
                            searchMode: $searchMode,
                            searchFocussed: $searchFocussed,
                            searchText: $searchText
                        )
                        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                        .listRowSeparator(.hidden)
                        .frame(maxWidth: .infinity)
                    }
                    ForEach(filteredContacts, id: \.id) { contact in
                        ContactListNavLink(contact: contact)
                            .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                            .padding(.trailing, -16)
                            .disabled(chatModel.chatRunning != true || chatModel.deletedChats.contains(contact.id))
                    }
                    .offset(x: -8)
                }
            }
            if filteredContacts.isEmpty && !contacts.isEmpty {
                Text("No filtered contacts")
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    .foregroundColor(.secondary)
            } else if contacts.isEmpty {
                Text("No contacts")
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    .foregroundColor(.secondary)
            }
        }
    }

    private func filteredContacts(_ contacts: [Contact]) -> [Contact] {
        let s = searchString()
        return contacts.filter { contact in
            return s == ""
            ? true
            : (viewNameContains(contact, s) ||
               contact.profile.displayName.localizedLowercase.contains(s) ||
               contact.fullName.localizedLowercase.contains(s))
        }
        .sorted{ $0.displayName.lowercased() < $1.displayName.lowercased() }

        func searchString() -> String {
            searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
        }

        func viewNameContains(_ contact: Contact, _ s: String) -> Bool {
            contact.chatViewName.localizedLowercase.contains(s)
        }
    }
}

struct ContactsSearchBar: View {
    @EnvironmentObject var m: ChatModel
    @Binding var searchMode: Bool
    @FocusState.Binding var searchFocussed: Bool
    @Binding var searchText: String

    var body: some View {
        VStack(spacing: 12) {
            HStack(spacing: 12) {
                HStack(spacing: 4) {
                    Image(systemName: "magnifyingglass")
                    TextField("Search", text: $searchText)
                        .focused($searchFocussed)
                        .frame(maxWidth: .infinity)
                    if !searchText.isEmpty {
                        Image(systemName: "xmark.circle.fill")
                            .onTapGesture {
                                searchText = ""
                            }
                    }
                }
                .padding(EdgeInsets(top: 7, leading: 7, bottom: 7, trailing: 7))
                .foregroundColor(.secondary)
                .background(Color(.tertiarySystemFill))
                .cornerRadius(10.0)

                if searchFocussed {
                    Text("Cancel")
                        .foregroundColor(.accentColor)
                        .onTapGesture {
                            searchText = ""
                            searchFocussed = false
                        }
                }
            }
        }
        .onChange(of: searchFocussed) { sf in
            withAnimation { searchMode = sf }
        }
    }
}

struct ContactsView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chats = [
            Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: []
            )
        ]
        return Group {
            ContactsView()
                .environmentObject(chatModel)
            ContactsView()
                .environmentObject(ChatModel())
        }
    }
}
