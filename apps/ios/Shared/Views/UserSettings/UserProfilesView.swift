//
// Created by Avently on 17.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserProfilesView: View {
    @EnvironmentObject private var m: ChatModel
    @Environment(\.editMode) private var editMode
    @State private var alert: UserProfilesAlert?

    private enum UserProfilesAlert: Identifiable {
        case deleteUser(index: Int)
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case let .deleteUser(index): return "deleteUser \(index)"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        List {
            Section {
                ForEach(m.users) { u in
                    userView(u.user)
                }
                .onDelete { indexSet in
                    if let i = indexSet.first {
                        alert = .deleteUser(index: i)
                    }
                }

                NavigationLink {
                    CreateProfile()
                } label: {
                    Label("Add profile", systemImage: "plus")
                }
                .frame(height: 44)
                .padding(.vertical, 4)
            } footer: {
                Text("Your chat profiles are stored locally, only on your device.")
            }
        }
        .toolbar { EditButton() }
        .alert(item: $alert) { alert in
            switch alert {
            case let .deleteUser(index):
                return Alert(
                    title: Text("Delete user profile?"),
                    message: Text("All chats and messages will be deleted - this cannot be undone!"),
                    primaryButton: .destructive(Text("Delete")) {
                        removeUser(index: index)
                    },
                    secondaryButton: .cancel()
                )
            case let .error(title, error):
                return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    private func removeUser(index: Int) {
        do {
            try apiDeleteUser(m.users[index].user.userId)
            m.users.remove(at: index)
        } catch let error {
            let a = getErrorAlert(error, "Error deleting user profile")
            alert = .error(title: a.title, error: a.message)
        }
    }

    @ViewBuilder private func userView(_ user: User) -> some View {
        Button {
            if !user.activeUser {
                changeActiveUser(user.userId)
            }
        } label: {
            HStack {
                ProfileImage(imageStr: user.image)
                    .frame(width: 44, height: 44)
                    .padding(.vertical, 4)
                    .padding(.trailing, 12)
                Text(user.chatViewName)
                Spacer()
                Image(systemName: "checkmark")
                    .foregroundColor(user.activeUser ? .primary : .clear)
            }
        }
        .foregroundColor(.primary)
        .deleteDisabled(user.activeUser)
    }
}

struct UserProfilesView_Previews: PreviewProvider {
    static var previews: some View {
        UserProfilesView()
    }
}
