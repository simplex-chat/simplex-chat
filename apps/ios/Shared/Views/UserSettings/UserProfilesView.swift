//
// Created by Avently on 17.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserProfilesView: View {
    @EnvironmentObject private var m: ChatModel
    @Environment(\.editMode) private var editMode
    @State private var showDeleteConfirmation = false
    @State private var userToDelete: Int?
    @State private var alert: UserProfilesAlert?

    private enum UserProfilesAlert: Identifiable {
        case deleteUser(index: Int, delSMPQueues: Bool)
        case activateUserError(error: String)
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case let .deleteUser(index, delSMPQueues): return "deleteUser \(index) \(delSMPQueues)"
            case let .activateUserError(err): return "activateUserError \(err)"
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
                        showDeleteConfirmation = true
                        userToDelete = i
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
        .confirmationDialog("Delete chat profile?", isPresented: $showDeleteConfirmation, titleVisibility: .visible) {
            deleteModeButton("Profile and server connections", true)
            deleteModeButton("Local profile data only", false)
        }
        .alert(item: $alert) { alert in
            switch alert {
            case let .deleteUser(index, delSMPQueues):
                return Alert(
                    title: Text("Delete user profile?"),
                    message: Text("All chats and messages will be deleted - this cannot be undone!"),
                    primaryButton: .destructive(Text("Delete")) {
                        removeUser(index, delSMPQueues)
                    },
                    secondaryButton: .cancel()
                )
            case let .activateUserError(error: err):
                return Alert(
                    title: Text("Error switching profile!"),
                    message: Text(err)
                )
            case let .error(title, error):
                return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    private func deleteModeButton(_ title: LocalizedStringKey, _ delSMPQueues: Bool) -> some View {
        Button(title, role: .destructive) {
            if let i = userToDelete {
                alert = .deleteUser(index: i, delSMPQueues: delSMPQueues)
            }
        }
    }

    private func removeUser(_ index: Int, _ delSMPQueues: Bool) {
        if index >= m.users.count { return }
        do {
            let u = m.users[index].user
            if u.activeUser {
                if let newActive = m.users.first(where: { !$0.user.activeUser }) {
                    try changeActiveUser_(newActive.user.userId)
                    try deleteUser(u.userId)
                }
            } else {
                try deleteUser(u.userId)
            }
        } catch let error {
            let a = getErrorAlert(error, "Error deleting user profile")
            alert = .error(title: a.title, error: a.message)
        }

        func deleteUser(_ userId: Int64) throws {
            try apiDeleteUser(userId, delSMPQueues)
            m.users.remove(at: index)
        }
    }

    private func userView(_ user: User) -> some View {
        Button {
            do {
                try changeActiveUser_(user.userId)
            } catch {
                alert = .activateUserError(error: responseError(error))
            }
        } label: {
            HStack {
                ProfileImage(imageStr: user.image, color: Color(uiColor: .tertiarySystemFill))
                    .frame(width: 44, height: 44)
                    .padding(.vertical, 4)
                    .padding(.trailing, 12)
                Text(user.chatViewName)
                Spacer()
                Image(systemName: "checkmark")
                    .foregroundColor(user.activeUser ? .primary : .clear)
            }
        }
        .disabled(user.activeUser)
        .foregroundColor(.primary)
        .deleteDisabled(m.users.count <= 1)
    }
}

struct UserProfilesView_Previews: PreviewProvider {
    static var previews: some View {
        UserProfilesView()
    }
}
