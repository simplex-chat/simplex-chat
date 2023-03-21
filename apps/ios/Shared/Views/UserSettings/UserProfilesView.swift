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
    @State private var userToDelete: UserInfo?
    @State private var alert: UserProfilesAlert?
    @State private var authorized = !UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA)
    @State private var searchTextOrPassword = ""
    @State private var showProfilePassword = false
    @State private var selectedUser: User?

    private enum UserProfilesAlert: Identifiable {
        case deleteUser(userInfo: UserInfo, delSMPQueues: Bool)
        case cantDeleteLastUser
//        case cantHideLastUser
        case activateUserError(error: String)
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case let .deleteUser(userInfo, delSMPQueues): return "deleteUser \(userInfo.user.userId) \(delSMPQueues)"
            case .cantDeleteLastUser: return "cantDeleteLastUser"
//            case let .cantHideLastUser: return "cantHideLastUser"
            case let .activateUserError(err): return "activateUserError \(err)"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        if authorized {
            userProfilesView()
        } else {
            Button(action: runAuth) { Label("Unlock", systemImage: "lock") }
            .onAppear(perform: runAuth)
        }
    }

    private func runAuth() { authorize(NSLocalizedString("Open user profiles", comment: "authentication reason"), $authorized) }

    private func userProfilesView() -> some View {
        List {
            Section {
                let users = filteredUsers()
                ForEach(users) { u in
                    userView(u.user)
                }
                .onDelete { indexSet in
                    if let i = indexSet.first {
                        if m.users.count > 1 && (m.users[i].user.hidden || visibleUsersCount > 1) {
                            showDeleteConfirmation = true
                            userToDelete = users[i]
                        } else {
                            alert = .cantDeleteLastUser
                        }
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
        .navigationTitle("Your chat profiles")
        .searchable(text: $searchTextOrPassword)
        .autocorrectionDisabled(true)
        .textInputAutocapitalization(.never)
        .confirmationDialog("Delete chat profile?", isPresented: $showDeleteConfirmation, titleVisibility: .visible) {
            deleteModeButton("Profile and server connections", true)
            deleteModeButton("Local profile data only", false)
        }
        .alert(item: $alert) { alert in
            switch alert {
            case let .deleteUser(userInfo, delSMPQueues):
                return Alert(
                    title: Text("Delete user profile?"),
                    message: Text("All chats and messages will be deleted - this cannot be undone!"),
                    primaryButton: .destructive(Text("Delete")) {
                        Task { await removeUser(userInfo, delSMPQueues) }
                    },
                    secondaryButton: .cancel()
                )
            case .cantDeleteLastUser:
                return Alert(
                    title: Text("Can't delete user profile!"),
                    message: m.users.count > 1
                            ? Text("There should be at least one visible user profile.")
                            : Text("There should be at least use user profile.")
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
        .sheet(item: $selectedUser) { user in
            HiddenProfileView(user: user)
        }
    }

    private func filteredUsers() -> [UserInfo] {
        let s = searchTextOrPassword.trimmingCharacters(in: .whitespaces)
        let lower = s.localizedLowercase
        return m.users.filter { u in
            if let ph = u.user.viewPwdHash {
                return u.user.activeUser || (s != "" && chatPasswordHash(s, ph.salt) == ph.hash)
            }
            return s == "" || u.user.chatViewName.localizedLowercase.contains(lower)
        }
    }

    private var visibleUsersCount: Int {
        m.users.filter({ u in !u.user.hidden }).count
    }

    private func userViewPassword(_ user: User) -> String? {
        user.activeUser || !user.hidden ? nil : searchTextOrPassword
    }

    private func deleteModeButton(_ title: LocalizedStringKey, _ delSMPQueues: Bool) -> some View {
        Button(title, role: .destructive) {
            if let userInfo = userToDelete {
                alert = .deleteUser(userInfo: userInfo, delSMPQueues: delSMPQueues)
            }
        }
    }

    private func removeUser(_ userInfo: UserInfo, _ delSMPQueues: Bool) async {
        do {
            let u = userInfo.user
            if u.activeUser {
                if let newActive = m.users.first(where: { u in !u.user.activeUser && !u.user.hidden }) {
                    try await changeActiveUser_(newActive.user.userId, viewPwd: nil)
                    try await deleteUser(u)
                }
            } else {
                try await deleteUser(u)
            }
        } catch let error {
            let a = getErrorAlert(error, "Error deleting user profile")
            alert = .error(title: a.title, error: a.message)
        }

        func deleteUser(_ user: User) async throws {
            try await apiDeleteUser(user.userId, delSMPQueues, viewPwd: userViewPassword(user))
            await MainActor.run { withAnimation { m.removeUser(user) } }
        }
    }

    private func userView(_ user: User) -> some View {
        Button {
            Task {
                do {
                    try await changeActiveUser_(user.userId, viewPwd: userViewPassword(user))
                } catch {
                    await MainActor.run { alert = .activateUserError(error: responseError(error)) }
                }
            }
        } label: {
            HStack {
                ProfileImage(imageStr: user.image, color: Color(uiColor: .tertiarySystemFill))
                    .frame(width: 44, height: 44)
                    .padding(.vertical, 4)
                    .padding(.trailing, 12)
                Text(user.chatViewName)
                Spacer()
                if user.activeUser {
                    Image(systemName: "checkmark").foregroundColor(.primary)
                } else if user.hidden {
                    Image(systemName: "lock").foregroundColor(.secondary)
                } else if user.showNtfs == false {
                    Image(systemName: "speaker.slash").foregroundColor(.secondary)
                } else {
                    Image(systemName: "checkmark").foregroundColor(.clear)
                }
            }
        }
        .disabled(user.activeUser)
        .foregroundColor(.primary)
        .deleteDisabled(m.users.count <= 1)
        .swipeActions(edge: .leading, allowsFullSwipe: true) {
            if user.hidden {
                Button("Unhide") {
                    setUserPrivacy(user) { try await apiUnhideUser(user.userId, viewPwd: userViewPassword(user)) }
                }
                .tint(.green)
            } else {
                if visibleUsersCount > 1 {
                    Button("Hide") {
                        selectedUser = user
                    }
                }
                Group {
                    if user.showNtfs == true {
                        Button("Mute") {
                            setUserPrivacy(user) { try await apiMuteUser(user.userId, viewPwd: userViewPassword(user)) }
                        }
                    } else {
                        Button("Unmute") {
                            setUserPrivacy(user) { try await apiUnmuteUser(user.userId, viewPwd: userViewPassword(user)) }
                        }
                    }
                }
                .tint(.accentColor)
            }
        }
    }

    private func setUserPrivacy(_ user: User, _ api: @escaping () async throws -> User) {
        Task {
            do {
                let u = try await api()
                await MainActor.run { withAnimation { m.updateUser(u) } }
            } catch let error {
                let a = getErrorAlert(error, "Error updating user privacy")
                alert = .error(title: a.title, error: a.message)
            }
        }
    }
}

public func chatPasswordHash(_ pwd: String, _ salt: String) -> String {
    var cPwd = pwd.cString(using: .utf8)!
    var cSalt = salt.cString(using: .utf8)!
    let cHash  = chat_password_hash(&cPwd, &cSalt)!
    let hash = fromCString(cHash)
    return hash
}

struct UserProfilesView_Previews: PreviewProvider {
    static var previews: some View {
        UserProfilesView()
    }
}
