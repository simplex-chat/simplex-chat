//
// Created by Avently on 17.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserProfilesView: View {
    @EnvironmentObject private var m: ChatModel
    @Environment(\.editMode) private var editMode
    @State private var selectedUser: Int? = nil
    @State private var showAddUser: Bool? = false

    var body: some View {
        List {
            Section("Your profiles") {
                ForEach(Array(m.users.enumerated()), id: \.0) { i, userInfo in
                    userProfileView(userInfo, index: i)
                        .deleteDisabled(userInfo.user.activeUser)
                }
                .onDelete { indexSet in
                    AlertManager.shared.showAlert(
                        Alert(
                            title: Text("Delete profile?"),
                            message: Text("All chats and messages will be deleted - this cannot be undone!"),
                            primaryButton: .destructive(Text("Delete")) {
                                removeUser(index: indexSet.first!)
                            },
                            secondaryButton: .cancel()
                        ))
                }
                NavigationLink(destination: CreateProfile(), tag: true, selection: $showAddUser) {
                    Text("Add profile…")
                }
            }
        }
        .toolbar { EditButton() }
    }

    private func removeUser(index: Int) {
        do {
            try apiDeleteUser(m.users[index].user.userId)
            var users = m.users
            users.remove(at: index)
            m.updateUsers(users)
        } catch {
            fatalError("Failed to delete user: \(responseError(error))")
        }
    }
    private func userProfileView(_ userBinding: UserInfo, index: Int) -> some View {
        let user = userBinding.user
        return NavigationLink(tag: index, selection: $selectedUser) {
//            UserPrefs(user: userBinding, index: index)
//            .navigationBarTitle(user.chatViewName)
//            .navigationBarTitleDisplayMode(.large)
        } label: {
            Text(user.chatViewName)
        }
    }
}

struct UserProfilesView_Previews: PreviewProvider {
    static var previews: some View {
        UserProfilesView()
    }
}
