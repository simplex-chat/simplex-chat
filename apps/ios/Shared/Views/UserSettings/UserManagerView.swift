//
// Created by Avently on 17.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserManagerView: View {
    @EnvironmentObject private var m: ChatModel
    @Environment(\.editMode) private var editMode
    @State private var users: [UserInfo] = []
    @State private var selectedUser: Int? = nil
    @State private var showAddUser: Bool? = false
    @State private var showScanSMPServer = false
    @State private var testing = false

    var body: some View {
        ZStack {
            userManagerView()
            .onAppear {
                Task {
                    users = await listUsers()
                }
            }
        }
    }

    private func userManagerView() -> some View {
        List {
            Section("Your profiles") {
                ForEach(Array($users.enumerated()), id: \.0) { i, userInfo in
                    userManagerView(userInfo, index: i)
                        .deleteDisabled(userInfo.wrappedValue.user.activeUser)
                }
                .onDelete { indexSet in
                    do {
                        try apiDeleteUser(users[indexSet.first!].user.userId)
                        users.remove(atOffsets: indexSet)
                    } catch {
                        fatalError("Failed to delete user: \(responseError(error))")
                    }
                }
                NavigationLink(destination: CreateProfile(), tag: true, selection: $showAddUser) {
                    Text("Add profile…")
                }
            }
        }
        .toolbar { EditButton() }
    }

    private func userManagerView(_ userBinding: Binding<UserInfo>, index: Int) -> some View {
        let user = userBinding.wrappedValue.user
        return NavigationLink(tag: index, selection: $selectedUser) {
//            UserPrefs(user: userBinding, index: index)
//            .navigationBarTitle(user.chatViewName)
//            .navigationBarTitleDisplayMode(.large)
        } label: {
            Text(user.chatViewName)
        }
    }
}

struct UserManagerView_Previews: PreviewProvider {
    static var previews: some View {
        UserManagerView()
    }
}
