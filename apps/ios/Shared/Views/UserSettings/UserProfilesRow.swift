//
//  UserProfilesRow.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 29/01/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct UserProfilesRow: View {
    @EnvironmentObject var m: ChatModel

    var body: some View {
        let users = m.users.filter { u in !u.user.activeUser }
        if users.count > 0 {
            NavigationLink {
                UserProfilesView()
                    .navigationTitle("Your chat profiles")
            } label: {
                settingsRow("person.crop.rectangle.stack") {
                    HStack(spacing: 16) {
                        Text("Your profiles")
                        ScrollView(.horizontal, showsIndicators: false) {
                            HStack(spacing: 16) {
                                ForEach(users) { u in
                                    ZStack(alignment: .topLeading) {
                                        ProfileImage(imageStr: u.user.image, color: Color(uiColor: .tertiarySystemFill))
                                            .frame(width: 40, height: 40)
                                            .onTapGesture { changeActiveUser(u.user.userId) }
                                            .padding(.vertical, 12)
                                        if u.unreadCount > 0 {
                                            unreadCounter(u.unreadCount)
                                                .padding(.leading, 28)
                                                .padding(.top, 6)
                                        }
                                    }
                                }
//                            NavigationLink {
//                                CreateProfile()
//                            } label: {
//                                Image(systemName: "plus")
//                                    .resizable()
//                                    .scaledToFit()
//                                    .frame(maxWidth: 16, maxHeight: 16, alignment: .center)
//                                    .foregroundColor(.secondary)
//                                    .frame(width: 36, height: 36)
//                            }
                            }
                        }
                        .padding(.trailing, 16)
                    }
                }
            }
            .listRowInsets(EdgeInsets(top: 0, leading: 20, bottom: 0, trailing: 20))
        } else {
            NavigationLink {
                CreateProfile()
            } label: {
                settingsRow("plus") { Text("Add chat profile") }
            }
        }
    }
}

struct UserProfilesRow_Previews: PreviewProvider {
    static var previews: some View {
        UserProfilesRow()
    }
}
