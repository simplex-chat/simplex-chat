//
//  SettingsView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SettingsView: View {
    @EnvironmentObject var chatModel: ChatModel

    var body: some View {
        let user: User = chatModel.currentUser!

        return NavigationView {
            List {
                Section("You") {
                    NavigationLink {
                        UserProfile()
                            .navigationTitle("Your chat profile")
                    } label: {
                        HStack {
                            Image(systemName: "person.crop.circle")
                                .padding(.trailing, 8)
                            VStack(alignment: .leading) {
                                Text(user.profile.displayName)
                                    .fontWeight(.bold)
                                    .font(.title2)
                                Text(user.profile.fullName)
                            }
                        }
                    }
                    NavigationLink {
                        UserAddress()
                            .navigationTitle("Your chat address")
                    } label: {
                        HStack {
                            Image(systemName: "qrcode")
                                .padding(.trailing, 8)
                            Text("Your SimpleX contact address")
                        }
                    }
                }

                Section("Develop") {
                    NavigationLink {
                        TerminalView()
                    } label: {
                        Text("Chat console")
                    }
                }

//                Section("Your SimpleX servers") {
//
//                }
            }
            .navigationTitle("Your settings")
        }
    }
}

struct SettingsView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.currentUser = sampleUser
        return SettingsView()
            .environmentObject(chatModel)
    }
}
