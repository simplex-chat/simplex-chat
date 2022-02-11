//
//  SettingsView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

let simplexTeamURL = URL(string: "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")!

struct SettingsView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Binding var showSettings: Bool

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

                Section("Help") {
                    NavigationLink {
                        VStack(alignment: .leading, spacing: 10) {
                            Text("Welcome \(user.displayName)!")
                                .font(.largeTitle)
                                .padding(.leading)
                            Divider()
                            ChatHelp(showSettings: $showSettings)
                        }
                        .frame(maxHeight: .infinity, alignment: .top)
                    } label: {
                        HStack {
                            Image(systemName: "questionmark.circle")
                                .padding(.trailing, 8)
                            Text("How to use SimpleX Chat")
                        }
                    }
                    HStack {
                        Image(systemName: "number")
                            .padding(.trailing, 8)
                        Button {
                            showSettings = false
                            DispatchQueue.main.async {
                                UIApplication.shared.open(simplexTeamURL)
                            }
                        } label: {
                            Text("Get help & advice via chat")
                        }
                    }
                    HStack {
                        Image(systemName: "envelope")
                            .padding(.trailing, 4)
                        Text("[Ask questions via email](mailto:chat@simplex.chat)")
                    }
                }

                Section("Develop") {
                    NavigationLink {
                        TerminalView()
                    } label: {
                        HStack {
                            Image(systemName: "terminal")
                                .frame(maxWidth: 24)
                                .padding(.trailing, 8)
                            Text("Chat console")
                        }
                    }
                    HStack {
                        Image("github")
                            .resizable()
                            .frame(width: 24, height: 24)
                            .padding(.trailing, 8)
                        Text("Install [SimpleX Chat for terminal](https://github.com/simplex-chat/simplex-chat)")
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
        chatModel.currentUser = User.sampleData
        @State var showSettings = false

        return SettingsView(showSettings: $showSettings)
            .environmentObject(chatModel)
    }
}
