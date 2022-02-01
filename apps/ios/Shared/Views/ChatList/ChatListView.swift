//
//  ChatListView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatListView: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var chatId: String?
    @State private var connectAlert = false
    @State private var connectError: Error?

    var user: User

    var body: some View {
        VStack {
//            if chatModel.chats.isEmpty {
//                VStack {
//                    Text("Hello chat")
//                    Text("Active user: \(user.localDisplayName) (\(user.profile.fullName))")
//                }
//            }

            NavigationView {
                GeometryReader { geometry in
                    List {
                        NavigationLink {
                            TerminalView()
                        } label: {
                            Text("Terminal")
                        }

                        ForEach(chatModel.chatPreviews) { chatPreview in
                            ChatListNavLink(
                                chatId: $chatId,
                                chatPreview: chatPreview,
                                width: geometry.size.width
                            )
                        }
                    }
                    .padding(0)
                    .offset(x: -8)
                    .listStyle(.plain)
                    .toolbar { ChatListToolbar(width: geometry.size.width) }
                    .navigationBarTitleDisplayMode(.inline)
                    .alert(isPresented: $connectAlert) { connectionErrorAlert() }
                }
            }
            .alert(isPresented: $chatModel.connectViaUrl) { connectViaUrlAlert() }
        }
    }

    private func connectViaUrlAlert() -> Alert {
        if let url = chatModel.appOpenUrl {
            var path = url.path
            if (path == "/contact" || path == "/invitation") {
                path.removeFirst()
                let link = url.absoluteString.replacingOccurrences(of: "///\(path)", with: "/\(path)")
                return Alert(
                    title: Text("Connect via \(path) link?"),
                    message: Text("Your profile will be sent to the contact that you received this link from: \(link)"),
                    primaryButton: .default(Text("Connect")) {
                        do {
                            try apiConnect(connReq: link)
                        } catch {
                            connectAlert = true
                            connectError = error
                            print(error)
                        }
                        chatModel.appOpenUrl = nil
                    }, secondaryButton: .cancel() {
                        chatModel.appOpenUrl = nil
                    }
                )
            } else {
                return Alert(title: Text("Error: URL not available"))
            }
        } else {
            return Alert(title: Text("Error: URL not available"))
        }
    }

    private func connectionErrorAlert() -> Alert {
        Alert(
            title: Text("Connection error"),
            message: Text(connectError?.localizedDescription ?? "")
        )
    }
}

struct ChatListView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chatPreviews = [
            Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "hello")]
            ),
            Chat(
                chatInfo: sampleGroupChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")]
            ),
            Chat(
                chatInfo: sampleContactRequestChatInfo,
                chatItems: []
            )

        ]
        return ChatListView(user: sampleUser)
            .environmentObject(chatModel)
    }
}
