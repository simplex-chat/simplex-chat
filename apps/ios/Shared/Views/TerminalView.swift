//
//  TerminalView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct TerminalView: View {
    @EnvironmentObject var chatModel: ChatModel
    @State var inProgress: Bool = false
    
    var body: some View {
        VStack {
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack {
                        ForEach(chatModel.terminalItems) { item in
                            NavigationLink {
                                ScrollView {
                                    Text(item.details)
                                        .textSelection(.enabled)
                                }
                            } label: {
                                Text(item.label)
                                .frame(width: 360, height: 30, alignment: .leading)
                            }
                        }
                        .onAppear { scrollToBottom(proxy) }
                        .onChange(of: chatModel.terminalItems.count) { _ in scrollToBottom(proxy) }
                    }
                }

                Spacer()

                SendMessageView(sendMessage: sendMessage, inProgress: inProgress)
            }
        }
        .navigationViewStyle(.stack)
        .navigationTitle("Chat console")
    }

    func scrollToBottom(_ proxy: ScrollViewProxy) {
        if let id = chatModel.terminalItems.last?.id {
            withAnimation {
                proxy.scrollTo(id, anchor: .bottom)
            }
        }
    }
    
    func sendMessage(_ cmdStr: String) {
        let cmd = ChatCommand.string(cmdStr)
        DispatchQueue.global().async {
            inProgress = true
            do {
                let _ = try chatSendCmd(cmd)
            } catch {
                logger.error("TerminalView.sendMessage chatSendCmd error: \(error.localizedDescription)")
            }
            inProgress = false
        }
    }
}

struct TerminalView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.terminalItems = [
            .resp(.now, ChatResponse.response(type: "contactSubscribed", json: "{}")),
            .resp(.now, ChatResponse.response(type: "newChatItem", json: "{}"))
        ]
        return NavigationView {
            TerminalView()
                .environmentObject(chatModel)
        }

    }
}
