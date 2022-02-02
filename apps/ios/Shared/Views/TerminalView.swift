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
                }
            }
            .navigationViewStyle(.stack)

            Spacer()

            SendMessageView(sendMessage: sendMessage, inProgress: inProgress)
        }
    }
    
    func sendMessage(_ cmdStr: String) {
        let cmd = ChatCommand.string(cmdStr)
        chatModel.terminalItems.append(.cmd(Date.now, cmd))

        DispatchQueue.global().async {
            inProgress = true
            do {
                let r = try chatSendCmd(cmd)
                DispatchQueue.main.async {
                    chatModel.terminalItems.append(.resp(Date.now, r))
                }
            } catch {
                print(error)
            }
            inProgress = false
        }
    }
}

struct TerminalView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.terminalItems = [
            .resp(Date.now, ChatResponse.response(type: "contactSubscribed", json: "{}")),
            .resp(Date.now, ChatResponse.response(type: "newChatItem", json: "{}"))
        ]
        return NavigationView {
            TerminalView()
                .environmentObject(chatModel)
        }

    }
}
