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
    @State var command: String = ""
    @State var inProgress: Bool = false
    
    var body: some View {
        VStack {
            ScrollView {
                LazyVStack {
                    ForEach(chatModel.apiResponses) { r in
                        NavigationLink {
                            ScrollView {
                                Text(r.resp.details)
                            }
                        } label: {
                            Text(r.resp.responseType)
                            .frame(width: 360, height: 30, alignment: .leading)
                        }
                    }
                }
            }
            .navigationViewStyle(.stack)

            Spacer()

            HStack {
                TextField("Message...", text: $command)
                   .textFieldStyle(RoundedBorderTextFieldStyle())
                   .frame(minHeight: 30)
                Button(action: sendMessage) {
                    Text("Send")
                }.disabled(command.isEmpty)
            }
            .frame(minHeight: 30)
            .padding()
        }
    }
    
    func sendMessage() {
        DispatchQueue.global().async {
            let cmd: String = self.$command.wrappedValue
            inProgress = true
            command = ""
            chatSendCmd(chatModel, ChatCommand.string(cmd))
            inProgress = false
        }
    }
}

struct TerminalView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.apiResponses = [
            APIResponse(resp: ChatResponse.response(type: "contactSubscribed", json: "{}"), id: 1),
            APIResponse(resp: ChatResponse.response(type: "newChatItem", json: "{}"), id: 2)
        ]
        return NavigationView {
            TerminalView()
                .environmentObject(chatModel)
        }

    }
}
