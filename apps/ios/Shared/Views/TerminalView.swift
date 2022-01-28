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
                    ForEach(chatModel.chatResponses, id: \.self) { cr in
                        Text(cr.responseType)
                    }
                }
            }
            HStack {
                TextField("Message...", text: $command)
                   .textFieldStyle(RoundedBorderTextFieldStyle())
                   .frame(minHeight: CGFloat(30))
                Button(action: sendMessage) {
                    Text("Send")
                }.disabled(command.isEmpty)
            }
            .frame(minHeight: CGFloat(30))
            .padding()
        }
    }
    
    func sendMessage() {
        DispatchQueue.global().async {
            let cmd: String = self.$command.wrappedValue
            inProgress = true
            command = ""
            processAPIResponse(chatModel, chatSendCmd(ChatCommand.string(cmd)))
            inProgress = false
        }
    }
}

struct TerminalView_Previews: PreviewProvider {
    static var previews: some View {
        TerminalView()
    }
}
