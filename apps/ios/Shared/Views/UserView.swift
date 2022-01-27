//
//  UserView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct UserView: View {
    @EnvironmentObject var chatModel: ChatModel
    var user: User

//    private let controller: chat_ctrl
//
//    var chatStore: chat_store
    
//    @State private var logbuffer = [String]()
//    @State private var chatcmd: String = ""
//    @State private var chatlog: String = ""
//    @FocusState private var focused: Bool
    
//    func addLine(line: String) {
//        print(line)
//        logbuffer.append(line)
//        if(logbuffer.count > 50) { _ = logbuffer.dropFirst() }
//        chatlog = logbuffer.joined(separator: "\n")
//    }
    
    var body: some View {
        if chatModel.userChats.isEmpty {
            VStack {
                Text("Hello chat")
                Text("Active user: \(user.localDisplayName) (\(user.profile.fullName))")
            }
        } else {
            ChatList()
        }
        
        
        
//        DispatchQueue.global().async {
//            while(true) {
//                let msg = String.init(cString: chat_recv_msg(controller))
//
//                DispatchQueue.main.async {
//                    addLine(line: msg)
//                }
//            }
//        }
        
//        return VStack {
//            ScrollView {
//                VStack(alignment: .leading) {
//                    HStack { Spacer() }
//                    Text(chatlog)
//                        .lineLimit(nil)
//                        .font(.system(.body, design: .monospaced))
//                }
//                .frame(maxWidth: .infinity)
//            }
//
//            TextField("Chat command", text: $chatcmd)
//                .focused($focused)
//                .onSubmit {
//                    print(chatcmd)
//                    var cCmd = chatcmd.cString(using: .utf8)!
////                    print(String.init(cString: chat_send_cmd(controller, &cCmd)))
//                }
//                .textInputAutocapitalization(.never)
//                .disableAutocorrection(true)
//                .padding()
//        }
    }

}

//struct UserView_Previews: PreviewProvider {
//    static var previews: some View {
//        UserView()
//    }
//}
