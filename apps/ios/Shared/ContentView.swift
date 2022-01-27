//
//  ContentView.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

//import SwiftUI

//struct ContentView: View {
//    @State var messages: [String] = ["Start session:"]
//    @State var text: String = ""
//
//    func sendMessage() {
//    }
//
//    var body: some View {
//        VStack {
//            ScrollView {
//                LazyVStack {
//                    ForEach(messages, id: \.self) { msg in
//                        MessageView(message: msg, sent: false)
//                    }
//                }
//                .padding(10)
//            }
//            .frame(minWidth: 0,
//                   maxWidth: .infinity,
//                   minHeight: 0,
//                   maxHeight: .infinity,
//                   alignment: .topLeading)
//            HStack {
//                TextField("Message...", text: $text)
//                   .textFieldStyle(RoundedBorderTextFieldStyle())
//                   .frame(minHeight: CGFloat(30))
//                Button(action: sendMessage) {
//                    Text("Send")
//                }.disabled(text.isEmpty)
//            }
//            .frame(minHeight: CGFloat(30))
//            .padding()
//        }
//    }
//}

import SwiftUI

struct ContentView: View {
    @EnvironmentObject var chatModel: ChatModel
    
//    var chatStore: chat_store
//    private let controller: chat_controller

//    init(chatStore: chat_store) {
//        self.chatStore = chatStore
//    }
    

//    @State private var logbuffer = [String]()
//    @State private var chatcmd: String = ""
//    @State private var chatlog: String = ""
//    @FocusState private var focused: Bool
//    
//    func addLine(line: String) {
//        print(line)
//        logbuffer.append(line)
//        if(logbuffer.count > 50) { _ = logbuffer.dropFirst() }
//        chatlog = logbuffer.joined(separator: "\n")
//    }
    
    var body: some View {
        if let user = chatModel.currentUser {
            ChatListView(user: user)
        } else {
            WelcomeView()
        }

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
//                    print(String.init(cString: chat_send_cmd(controller, &cCmd)))
//                }
//                .textInputAutocapitalization(.never)
//                .disableAutocorrection(true)
//                .padding()
//        }
    }    
}


//struct ContentView_Previews: PreviewProvider {
//    static var previews: some View {
//        ContentView(text: "Hello!")
//    }
//}
