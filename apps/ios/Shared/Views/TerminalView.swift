//
//  TerminalView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let terminalFont = Font.custom("Menlo", size: 16)

struct TerminalView: View {
    @EnvironmentObject var chatModel: ChatModel
    @State var inProgress: Bool = false
    @FocusState private var keyboardVisible: Bool

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
                                        .padding()
                                }
                            } label: {
                                HStack {
                                    Text(item.id.formatted(date: .omitted, time: .standard))
                                    Text(item.label)
                                        .frame(maxWidth: .infinity, maxHeight: 30, alignment: .leading)
                                }
                                .font(terminalFont)
                                .padding(.horizontal)
                            }
                        }
                        .onAppear { scrollToBottom(proxy) }
                        .onChange(of: chatModel.terminalItems.count) { _ in scrollToBottom(proxy) }
                        .onChange(of: keyboardVisible) { _ in
                            if keyboardVisible {
                                DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
                                    scrollToBottom(proxy, animation: .easeInOut(duration: 1))
                                }
                            }
                        }
                    }
                }

                Spacer()

                SendMessageView(
                    sendMessage: sendMessage,
                    inProgress: inProgress,
                    keyboardVisible: $keyboardVisible
                )
            }
        }
        .navigationViewStyle(.stack)
        .navigationTitle("Chat console")
    }

    func scrollToBottom(_ proxy: ScrollViewProxy, animation: Animation = .default) {
        if let id = chatModel.terminalItems.last?.id {
            withAnimation(animation) {
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
