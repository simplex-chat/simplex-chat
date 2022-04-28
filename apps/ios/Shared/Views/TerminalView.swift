//
//  TerminalView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let terminalFont = Font.custom("Menlo", size: 16)

private let maxItemSize: Int = 50000

struct TerminalView: View {
    @EnvironmentObject var chatModel: ChatModel
    @State var composeState: ComposeState = ComposeState()
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        VStack {
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack {
                        ForEach(chatModel.terminalItems) { item in
                            NavigationLink {
                                let s = item.details
                                ScrollView {
                                    Text(s.prefix(maxItemSize))
                                        .padding()
                                }
                                .toolbar {
                                    ToolbarItem(placement: .navigationBarTrailing) {
                                        Button { showShareSheet(items: [s]) } label: {
                                            Image(systemName: "square.and.arrow.up")
                                        }
                                    }
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
                    composeState: $composeState,
                    sendMessage: sendMessage,
                    keyboardVisible: $keyboardVisible
                )
                .padding(.horizontal, 12)
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
    
    func sendMessage() {
        let cmd = ChatCommand.string(composeState.message)
        DispatchQueue.global().async {
            Task {
                composeState.inProgress = true
                _ = await chatSendCmd(cmd)
                composeState.inProgress = false
            }
        }
        composeState = ComposeState()
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
