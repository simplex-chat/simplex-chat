//
//  TerminalView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let terminalFont = Font.custom("Menlo", size: 16)

private let maxItemSize: Int = 50000

struct TerminalView: View {
    @EnvironmentObject var chatModel: ChatModel
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State var composeState: ComposeState = ComposeState()
    @State var selectedRange = NSRange()
    @State private var keyboardVisible = false
    @State private var keyboardHiddenDate = Date.now
    @State var authorized = !UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA)
    @State private var terminalItem: TerminalItem?
    @State private var scrolled = false
    @State private var showing = false

    var body: some View {
        if authorized {
            terminalView()
                .onAppear {
                    if showing { return }
                    showing = true
                    Task {
                        let items = await TerminalItems.shared.items()
                        await MainActor.run {
                            chatModel.terminalItems = items
                            chatModel.showingTerminal = true
                        }
                    }
                }
                .onDisappear {
                    if terminalItem == nil {
                        chatModel.showingTerminal = false
                        chatModel.terminalItems = []
                    }
                }
        } else {
            Button(action: runAuth) { Label("Unlock", systemImage: "lock") }
            .onAppear(perform: runAuth)
        }
    }

    private func runAuth() { authorize(NSLocalizedString("Open chat console", comment: "authentication reason"), $authorized) }

    private func terminalView() -> some View {
        VStack {
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack {
                        ForEach(chatModel.terminalItems) { item in
                            Button {
                                terminalItem = item
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
                        .onAppear {
                            if !scrolled {
                                scrollToBottom(proxy)
                                scrolled = true
                            }
                        }
                        .onChange(of: chatModel.terminalItems.count) { _ in scrollToBottom(proxy) }
                        .onChange(of: keyboardVisible) { _ in
                            if keyboardVisible {
                                DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
                                    scrollToBottom(proxy, animation: .easeInOut(duration: 1))
                                }
                            }
                        }
                        .background(NavigationLink(
                            isActive: Binding(get: { terminalItem != nil }, set: { _ in }),
                            destination: terminalItemView,
                            label: { EmptyView() }
                        ))
                    }
                }

                Spacer()

                SendMessageView(
                    composeState: $composeState,
                    selectedRange: $selectedRange,
                    sendMessage: { _ in consoleSendMessage() },
                    showVoiceMessageButton: false,
                    onMediaAdded: { _ in },
                    keyboardVisible: $keyboardVisible,
                    keyboardHiddenDate: $keyboardHiddenDate
                )
                .padding(.horizontal, 12)
            }
        }
        .navigationViewStyle(.stack)
        .toolbar {
            // Redaction broken for `.navigationTitle` - using a toolbar item instead.
            ToolbarItem(placement: .principal) {
                Text("Chat console").font(.headline)
            }
        }
        .modifier(ThemedBackground())
    }

    func scrollToBottom(_ proxy: ScrollViewProxy, animation: Animation = .default) {
        if let id = chatModel.terminalItems.last?.id {
            withAnimation(animation) {
                proxy.scrollTo(id, anchor: .bottom)
            }
        }
    }

    func terminalItemView() -> some View {
        let s = terminalItem?.details ?? ""
        return ScrollView {
            Text(s.prefix(maxItemSize))
                .padding()
                .frame(maxWidth: .infinity)
        }
        .toolbar {
            ToolbarItem(placement: .navigationBarTrailing) {
                Button { showShareSheet(items: [s]) } label: {
                    Image(systemName: "square.and.arrow.up")
                }
            }
        }
        .onDisappear { terminalItem = nil }
        .modifier(ThemedBackground())
    }
    
    func consoleSendMessage() {
        if composeState.message.starts(with: "/sql") && (!prefPerformLA || !developerTools) {
            let resp: APIResult<ChatResponse2> = APIResult.error(ChatError.error(errorType: ChatErrorType.commandError(message: "Failed reading: empty")))
            Task {
                await TerminalItems.shared.addCommand(.now, .string(composeState.message), resp)
            }
        } else {
            let cmd = composeState.message
            DispatchQueue.global().async {
                Task {
                    await MainActor.run { composeState.inProgress = true }
                    await sendTerminalCmd(cmd)
                    await MainActor.run { composeState.inProgress = false }
                }
            }
        }
        composeState = ComposeState()
    }
}

func sendTerminalCmd(_ cmd: String) async {
    let start: Date = .now
    await withCheckedContinuation { (cont: CheckedContinuation<Void, Never>) in
        let d = sendSimpleXCmdStr(cmd)
        Task {
            guard let d else {
                await TerminalItems.shared.addCommand(start, ChatCommand.string(cmd), APIResult<ChatResponse2>.error(.invalidJSON(json: nil)))
                return
            }
            let r0: APIResult<ChatResponse0> = decodeAPIResult(d)
            guard case .invalid = r0 else {
                await TerminalItems.shared.addCommand(start, .string(cmd), r0)
                return
            }
            let r1: APIResult<ChatResponse1> = decodeAPIResult(d)
            guard case .invalid = r1 else {
                await TerminalItems.shared.addCommand(start, .string(cmd), r1)
                return
            }
            let r2: APIResult<ChatResponse2> = decodeAPIResult(d)
            await TerminalItems.shared.addCommand(start, .string(cmd), r2)
        }
        cont.resume(returning: ())
    }
}

struct TerminalView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.terminalItems = [
            .err(.now, APIResult<ChatResponse2>.invalid(type: "contactSubscribed", json: "{}".data(using: .utf8)!).unexpected),
            .err(.now, APIResult<ChatResponse2>.invalid(type: "newChatItems", json: "{}".data(using: .utf8)!).unexpected)
        ]
        return NavigationView {
            TerminalView()
                .environmentObject(chatModel)
        }

    }
}
