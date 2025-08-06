//
//  CommandsMenuView.swift
//  SimpleX (iOS)
//
//  Created by EP on 03/08/2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let COMMAND_ROW_SIZE: CGFloat = 40
let MAX_VISIBLE_COMMAND_ROWS: CGFloat = 6.8

struct CommandsMenuView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    @Binding var composeState: ComposeState
    @Binding var selectedRange: NSRange
    @Binding var showCommandsMenu: Bool

    @State private var currentCommands: [ChatBotCommand] = []
    @State private var menuTreeBackPath: [(label: String, commands: [ChatBotCommand])] = []
    @State private var keywordWidth: CGFloat = 0

    var body: some View {
        ZStack(alignment: .bottom) {
            if !currentCommands.isEmpty {
                Color.white.opacity(0.01)
                    .edgesIgnoringSafeArea(.all)
                    .onTapGesture {
                        showCommandsMenu = false
                        currentCommands = []
                        menuTreeBackPath = []
                    }
                VStack(spacing: 0) {
                    Spacer()
                    Divider()
                    let cmdsCount = currentCommands.count + (menuTreeBackPath.isEmpty ? 0 : 1)
                    let scroll = ScrollView {
                        VStack(spacing: 0) {
                            if let prev = menuTreeBackPath.last {
                                menuLabelRow(prev)
                            }
                            ForEach(currentCommands, id: \.self, content: commandRow)
                        }
                        .padding(.top)
                    }
                    .frame(maxWidth: .infinity, maxHeight: COMMAND_ROW_SIZE * min(MAX_VISIBLE_COMMAND_ROWS, CGFloat(cmdsCount)) + 16)
                    .background(theme.colors.background)

                    if #available(iOS 16.0, *) {
                        scroll.scrollDismissesKeyboard(.never)
                    } else {
                        scroll
                    }
                }
                .onPreferenceChange(DetermineWidth.Key.self) { keywordWidth = $0 }
            }
        }
        .onChange(of: composeState.message) { message in
            let msg = message.trimmingCharacters(in: .whitespaces)
            if msg == "/" {
                currentCommands = chat.chatInfo.menuCommands
            } else if msg.first == "/" {
                currentCommands = filterShownCommands(chat.chatInfo.menuCommands, msg.dropFirst())
            } else {
                showCommandsMenu = false
                currentCommands = []
            }
            menuTreeBackPath = []
        }
        .onChange(of: showCommandsMenu) { show in
            currentCommands = show ? chat.chatInfo.menuCommands : []
            menuTreeBackPath = []
        }
    }

    private func menuLabelRow(_ prev: (label: String, commands: [ChatBotCommand])) -> some View {
        HStack {
            Image(systemName: "chevron.left")
                .foregroundColor(theme.colors.secondary)
            Text(prev.label)
                .fontWeight(.medium)
                .frame(maxWidth: .infinity)
        }
        .padding(.horizontal)
        .padding(.bottom)
        .frame(maxWidth: .infinity, maxHeight: COMMAND_ROW_SIZE, alignment: .leading)
        .contentShape(Rectangle())
        .onTapGesture {
            if !menuTreeBackPath.isEmpty {
                currentCommands = menuTreeBackPath.removeLast().commands
            }
        }
    }

    @ViewBuilder
    private func commandRow(_ command: ChatBotCommand) -> some View {
        switch command {
        case let .command(keyword, label, params, _):
            HStack {
                Text(label)
                    .lineLimit(1)
                    .frame(maxWidth: .infinity, alignment: .leading)
                Text("/" + keyword)
                    .font(.subheadline)
                    .lineLimit(1)
                    .foregroundColor(theme.colors.secondary)
                    .frame(minWidth: keywordWidth, alignment: .trailing)
                    .overlay(DetermineWidth())
            }
            .padding(.horizontal)
            .padding(.bottom)
            .frame(maxHeight: COMMAND_ROW_SIZE)
            .contentShape(Rectangle())
            .onTapGesture {
                if let params {
                    composeState.message = "/\(keyword) \(params)"
                    selectedRange = NSRange(location: composeState.message.count, length: 0)
                } else {
                    composeState.message = ""
                    sendBotCommand(chat, "/\(keyword)")
                }
                showCommandsMenu = false
                currentCommands = []
                menuTreeBackPath = []
            }
        case let .menu(label, cmds):
            HStack {
                Text(label)
                    .fontWeight(.medium)
                    .lineLimit(1)
                Spacer()
                Image(systemName: "chevron.right")
                    .foregroundColor(theme.colors.secondary)
            }
            .padding(.horizontal)
            .padding(.bottom)
            .frame(maxHeight: COMMAND_ROW_SIZE)
            .contentShape(Rectangle())
            .onTapGesture {
                menuTreeBackPath.append((label: label, commands: currentCommands))
                currentCommands = cmds
            }
        }
    }

    private func filterShownCommands(_ commands: [ChatBotCommand], _ msg: String.SubSequence) -> [ChatBotCommand] {
        var cmds: [ChatBotCommand] = []
        for command in commands {
            switch command {
            case let .command(keyword, _, _, hidden):
                if hidden != true && keyword.starts(with: msg) {
                    cmds.append(command)
                }
            case let .menu(_, innerCmds):
                cmds.append(contentsOf: filterShownCommands(innerCmds, msg))
            }
        }
        return cmds
    }
}

func sendBotCommand(_ chat: Chat, _ cmd: String) {
    Task {
        if let chatItems = await apiSendMessages(
            type: chat.chatInfo.chatType,
            id: chat.chatInfo.apiId,
            scope: nil,
            composedMessages: [ComposedMessage(msgContent: .text(cmd))]
        ) {
            await MainActor.run {
                for ci in chatItems {
                    ChatModel.shared.addChatItem(chat.chatInfo, ci)
                }
            }
        }
    }
}
