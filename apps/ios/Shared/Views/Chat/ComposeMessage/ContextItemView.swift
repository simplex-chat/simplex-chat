//
//  ContextItemView.swift
//  SimpleX
//
//  Created by JRoberts on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContextItemView: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    let contextItems: [ChatItem]
    let contextIcon: String
    let cancelContextItem: () -> Void
    var contextIconForeground: Color? = nil
    var showSender: Bool = true

    var body: some View {
        HStack {
            Image(systemName: contextIcon)
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 16, height: 16)
                .foregroundColor(contextIconForeground ?? theme.colors.secondary)
            if let singleItem = contextItems.first, contextItems.count == 1 {
                if showSender, let sender = singleItem.memberDisplayName {
                     VStack(alignment: .leading, spacing: 4) {
                         Text(sender).font(.caption).foregroundColor(theme.colors.secondary)
                         msgContentView(lines: 2, contextItem: singleItem)
                     }
                 } else {
                     msgContentView(lines: 3, contextItem: singleItem)
                 }
            } else {
                Text(
                    chat.chatInfo.chatType == .local
                    ? "Saving \(contextItems.count) messages"
                    : "Forwarding \(contextItems.count) messages"
                )
                .italic()
            }
            Spacer()
            Button {
                withAnimation {
                    cancelContextItem()
                }
            } label: {
                Image(systemName: "multiply")
            }
            .tint(theme.colors.primary)
        }
        .padding(12)
        .frame(minHeight: 54)
        .frame(maxWidth: .infinity)
        .background(background)
    }

    private var background: Color {
        contextItems.first
            .map { chatItemFrameColor($0, theme) }
            ?? Color(uiColor: .tertiarySystemBackground)
    }

    private func msgContentView(lines: Int, contextItem: ChatItem) -> some View {
        contextMsgPreview(contextItem)
            .multilineTextAlignment(isRightToLeft(contextItem.text) ? .trailing : .leading)
            .lineLimit(lines)
    }

    private func contextMsgPreview(_ contextItem: ChatItem) -> some View {
        let r = messageText(contextItem.text, contextItem.formattedText, sender: nil, preview: true, mentions: contextItem.mentions, userMemberId: nil, showSecrets: nil, backgroundColor: UIColor(background))
        let t = attachment() + Text(AttributedString(r.string))
        return t.if(r.hasSecrets, transform: hiddenSecretsView)

        func attachment() -> Text {
            let isFileLoaded = if let fileSource = getLoadedFileSource(contextItem.file) {
                FileManager.default.fileExists(atPath: getAppFilePath(fileSource.filePath).path)
            } else { false }
            switch contextItem.content.msgContent {
            case .file: return isFileLoaded ? image("doc.fill") : Text("")
            case .image: return image("photo")
            case .voice: return isFileLoaded ? image("play.fill") : Text("")
            default: return Text("")
            }
        }

        func image(_ s: String) -> Text {
            Text(Image(systemName: s)).foregroundColor(Color(uiColor: .tertiaryLabel)) + textSpace
        }
    }
}

struct ContextItemView_Previews: PreviewProvider {
    static var previews: some View {
        let contextItem: ChatItem = ChatItem.getSample(1, .directSnd, .now, "hello")
        return ContextItemView(chat: Chat.sampleData, contextItems: [contextItem], contextIcon: "pencil.circle", cancelContextItem: {}, contextIconForeground: Color.red)
    }
}
