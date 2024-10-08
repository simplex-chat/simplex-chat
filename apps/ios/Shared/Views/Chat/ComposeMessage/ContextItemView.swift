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
    let contextItem: ChatItem
    let contextIcon: String
    let cancelContextItem: () -> Void
    var showSender: Bool = true

    var body: some View {
        HStack {
            Image(systemName: contextIcon)
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 16, height: 16)
                .foregroundColor(theme.colors.secondary)
            if showSender, let sender = contextItem.memberDisplayName {
                VStack(alignment: .leading, spacing: 4) {
                    Text(sender).font(.caption).foregroundColor(theme.colors.secondary)
                    msgContentView(lines: 2)
                }
            } else {
                msgContentView(lines: 3)
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
        .background(chatItemFrameColor(contextItem, theme))
    }

    private func msgContentView(lines: Int) -> some View {
        contextMsgPreview()
            .multilineTextAlignment(isRightToLeft(contextItem.text) ? .trailing : .leading)
            .lineLimit(lines)
    }

    private func contextMsgPreview() -> Text {
        return attachment() + messageText(contextItem.text, contextItem.formattedText, nil, preview: true, showSecrets: false, secondaryColor: theme.colors.secondary)

        func attachment() -> Text {
            switch contextItem.content.msgContent {
            case .file: return image("doc.fill")
            case .image: return image("photo")
            case .voice: return image("play.fill")
            default: return Text("")
            }
        }

        func image(_ s: String) -> Text {
            Text(Image(systemName: s)).foregroundColor(Color(uiColor: .tertiaryLabel)) + Text(" ")
        }
    }
}

struct ContextItemView_Previews: PreviewProvider {
    static var previews: some View {
        let contextItem: ChatItem = ChatItem.getSample(1, .directSnd, .now, "hello")
        return ContextItemView(chat: Chat.sampleData, contextItem: contextItem, contextIcon: "pencil.circle", cancelContextItem: {})
    }
}
