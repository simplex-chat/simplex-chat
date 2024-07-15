//
//  ContextItemView.swift
//  SimpleX
//
//  Created by JRoberts on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

func composeContextItemBackground() -> some View {
    Rectangle()
        .fill(.thinMaterial)
        .shadow(color: .black.opacity(0.12), radius: 2, x: 0, y: 0)
        .mask(Rectangle().padding(.bottom, -8))
}

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
        .frame(minHeight: 50)
        .frame(maxWidth: .infinity)
        .background(composeContextItemBackground())
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
