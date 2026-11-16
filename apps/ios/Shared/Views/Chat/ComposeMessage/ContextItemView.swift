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
        let r = messageText(contextItem.text, contextItem.formattedText, markdownBlocks: contextItem.markdownBlocks, sender: nil, preview: true, mentions: contextItem.mentions, userMemberId: nil, showSecrets: nil, backgroundColor: UIColor(background), stripLink: contextItem.content.msgContent?.chatLinkStr)
        let t = attachment() + Text(AttributedString(r.string))
        return t.if(r.hasSecrets, transform: hiddenSecretsView)

        func attachment() -> Text {
            let isFileLoaded = if let fileSource = getLoadedFileSource(contextItem.file) {
                FileManager.default.fileExists(atPath: getAppFilePath(fileSource.filePath).path)
            } else { false }
            switch contextItem.content.msgContent {
            case .file: return isFileLoaded ? image(Image(systemName: "doc.fill")) : Text("")
            case .image: return image(Image(systemName: "photo"))
            case .voice: return isFileLoaded ? image(Image(systemName: "play.fill")) : Text("")
            case let .chat(_, chatLink, _):
                let hasText = contextItem.text != chatLink.connLinkStr
                return image(chatLink.smallIcon(agent: textAgentIcon())) + Text(chatLink.displayName) + Text(verbatim: hasText ? " - " : "")
            default: return Text("")
            }
        }

        func image(_ i: Image) -> Text {
            Text(i).foregroundColor(Color(uiColor: .tertiaryLabel)) + textSpace
        }

        func textAgentIcon() -> Image {
            guard let person = UIImage(systemName: "person", withConfiguration: UIImage.SymbolConfiguration(textStyle: .body)),
                  let agent = UIImage(named: "agent") else { return Image("agent") }
            let size = CGSize(width: person.size.height * agent.size.width / agent.size.height, height: person.size.height)
            return Image(uiImage: UIGraphicsImageRenderer(size: size).image { _ in agent.draw(in: CGRect(origin: .zero, size: size)) }.withRenderingMode(.alwaysTemplate))
        }
    }
}

struct ContextItemView_Previews: PreviewProvider {
    static var previews: some View {
        let contextItem: ChatItem = ChatItem.getSample(1, .directSnd, previewDate, "hello")
        return ContextItemView(chat: Chat.sampleData, contextItems: [contextItem], contextIcon: "pencil.circle", cancelContextItem: {}, contextIconForeground: Color.red)
    }
}
