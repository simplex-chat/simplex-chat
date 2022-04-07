//
//  ComposeView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import LinkPresentation

// TODO
//enum ComposeState {
//    case plain
//    case quoted(quotedItem: ChatItem)
//    case editing(editingItem: ChatItem)
//}

struct ComposeView: View {
    @Binding var message: String
    @Binding var quotedItem: ChatItem?
    @Binding var editingItem: ChatItem?
    @Binding var linkPreview: LinkPreview?

    var sendMessage: (String) -> Void
    var resetMessage: () -> Void
    var inProgress: Bool = false
    @FocusState.Binding var keyboardVisible: Bool
    @State var editing: Bool = false
    @State var linkUrl: URL? = nil
    @State var previewCancelled: Bool = false
    
    
    private func isValidLink(link: String) -> Bool {
        return !(link.starts(with: "https://simplex.chat") || link.starts(with: "http://simplex.chat") || link.starts(with: "simplex.chat"))
    }
    
    func cancelPreview() {
        previewCancelled = true
    }

    func parseMessage(_ msg: String) {
        Task {
            do {
                if let parsedMsg = try await apiParseMarkdown(text: msg),
                   let link = parsedMsg.first(where: { $0.format == .uri }),
                   isValidLink(link: link.text) {
                        linkUrl = URL(string: link.text)
                    }
            } catch {
                logger.error("MessageParsing error: \(error.localizedDescription)")
            }
        }
    }

    var body: some View {
        VStack(spacing: 0) {
            if !previewCancelled, let metadata = linkPreview {
                SmallLinkPreviewView(metadata: metadata, cancelPreview: cancelPreview)
            }
            if (quotedItem != nil) {
                ContextItemView(contextItem: $quotedItem, editing: $editing)
            } else if (editingItem != nil) {
                ContextItemView(contextItem: $editingItem, editing: $editing, resetMessage: resetMessage)
            }
            SendMessageView(
                sendMessage: sendMessage,
                inProgress: inProgress,
                message: $message,
                keyboardVisible: $keyboardVisible,
                editing: $editing
            )
            .background(.background)
        }
        .onChange(of: message) {
            _ in
            if  message.count > 0 {
                parseMessage(message)
                if let url = linkUrl {
                    Task { linkPreview = await getLinkMetadata(url: url) }
                }
            }
        }
        .onChange(of: editingItem == nil) { _ in
            editing = (editingItem != nil)
        }
    }
}

struct ComposeView_Previews: PreviewProvider {
    static var previews: some View {
        @State var message: String = ""
        @FocusState var keyboardVisible: Bool
        @State var item: ChatItem? = ChatItem.getSample(1, .directSnd, .now, "hello")
        @State var nilItem: ChatItem? = nil
        @State var linkPreview: LinkPreview? = nil

        return Group {
            ComposeView(
                message: $message,
                quotedItem: $item,
                editingItem: $nilItem,
                linkPreview: $linkPreview,
                sendMessage: { print ($0) },
                resetMessage: {},
                keyboardVisible: $keyboardVisible
            )
            ComposeView(
                message: $message,
                quotedItem: $nilItem,
                editingItem: $item,
                linkPreview: $linkPreview,
                sendMessage: { print ($0) },
                resetMessage: {},
                keyboardVisible: $keyboardVisible
            )
        }
    }
}
