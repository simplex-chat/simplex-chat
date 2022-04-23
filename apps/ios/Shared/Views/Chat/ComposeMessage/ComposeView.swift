//
//  ComposeView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

//enum ComposePreview {
//    case noPreview
//    case linkPreview(linkPreview: LinkPreview)
//    case imagePreview(image: String)
//}

enum ComposeContextItem {
    case noContextItem
    case quotedItem(chatItem: ChatItem)
    case editingItem(chatItem: ChatItem)
}

struct ComposeState {
    var message: String
    var contextItem: ComposeContextItem

    func editing() -> Bool {
        switch contextItem {
        case .editingItem: return true
        default: return false
        }
    }

    func sendEnabled() -> Bool {
        return !message.isEmpty
    }
}

func newComposeState() -> ComposeState {
    return ComposeState(message: "", contextItem: .noContextItem)
}

func composeStateEditing(editingItem: ChatItem) -> ComposeState {
    return ComposeState(message: editingItem.content.text, contextItem: .editingItem(chatItem: editingItem))
}

func composeStateQuoted(oldState: ComposeState, quotedItem: ChatItem) -> ComposeState {
    return ComposeState(message: oldState.message, contextItem: .quotedItem(chatItem: quotedItem))
}

func composeStateCancelQuote(oldState: ComposeState, quotedItem: ChatItem) -> ComposeState {
    return ComposeState(message: oldState.message, contextItem: .noContextItem)
}

struct ComposeView: View {
    @EnvironmentObject var chatModel: ChatModel
    let chat: Chat
    @Binding var composeState: ComposeState

    @State var inProgress: Bool = false
    @FocusState.Binding var keyboardVisible: Bool

    @State var linkPreview: LinkPreview? = nil
    @State var linkUrl: URL? = nil
    @State var prevLinkUrl: URL? = nil
    @State var pendingLinkUrl: URL? = nil
    @State var cancelledLinks: Set<String> = []

    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var imageSource: ImageSource = .imageLibrary
    @State var chosenImage: UIImage?
    @State var imagePreview: String?
    
    var body: some View {
        VStack(spacing: 0) {
            if let metadata = imagePreview {
                ComposeImageView(image: metadata, cancelImage: cancelImage)
            } else if let metadata = linkPreview {
                ComposeLinkView(linkPreview: metadata, cancelPreview: cancelPreview)
            }
            contextItemView()
            HStack (alignment: .bottom) {
                Button {
                    showChooseSource = true
                } label: {
                    Image(systemName: "paperclip")
                        .resizable()
                }
                .disabled(composeState.editing())
                .frame(width: 25, height: 25)
                .padding(.bottom, 12)
                .padding(.leading, 12)
                SendMessageView(
                    composeState: $composeState,
                    sendMessage: { text in
                        sendMessage(text)
                        resetLinkPreview()
                    },
                    inProgress: inProgress,
                    keyboardVisible: $keyboardVisible
                )
                .padding(.trailing, 12)
                .background(.background)
            }
        }
        .onChange(of: composeState.message) { _ in
            if composeState.message.count > 0 {
                if imagePreview == nil {
                    showLinkPreview(composeState.message)
                }
            } else {
                resetLinkPreview()
            }
        }
        .confirmationDialog("Attach", isPresented: $showChooseSource, titleVisibility: .visible) {
            Button("Take picture") {
                imageSource = .camera
                showImagePicker = true
            }
            Button("Choose from library") {
                imageSource = .imageLibrary
                showImagePicker = true
            }
        }
        .sheet(isPresented: $showImagePicker) {
            switch imageSource {
            case .imageLibrary:
                LibraryImagePicker(image: $chosenImage) {
                    didSelectItem in showImagePicker = false
                }
            case .camera:
                CameraImagePicker(image: $chosenImage)
            }
        }
        .onChange(of: chosenImage) { image in
            if let image = image {
                imagePreview = resizeImageToStrSize(image, maxDataSize: 14000)
            } else {
                imagePreview = nil
            }
        }
        .onChange(of: imagePreview) { imagePreview in
            if imagePreview != nil {
                linkPreview = nil
            }
        }
    }

    //    @ViewBuilder func previewView() -> some View {
    //        switch composeState.preview {
    //        case .noPreview:
    //            EmptyView()
    //        case let .linkPreview(linkPreview: preview):
    //            ComposeLinkView(linkPreview: preview, cancelPreview: cancelLinkPreview)
    //        case let .imagePreview(image: img):
    //            ComposeImageView(image: img, cancelImage: cancelImagePreview)
    //        }
    //    }

    @ViewBuilder private func contextItemView() -> some View {
        switch composeState.contextItem {
        case .noContextItem:
            EmptyView()
        case let .quotedItem(chatItem: quotedItem):
            ContextItemView(contextItem: quotedItem, cancelContextItem: { composeState = composeStateCancelQuote(oldState: composeState, quotedItem: quotedItem) })
        case let .editingItem(chatItem: editingItem):
            ContextItemView(contextItem: editingItem, cancelContextItem: { composeState = newComposeState()})
        }
    }

    private func sendMessage(_ text: String) {
        logger.debug("ChatView sendMessage")
        Task {
            logger.debug("ChatView sendMessage: in Task")
            do {
                switch composeState.contextItem {
                case let .editingItem(chatItem: ei):
                    if let oldMsgContent = ei.content.msgContent {
                        let chatItem = try await apiUpdateChatItem(
                            type: chat.chatInfo.chatType,
                            id: chat.chatInfo.apiId,
                            itemId: ei.id,
                            msg: updateMsgContent(oldMsgContent, composeState.message)
                        )
                        DispatchQueue.main.async {
                            let _ = self.chatModel.upsertChatItem(self.chat.chatInfo, chatItem)
                        }
                    }
                default:
                    var mc: MsgContent? = nil
                    var file: String? = nil
                    if let preview = imagePreview,
                       let uiImage = chosenImage,
                       let savedFile = saveImage(uiImage) {
                        mc = .image(text: text, image: preview)
                        file = savedFile
                    } else if let preview = linkPreview {
                        mc = .link(text: text, preview: preview)
                    } else {
                        mc = .text(text)
                    }
                    var quotedItemId: Int64? = nil
                    switch (composeState.contextItem) {
                    case let .quotedItem(chatItem: quotedItem):
                        quotedItemId = quotedItem.id
                    default:
                        quotedItemId = nil
                    }
                    if let mc = mc {
                        let chatItem = try await apiSendMessage(
                            type: chat.chatInfo.chatType,
                            id: chat.chatInfo.apiId,
                            file: file,
                            quotedItemId: quotedItemId,
                            msg: mc
                        )
                        chatModel.addChatItem(chat.chatInfo, chatItem)
                    }
                }
                composeState = newComposeState()
            } catch {
                logger.error("ChatView.sendMessage error: \(error.localizedDescription)")
            }
        }
    }

    private func updateMsgContent(_ msgContent: MsgContent, _ text: String) -> MsgContent {
        switch msgContent {
        case .text:
            return .text(text)
        case .link(_, let preview):
            return .link(text: text, preview: preview)
        case .image(_, let image):
            return .image(text: text, image: image)
        case .unknown(let type, _):
            return .unknown(type: type, text: text)
        }
    }

    private func saveImage(_ uiImage: UIImage) -> String? {
        if let imageDataResized = resizeImageToDataSize(uiImage, maxDataSize: maxImageSize) {
            let millisecondsSince1970 = Int64((Date().timeIntervalSince1970 * 1000.0).rounded())
            let fileToSave = "image_\(millisecondsSince1970).jpg"
            let filePath = getAppFilesDirectory().appendingPathComponent(fileToSave)
            do {
                try imageDataResized.write(to: filePath)
                return fileToSave
            } catch {
                logger.error("ChatView.saveImage error: \(error.localizedDescription)")
                return nil
            }
        }
        return nil
    }

    private func showLinkPreview(_ s: String) {
        prevLinkUrl = linkUrl
        linkUrl = parseMessage(s)
        if let url = linkUrl {
            if url != linkPreview?.uri && url != pendingLinkUrl {
                pendingLinkUrl = url
                if prevLinkUrl == url {
                    loadLinkPreview(url)
                } else {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 1.5) {
                        loadLinkPreview(url)
                    }
                }
            }
        } else {
            linkPreview = nil
        }
    }

    private func parseMessage(_ msg: String) -> URL? {
        do {
            let parsedMsg = try apiParseMarkdown(text: msg)
            let uri = parsedMsg?.first(where: { ft in
                ft.format == .uri && !cancelledLinks.contains(ft.text) && !isSimplexLink(ft.text)
            })
            if let uri = uri { return URL(string: uri.text) }
            else { return nil }
        } catch {
            logger.error("apiParseMarkdown error: \(error.localizedDescription)")
            return nil
        }
    }

    private func cancelImage() {
        chosenImage = nil
        imagePreview = nil
    }

    private func isSimplexLink(_ link: String) -> Bool {
        link.starts(with: "https://simplex.chat") || link.starts(with: "http://simplex.chat")
    }

    private func cancelPreview() {
        if let uri = linkPreview?.uri.absoluteString {
            cancelledLinks.insert(uri)
        }
        linkPreview = nil
    }

    private func loadLinkPreview(_ url: URL) {
        if pendingLinkUrl == url {
            getLinkPreview(url: url) { lp in
                if pendingLinkUrl == url {
                    linkPreview = lp
                    pendingLinkUrl = nil
                }
            }
        }
    }

    private func resetLinkPreview() {
        linkUrl = nil
        prevLinkUrl = nil
        pendingLinkUrl = nil
        cancelledLinks = []
    }
}

struct ComposeView_Previews: PreviewProvider {
    static var previews: some View {
        let chat = Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: [])
        @State var composeState = ComposeState(message: "hello", contextItem: .noContextItem)
        @FocusState var keyboardVisible: Bool

        return Group {
            ComposeView(
                chat: chat,
                composeState: $composeState,
                keyboardVisible: $keyboardVisible
            )
            ComposeView(
                chat: chat,
                composeState: $composeState,
                keyboardVisible: $keyboardVisible
            )
        }
    }
}
