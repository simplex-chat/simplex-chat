//
//  ComposeView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum ComposePreview {
    case noPreview
    case linkPreview(linkPreview: LinkPreview?)
    case imagePreview(imagePreview: String)
    case filePreview(fileName: String)
}

enum ComposeContextItem {
    case noContextItem
    case quotedItem(chatItem: ChatItem)
    case editingItem(chatItem: ChatItem)
}

struct ComposeState {
    var message: String
    var preview: ComposePreview
    var contextItem: ComposeContextItem
    var inProgress: Bool = false
    var useLinkPreviews: Bool = UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_LINK_PREVIEWS)

    init(
        message: String = "",
        preview: ComposePreview = .noPreview,
        contextItem: ComposeContextItem = .noContextItem
    ) {
        self.message = message
        self.preview = preview
        self.contextItem = contextItem
    }

    init(editingItem: ChatItem) {
        self.message = editingItem.content.text
        self.preview = chatItemPreview(chatItem: editingItem)
        self.contextItem = .editingItem(chatItem: editingItem)
    }

    func copy(
        message: String? = nil,
        preview: ComposePreview? = nil,
        contextItem: ComposeContextItem? = nil
    ) -> ComposeState {
        ComposeState(
            message: message ?? self.message,
            preview: preview ?? self.preview,
            contextItem: contextItem ?? self.contextItem
        )
    }

    func editing() -> Bool {
        switch contextItem {
        case .editingItem: return true
        default: return false
        }
    }

    func sendEnabled() -> Bool {
        switch preview {
        case .imagePreview:
            return true
        case .filePreview:
            return true
        default:
            return !message.isEmpty
        }
    }

    func linkPreviewAllowed() -> Bool {
        switch preview {
        case .imagePreview:
            return false
        case .filePreview:
            return false
        default:
            return useLinkPreviews
        }
    }

    func linkPreview() -> LinkPreview? {
        switch preview {
        case let .linkPreview(linkPreview):
            return linkPreview
        default:
            return nil
        }
    }
}

func chatItemPreview(chatItem: ChatItem) -> ComposePreview {
    let chatItemPreview: ComposePreview
    switch chatItem.content.msgContent {
    case .text:
        chatItemPreview = .noPreview
    case let .link(_, preview: preview):
        chatItemPreview = .linkPreview(linkPreview: preview)
    case let .image(_, image: image):
        chatItemPreview = .imagePreview(imagePreview: image)
    case .file:
        chatItemPreview = .filePreview(fileName: chatItem.file?.fileName ?? "")
    default:
        chatItemPreview = .noPreview
    }
    return chatItemPreview
}

struct ComposeView: View {
    @EnvironmentObject var chatModel: ChatModel
    let chat: Chat
    @Binding var composeState: ComposeState
    @FocusState.Binding var keyboardVisible: Bool

    @State var linkUrl: URL? = nil
    @State var prevLinkUrl: URL? = nil
    @State var pendingLinkUrl: URL? = nil
    @State var cancelledLinks: Set<String> = []

    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State var chosenImage: UIImage? = nil
    @State private var showFileImporter = false
    @State var chosenFile: URL? = nil
    
    var body: some View {
        VStack(spacing: 0) {
            contextItemView()
            switch (composeState.editing(), composeState.preview) {
                case (true, .filePreview): EmptyView()
                default: previewView()
            }
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
                    sendMessage: {
                        sendMessage()
                        resetLinkPreview()
                    },
                    keyboardVisible: $keyboardVisible
                )
                .padding(.trailing, 12)
                .background(.background)
            }
        }
        .onChange(of: composeState.message) { _ in
            if composeState.linkPreviewAllowed() {
                if composeState.message.count > 0 {
                    showLinkPreview(composeState.message)
                } else {
                    resetLinkPreview()
                }
            }
        }
        .confirmationDialog("Attach", isPresented: $showChooseSource, titleVisibility: .visible) {
            Button("Take picture") {
                showTakePhoto = true
            }
            Button("Choose from library") {
                showImagePicker = true
            }
            if UIPasteboard.general.hasImages {
                Button("Paste image") {
                    chosenImage = UIPasteboard.general.image
                }
            }
            Button("Choose file") {
                showFileImporter = true
            }
        }
        .fullScreenCover(isPresented: $showTakePhoto) {
            ZStack {
                Color.black.edgesIgnoringSafeArea(.all)
                CameraImagePicker(image: $chosenImage)
            }
        }
        .sheet(isPresented: $showImagePicker) {
            LibraryImagePicker(image: $chosenImage) {
                didSelectItem in showImagePicker = false
            }
        }
        .onChange(of: chosenImage) { image in
            if let image = image,
               let imagePreview = resizeImageToStrSize(image, maxDataSize: 14000) {
                composeState = composeState.copy(preview: .imagePreview(imagePreview: imagePreview))
            } else {
                composeState = composeState.copy(preview: .noPreview)
            }
        }
        .fileImporter(
            isPresented: $showFileImporter,
            allowedContentTypes: [.data],
            allowsMultipleSelection: false
        ) { result in
            if case .success = result {
                do {
                    let fileURL: URL = try result.get().first!
                    var fileSize: Int? = nil
                    if fileURL.startAccessingSecurityScopedResource() {
                        let resourceValues = try fileURL.resourceValues(forKeys: [.fileSizeKey])
                        fileSize = resourceValues.fileSize
                    }
                    fileURL.stopAccessingSecurityScopedResource()
                    if let fileSize = fileSize,
                       fileSize <= maxFileSize {
                        chosenFile = fileURL
                        composeState = composeState.copy(preview: .filePreview(fileName: fileURL.lastPathComponent))
                    } else {
                        let prettyMaxFileSize = ByteCountFormatter().string(fromByteCount: maxFileSize)
                        AlertManager.shared.showAlertMsg(
                            title: "Large file!",
                            message: "Currently maximum supported file size is \(prettyMaxFileSize)."
                        )
                    }
                } catch {
                    logger.error("ComposeView fileImporter error \(error.localizedDescription)")
                }
            }
        }
    }

    @ViewBuilder func previewView() -> some View {
        switch composeState.preview {
        case .noPreview:
            EmptyView()
        case let .linkPreview(linkPreview: preview):
            ComposeLinkView(linkPreview: preview, cancelPreview: cancelLinkPreview)
        case let .imagePreview(imagePreview: img):
            ComposeImageView(
                image: img,
                cancelImage: {
                    composeState = composeState.copy(preview: .noPreview)
                    chosenImage = nil
                },
                cancelEnabled: !composeState.editing())
        case let .filePreview(fileName: fileName):
            ComposeFileView(
                fileName: fileName,
                cancelFile: {
                    composeState = composeState.copy(preview: .noPreview)
                    chosenFile = nil
                },
                cancelEnabled: !composeState.editing())
        }
    }

    @ViewBuilder private func contextItemView() -> some View {
        switch composeState.contextItem {
        case .noContextItem:
            EmptyView()
        case let .quotedItem(chatItem: quotedItem):
            ContextItemView(
                contextItem: quotedItem,
                contextIcon: "arrowshape.turn.up.left",
                cancelContextItem: { composeState = composeState.copy(contextItem: .noContextItem) }
            )
        case let .editingItem(chatItem: editingItem):
            ContextItemView(
                contextItem: editingItem,
                contextIcon: "pencil",
                cancelContextItem: { clearState() }
            )
        }
    }

    private func sendMessage() {
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
                            msg: updateMsgContent(oldMsgContent)
                        )
                        DispatchQueue.main.async {
                            let _ = self.chatModel.upsertChatItem(self.chat.chatInfo, chatItem)
                        }
                    }
                default:
                    var mc: MsgContent? = nil
                    var file: String? = nil
                    switch (composeState.preview) {
                    case .noPreview:
                        mc = .text(composeState.message)
                    case .linkPreview:
                        mc = checkLinkPreview()
                    case let .imagePreview(imagePreview: image):
                        if let uiImage = chosenImage,
                           let savedFile = saveImage(uiImage) {
                            mc = .image(text: composeState.message, image: image)
                            file = savedFile
                        }
                    case .filePreview:
                        if let fileURL = chosenFile,
                           let savedFile = saveFileFromURL(fileURL) {
                            mc = .file(composeState.message)
                            file = savedFile
                        }
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
                clearState()
            } catch {
                clearState()
                logger.error("ChatView.sendMessage error: \(error.localizedDescription)")
            }
        }
    }

    private func clearState() {
        composeState = ComposeState()
        linkUrl = nil
        prevLinkUrl = nil
        pendingLinkUrl = nil
        cancelledLinks = []
        chosenImage = nil
        chosenFile = nil
    }

    private func updateMsgContent(_ msgContent: MsgContent) -> MsgContent {
        switch msgContent {
        case .text:
            return checkLinkPreview()
        case .link:
            return checkLinkPreview()
        case .image(_, let image):
            return .image(text: composeState.message, image: image)
        case .file:
            return .file(composeState.message)
        case .unknown(let type, _):
            return .unknown(type: type, text: composeState.message)
        }
    }

    private func showLinkPreview(_ s: String) {
        prevLinkUrl = linkUrl
        linkUrl = parseMessage(s)
        if let url = linkUrl {
            if url != composeState.linkPreview()?.uri && url != pendingLinkUrl {
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
            composeState = composeState.copy(preview: .noPreview)
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

    private func isSimplexLink(_ link: String) -> Bool {
        link.starts(with: "https://simplex.chat") || link.starts(with: "http://simplex.chat")
    }

    private func cancelLinkPreview() {
        if let uri = composeState.linkPreview()?.uri.absoluteString {
            cancelledLinks.insert(uri)
        }
        pendingLinkUrl = nil
        composeState = composeState.copy(preview: .noPreview)
    }

    private func loadLinkPreview(_ url: URL) {
        if pendingLinkUrl == url {
            composeState = composeState.copy(preview: .linkPreview(linkPreview: nil))
            getLinkPreview(url: url) { linkPreview in
                if let linkPreview = linkPreview,
                   pendingLinkUrl == url {
                    composeState = composeState.copy(preview: .linkPreview(linkPreview: linkPreview))
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

    private func checkLinkPreview() -> MsgContent {
        switch (composeState.preview) {
        case let .linkPreview(linkPreview: linkPreview):
            if let url = parseMessage(composeState.message),
               let linkPreview = linkPreview,
               url == linkPreview.uri {
                return .link(text: composeState.message, preview: linkPreview)
            } else {
                return .text(composeState.message)
            }
        default:
            return .text(composeState.message)
        }
    }
}

struct ComposeView_Previews: PreviewProvider {
    static var previews: some View {
        let chat = Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: [])
        @State var composeState = ComposeState(message: "hello")
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
