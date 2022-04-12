//
//  ComposeView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

// TODO
//enum ComposeState {
//    case plain
//    case quoted(quotedItem: ChatItem)
//    case editing(editingItem: ChatItem)
//}

//enum ReferencedItem {
//    case none
//    case quoted(quotedItem: ChatItem)
//    case editing(editingItem: ChatItem)
//}
//
//enum Preview {
//    case none
//    case link(linkPreview: LinkPreview)
//    case image(image: UIImage)
//}
//
//struct ComposeState {
//    var quotedItem: ChatItem? = nil
//    var editingItem: ChatItem? = nil
//    var linkPreview: LinkPreview? = nil
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
    @State var prevLinkUrl: URL? = nil
    @State var pendingLinkUrl: URL? = nil
    @State var cancelledLinks: Set<String> = []

    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var imageSource: ImageSource = .imageLibrary
    @Binding var chosenImage: UIImage?
    @Binding var imagePreview: String?
    
    var body: some View {
        VStack(spacing: 0) {
            if let metadata = imagePreview {
                ComposeImageView(image: metadata, cancelImage: nil)
            } else if let metadata = linkPreview {
                ComposeLinkView(linkPreview: metadata, cancelPreview: cancelPreview)
            }
            if (quotedItem != nil) {
                ContextItemView(contextItem: $quotedItem, editing: $editing)
            } else if (editingItem != nil) {
                ContextItemView(contextItem: $editingItem, editing: $editing, resetMessage: resetMessage)
            }
            HStack{
                Button {
                    showChooseSource = true
                } label: {
                    Image(systemName: "paperclip")
                        .resizable()
                }
                .disabled(editingItem != nil)
                .frame(width: 25, height: 25)
                .padding(.vertical, 4)
                .padding(.leading, 12)
                SendMessageView(
                    sendMessage: { text in
                        sendMessage(text)
                        resetLinkPreview()
                    },
                    inProgress: inProgress,
                    message: $message,
                    keyboardVisible: $keyboardVisible,
                    editing: $editing
                )
                .padding(.trailing, 12)
                .background(.background)
            }
        }
        .onChange(of: message) { _ in
            if message.count > 0 {
                showLinkPreview(message)
            } else {
                resetLinkPreview()
            }
        }
        .onChange(of: editingItem == nil) { _ in
            editing = (editingItem != nil)
        }
        .confirmationDialog("Add image", isPresented: $showChooseSource, titleVisibility: .visible) {
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
                imagePreview = resizeImageToDataSize(cropToSquare(image), maxDataSize: 12500)
            } else {
                imagePreview = nil
            }
        }
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
        @State var message: String = ""
        @FocusState var keyboardVisible: Bool
        @State var item: ChatItem? = ChatItem.getSample(1, .directSnd, .now, "hello")
        @State var nilItem: ChatItem? = nil
        @State var linkPreview: LinkPreview? = nil
        @State var chosenImage: UIImage? = nil
        @State var imagePreview: String? = nil

        return Group {
            ComposeView(
                message: $message,
                quotedItem: $item,
                editingItem: $nilItem,
                linkPreview: $linkPreview,
                sendMessage: { print ($0) },
                resetMessage: {},
                keyboardVisible: $keyboardVisible,
                chosenImage: $chosenImage,
                imagePreview: $imagePreview
            )
            ComposeView(
                message: $message,
                quotedItem: $nilItem,
                editingItem: $item,
                linkPreview: $linkPreview,
                sendMessage: { print ($0) },
                resetMessage: {},
                keyboardVisible: $keyboardVisible,
                chosenImage: $chosenImage,
                imagePreview: $imagePreview
            )
        }
    }
}
