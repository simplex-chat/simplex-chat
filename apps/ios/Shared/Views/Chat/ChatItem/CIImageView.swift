//
//  CIImageView.swift
//  SimpleX
//
//  Created by JRoberts on 12/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIImageView: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme
    let chatItem: ChatItem
    let image: String
    let maxWidth: CGFloat
    @Binding var imgWidth: CGFloat?
    @State var scrollProxy: ScrollViewProxy?
    @State var metaColor: Color
    @State private var showFullScreenImage = false

    var body: some View {
        let file = chatItem.file
        VStack(alignment: .center, spacing: 6) {
            if let uiImage = getLoadedImage(file) {
                imageView(uiImage)
                .fullScreenCover(isPresented: $showFullScreenImage) {
                    FullScreenMediaView(chatItem: chatItem, image: uiImage, showView: $showFullScreenImage, scrollProxy: scrollProxy)
                }
                .onTapGesture { showFullScreenImage = true }
                .onChange(of: m.activeCallViewIsCollapsed) { _ in
                    showFullScreenImage = false
                }
            } else if let data = Data(base64Encoded: dropImagePrefix(image)),
                      let uiImage = UIImage(data: data) {
                imageView(uiImage)
                    .onTapGesture {
                        if let file = file {
                            switch file.fileStatus {
                            case .rcvInvitation:
                                Task {
                                    if let user = m.currentUser {
                                        await receiveFile(user: user, fileId: file.fileId)
                                    }
                                }
                            case .rcvAccepted:
                                switch file.fileProtocol {
                                case .xftp:
                                    AlertManager.shared.showAlertMsg(
                                        title: "Waiting for image",
                                        message: "Image will be received when your contact completes uploading it."
                                    )
                                case .smp:
                                    AlertManager.shared.showAlertMsg(
                                        title: "Waiting for image",
                                        message: "Image will be received when your contact is online, please wait or check later!"
                                    )
                                case .local: ()
                                }
                            case .rcvTransfer: () // ?
                            case .rcvComplete: () // ?
                            case .rcvCancelled: () // TODO
                            default: ()
                            }
                        }
                    }
            }
        }
    }

    private func imageView(_ img: UIImage) -> some View {
        let w = img.size.width <= img.size.height ? maxWidth * 0.75 : maxWidth
        DispatchQueue.main.async { imgWidth = w }
        return ZStack(alignment: .topTrailing) {
            if img.imageData == nil {
                Image(uiImage: img)
                        .resizable()
                        .scaledToFit()
                        .frame(width: w)
            } else {
                SwiftyGif(image: img)
                        .frame(width: w, height: w * img.size.height / img.size.width)
                        .scaledToFit()
            }
            loadingIndicator()
        }
    }

    @ViewBuilder private func loadingIndicator() -> some View {
        if let file = chatItem.file {
            switch file.fileStatus {
            case .sndStored:
                switch file.fileProtocol {
                case .xftp: progressView()
                case .smp: EmptyView()
                case .local: EmptyView()
                }
            case .sndTransfer: progressView()
            case .sndComplete: fileIcon("checkmark", 10, 13)
            case .sndCancelled: fileIcon("xmark", 10, 13)
            case .sndError: fileIcon("xmark", 10, 13)
            case .rcvInvitation: fileIcon("arrow.down", 10, 13)
            case .rcvAccepted: fileIcon("ellipsis", 14, 11)
            case .rcvTransfer: progressView()
            case .rcvCancelled: fileIcon("xmark", 10, 13)
            case .rcvError: fileIcon("xmark", 10, 13)
            case .invalid: fileIcon("questionmark", 10, 13)
            default: EmptyView()
            }
        }
    }

    private func fileIcon(_ icon: String, _ size: CGFloat, _ padding: CGFloat) -> some View {
        Image(systemName: icon)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: size, height: size)
            .foregroundColor(metaColor)
            .padding(padding)
    }

    private func progressView() -> some View {
        ProgressView()
            .progressViewStyle(.circular)
            .frame(width: 20, height: 20)
            .tint(.white)
            .padding(8)
    }
}
