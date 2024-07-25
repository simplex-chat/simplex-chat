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
    let chatItem: ChatItem
    var preview: UIImage?
    let maxWidth: CGFloat
    var imgWidth: CGFloat?
    var smallView: Bool = false
    @Binding var showFullScreenImage: Bool
    @State private var blurred: Bool = UserDefaults.standard.integer(forKey: DEFAULT_PRIVACY_MEDIA_BLUR_RADIUS) > 0

    var body: some View {
        let file = chatItem.file
        VStack(alignment: .center, spacing: 6) {
            if let uiImage = getLoadedImage(file) {
                Group { if smallView { smallViewImageView(uiImage) } else { imageView(uiImage) } }
                .fullScreenCover(isPresented: $showFullScreenImage) {
                    FullScreenMediaView(chatItem: chatItem, image: uiImage, showView: $showFullScreenImage)
                }
                .modifier(PrivacyBlur(enabled: !smallView, blurred: $blurred))
                .onTapGesture { showFullScreenImage = true }
                .onChange(of: m.activeCallViewIsCollapsed) { _ in
                    showFullScreenImage = false
                }
            } else if let preview {
                Group { if smallView { smallViewImageView(preview) } else { imageView(preview) } }
                    .modifier(PrivacyBlur(enabled: !smallView, blurred: $blurred))
                    .onTapGesture {
                        if let file = file {
                            switch file.fileStatus {
                            case .rcvInvitation, .rcvAborted:
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
                            case let .rcvError(rcvFileError):
                                AlertManager.shared.showAlert(Alert(
                                    title: Text("File error"),
                                    message: Text(rcvFileError.errorInfo)
                                ))
                            case let .rcvWarning(rcvFileError):
                                AlertManager.shared.showAlert(Alert(
                                    title: Text("Temporary file error"),
                                    message: Text(rcvFileError.errorInfo)
                                ))
                            case let .sndError(sndFileError):
                                AlertManager.shared.showAlert(Alert(
                                    title: Text("File error"),
                                    message: Text(sndFileError.errorInfo)
                                ))
                            case let .sndWarning(sndFileError):
                                AlertManager.shared.showAlert(Alert(
                                    title: Text("Temporary file error"),
                                    message: Text(sndFileError.errorInfo)
                                ))
                            default: ()
                            }
                        }
                    }
            }
        }
        .onDisappear {
            showFullScreenImage = false
        }
    }

    private func imageView(_ img: UIImage) -> some View {
        let w = img.size.width <= img.size.height ? maxWidth * 0.75 : maxWidth
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
            if !blurred || !showDownloadButton(chatItem.file?.fileStatus) {
                loadingIndicator()
            }
        }
    }

    private func smallViewImageView(_ img: UIImage) -> some View {
        ZStack(alignment: .topTrailing) {
            if img.imageData == nil {
                Image(uiImage: img)
                    .resizable()
                    .aspectRatio(contentMode: .fill)
                    .frame(width: maxWidth, height: maxWidth)
            } else {
                SwiftyGif(image: img, contentMode: .scaleAspectFill)
                    .frame(width: maxWidth, height: maxWidth)
            }
            if chatItem.file?.showStatusIconInSmallView == true {
                loadingIndicator()
            }
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
            case .sndWarning: fileIcon("exclamationmark.triangle.fill", 10, 13)
            case .rcvInvitation: fileIcon("arrow.down", 10, 13)
            case .rcvAccepted: fileIcon("ellipsis", 14, 11)
            case .rcvTransfer: progressView()
            case .rcvAborted: fileIcon("exclamationmark.arrow.circlepath", 14, 11)
            case .rcvComplete: EmptyView()
            case .rcvCancelled: fileIcon("xmark", 10, 13)
            case .rcvError: fileIcon("xmark", 10, 13)
            case .rcvWarning: fileIcon("exclamationmark.triangle.fill", 10, 13)
            case .invalid: fileIcon("questionmark", 10, 13)
            }
        }
    }

    private func fileIcon(_ icon: String, _ size: CGFloat, _ padding: CGFloat) -> some View {
        Image(systemName: icon)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: size, height: size)
            .foregroundColor(.white)
            .padding(padding)
    }

    private func progressView() -> some View {
        ProgressView()
            .progressViewStyle(.circular)
            .frame(width: 20, height: 20)
            .tint(.white)
            .padding(8)
    }

    private func showDownloadButton(_ fileStatus: CIFileStatus?) -> Bool {
        switch fileStatus {
        case .rcvInvitation: true
        case .rcvAborted: true
        default: false
        }
    }
}
