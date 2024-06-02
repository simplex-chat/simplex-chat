//
//  CIFileView.swift
//  SimpleX
//
//  Created by JRoberts on 28/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIFileView: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme
    let file: CIFile?
    let edited: Bool

    var body: some View {
        let metaReserve = edited
        ? "                           "
        : "                       "
        Button(action: fileAction) {
            HStack(alignment: .bottom, spacing: 6) {
                fileIndicator()
                    .padding(.top, 5)
                    .padding(.bottom, 3)
                if let file = file {
                    let prettyFileSize = ByteCountFormatter.string(fromByteCount: file.fileSize, countStyle: .binary)
                    VStack(alignment: .leading, spacing: 2) {
                        Text(file.fileName)
                            .lineLimit(1)
                            .multilineTextAlignment(.leading)
                            .foregroundColor(.primary)
                        Text(prettyFileSize + metaReserve)
                            .font(.caption)
                            .lineLimit(1)
                            .multilineTextAlignment(.leading)
                            .foregroundColor(.secondary)
                    }
                } else {
                    Text(metaReserve)
                }
            }
            .padding(.top, 4)
            .padding(.bottom, 6)
            .padding(.leading, 10)
            .padding(.trailing, 12)
        }
        .disabled(!itemInteractive)
    }

    private var itemInteractive: Bool {
        if let file = file {
            switch (file.fileStatus) {
            case .sndStored: return file.fileProtocol == .local
            case .sndTransfer: return false
            case .sndComplete: return true
            case .sndCancelled: return false
            case .sndError: return false
            case .rcvInvitation: return true
            case .rcvAccepted: return true
            case .rcvTransfer: return false
            case .rcvAborted: return true
            case .rcvComplete: return true
            case .rcvCancelled: return false
            case .rcvError: return false
            case .invalid: return false
            }
        }
        return false
    }

    private func fileAction() {
        logger.debug("CIFileView fileAction")
        if let file = file {
            switch (file.fileStatus) {
            case .rcvInvitation, .rcvAborted:
                if fileSizeValid(file) {
                    Task {
                        logger.debug("CIFileView fileAction - in .rcvInvitation, .rcvAborted, in Task")
                        if let user = m.currentUser {
                            await receiveFile(user: user, fileId: file.fileId)
                        }
                    }
                } else {
                    let prettyMaxFileSize = ByteCountFormatter.string(fromByteCount: getMaxFileSize(file.fileProtocol), countStyle: .binary)
                    AlertManager.shared.showAlertMsg(
                        title: "Large file!",
                        message: "Your contact sent a file that is larger than currently supported maximum size (\(prettyMaxFileSize))."
                    )
                }
            case .rcvAccepted:
                switch file.fileProtocol {
                case .xftp:
                    AlertManager.shared.showAlertMsg(
                        title: "Waiting for file",
                        message: "File will be received when your contact completes uploading it."
                    )
                case .smp:
                    AlertManager.shared.showAlertMsg(
                        title: "Waiting for file",
                        message: "File will be received when your contact is online, please wait or check later!"
                    )
                case .local: ()
                }
            case .rcvComplete:
                logger.debug("CIFileView fileAction - in .rcvComplete")
                if let fileSource = getLoadedFileSource(file) {
                    saveCryptoFile(fileSource)
                }
            case .sndStored:
                logger.debug("CIFileView fileAction - in .sndStored")
                if file.fileProtocol == .local, let fileSource = getLoadedFileSource(file) {
                    saveCryptoFile(fileSource)
                }
            case .sndComplete:
                logger.debug("CIFileView fileAction - in .sndComplete")
                if let fileSource = getLoadedFileSource(file) {
                    saveCryptoFile(fileSource)
                }
            default: break
            }
        }
    }

    @ViewBuilder private func fileIndicator() -> some View {
        if let file = file {
            switch file.fileStatus {
            case .sndStored:
                switch file.fileProtocol {
                case .xftp: progressView()
                case .smp: fileIcon("doc.fill")
                case .local: fileIcon("doc.fill")
                }
            case let .sndTransfer(sndProgress, sndTotal):
                switch file.fileProtocol {
                case .xftp: progressCircle(sndProgress, sndTotal)
                case .smp: progressView()
                case .local: EmptyView()
                }
            case .sndComplete: fileIcon("doc.fill", innerIcon: "checkmark", innerIconSize: 10)
            case .sndCancelled: fileIcon("doc.fill", innerIcon: "xmark", innerIconSize: 10)
            case .sndError: fileIcon("doc.fill", innerIcon: "xmark", innerIconSize: 10)
            case .rcvInvitation:
                if fileSizeValid(file) {
                    fileIcon("arrow.down.doc.fill", color: .accentColor)
                } else {
                    fileIcon("doc.fill", color: .orange, innerIcon: "exclamationmark", innerIconSize: 12)
                }
            case .rcvAccepted: fileIcon("doc.fill", innerIcon: "ellipsis", innerIconSize: 12)
            case let .rcvTransfer(rcvProgress, rcvTotal):
                if file.fileProtocol == .xftp && rcvProgress < rcvTotal {
                    progressCircle(rcvProgress, rcvTotal)
                } else {
                    progressView()
                }
            case .rcvAborted:
                fileIcon("doc.fill", color: .accentColor, innerIcon: "exclamationmark.arrow.circlepath", innerIconSize: 12)
            case .rcvComplete: fileIcon("doc.fill")
            case .rcvCancelled: fileIcon("doc.fill", innerIcon: "xmark", innerIconSize: 10)
            case .rcvError: fileIcon("doc.fill", innerIcon: "xmark", innerIconSize: 10)
            case .invalid: fileIcon("doc.fill", innerIcon: "questionmark", innerIconSize: 10)
            }
        } else {
            fileIcon("doc.fill")
        }
    }

    private func fileIcon(_ icon: String, color: Color = Color(uiColor: .tertiaryLabel), innerIcon: String? = nil, innerIconSize: CGFloat? = nil) -> some View {
        ZStack(alignment: .center) {
            Image(systemName: icon)
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 30, height: 30)
                .foregroundColor(color)
            if let innerIcon = innerIcon,
               let innerIconSize = innerIconSize {
                Image(systemName: innerIcon)
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(maxHeight: 16)
                    .frame(width: innerIconSize, height: innerIconSize)
                    .foregroundColor(.white)
                    .padding(.top, 12)
            }
        }
    }

    private func progressView() -> some View {
        ProgressView().frame(width: 30, height: 30)
    }

    private func progressCircle(_ progress: Int64, _ total: Int64) -> some View {
        Circle()
            .trim(from: 0, to: Double(progress) / Double(total))
            .stroke(
                Color(uiColor: .tertiaryLabel),
                style: StrokeStyle(lineWidth: 3)
            )
            .rotationEffect(.degrees(-90))
            .frame(width: 30, height: 30)
    }
}

func fileSizeValid(_ file: CIFile?) -> Bool {
    if let file = file {
        return file.fileSize <= getMaxFileSize(file.fileProtocol)
    }
    return false
}

func saveCryptoFile(_ fileSource: CryptoFile) {
    if let cfArgs = fileSource.cryptoArgs {
        let url = getAppFilePath(fileSource.filePath)
        let tempUrl = getTempFilesDirectory().appendingPathComponent(fileSource.filePath)
        Task {
            do {
                try decryptCryptoFile(fromPath: url.path, cryptoArgs: cfArgs, toPath: tempUrl.path)
                await MainActor.run {
                    showShareSheet(items: [tempUrl]) {
                        removeFile(tempUrl)
                    }
                }
            } catch {
                await MainActor.run {
                    AlertManager.shared.showAlertMsg(title: "Error decrypting file", message: "Error: \(error.localizedDescription)")
                }
            }
        }
    } else {
        let url = getAppFilePath(fileSource.filePath)
        showShareSheet(items: [url])
    }
}

struct CIFileView_Previews: PreviewProvider {
    static var previews: some View {
        let sentFile: ChatItem = ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "", .sndSent(sndProgress: .complete), itemEdited: true),
            content: .sndMsgContent(msgContent: .file("")),
            quotedItem: nil,
            file: CIFile.getSample(fileStatus: .sndComplete)
        )
        let fileChatItemWtFile = ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "", .rcvRead),
            content: .rcvMsgContent(msgContent: .file("")),
            quotedItem: nil,
            file: nil
        )
        Group {
            ChatItemView(chat: Chat.sampleData, chatItem: sentFile, revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getFileMsgContentSample(), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getFileMsgContentSample(fileName: "some_long_file_name_here", fileStatus: .rcvInvitation), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvAccepted), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvTransfer(rcvProgress: 7, rcvTotal: 10)), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvCancelled), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getFileMsgContentSample(fileSize: 1_000_000_000, fileStatus: .rcvInvitation), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getFileMsgContentSample(text: "Hello there", fileStatus: .rcvInvitation), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getFileMsgContentSample(text: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.", fileStatus: .rcvInvitation), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: fileChatItemWtFile, revealed: Binding.constant(false))
        }
        .previewLayout(.fixed(width: 360, height: 360))
    }
}
