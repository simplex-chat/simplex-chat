//
//  CIFileView.swift
//  SimpleX
//
//  Created by JRoberts on 28/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct CIFileView: View {
    @Environment(\.colorScheme) var colorScheme
    let file: CIFile?
    let edited: Bool

    var body: some View {
        let metaReserve = edited
          ? "                         "
          : "                     "
        Button(action: fileAction) {
            HStack(alignment: .bottom, spacing: 6) {
                fileIndicator()
                    .padding(.top, 5)
                    .padding(.bottom, 3)
                if let file = file {
                    VStack(alignment: .leading, spacing: 2) {
                        Text(file.fileName)
                            .lineLimit(1)
                            .multilineTextAlignment(.leading)
                            .foregroundColor(.primary)
                        Text(formatBytes(bytes: file.fileSize) + metaReserve)
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
        .disabled(file == nil || (file?.fileStatus != .rcvInvitation && file?.fileStatus != .rcvAccepted && file?.fileStatus != .rcvComplete))
    }

    func fileSizeValid() -> Bool {
        if let file = file {
            return file.fileSize <= maxFileSize
        }
        return false
    }

    func fileAction() {
        logger.debug("CIFileView processFile")
        if let file = file {
            switch (file.fileStatus) {
            case .rcvInvitation:
                if fileSizeValid() {
                    Task {
                        logger.debug("CIFileView processFile - in .rcvInvitation, in Task")
                        await receiveFile(fileId: file.fileId)
                    }
                } else {
                    AlertManager.shared.showAlertMsg(
                        title: "Large file!",
                        message: "Your contact sent a file that is larger than currently supported maximum size (\(maxFileSize) bytes)."
                    )
                }
            case .rcvAccepted:
                AlertManager.shared.showAlertMsg(
                    title: "Waiting for file",
                    message: "File will be received when your contact is online, please wait or check later!"
                )
            case .rcvComplete:
                logger.debug("CIFileView processFile - in .rcvComplete")
                if let filePath = getStoredFilePath(file){
                    let url = URL(fileURLWithPath: filePath)
                    showShareSheet(items: [url])
                }
            default: break
            }
        }
    }

    @ViewBuilder func fileIndicator() -> some View {
        if let file = file {
            switch file.fileStatus {
            case .sndCancelled: fileIcon("doc.fill", innerIcon: "xmark", innerIconSize: 10)
            case .rcvInvitation:
                if fileSizeValid() {
                    fileIcon("arrow.down.doc.fill", color: .accentColor)
                } else {
                    fileIcon("doc.fill", color: .orange, innerIcon: "exclamationmark", innerIconSize: 12)
                }
            case .rcvAccepted: fileIcon("doc.fill", innerIcon: "ellipsis", innerIconSize: 12)
            case .rcvTransfer: ProgressView().frame(width: 30, height: 30)
            case .rcvCancelled: fileIcon("doc.fill", innerIcon: "xmark", innerIconSize: 10)
            default: fileIcon("doc.fill")
            }
        } else {
            fileIcon("doc.fill")
        }
    }

    func fileIcon(_ icon: String, color: Color = .secondary, innerIcon: String? = nil, innerIconSize: CGFloat? = nil) -> some View {
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

    func formatBytes(bytes: Int64) -> String {
        if (bytes == 0) { return "0 bytes" }

        let bytesDouble = Double(bytes)
        let k: Double = 1000
        let units = ["bytes", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"]

        let i = floor(log2(bytesDouble) / log2(k))
        let size = bytesDouble / pow(k, i)
        let unit = units[Int(i)]

        if (i <= 1) {
            return String(format: "%.0f \(unit)", size)
        } else {
            return String(format: "%.2f \(unit)", size)
        }
    }
}

struct CIFileView_Previews: PreviewProvider {
    static var previews: some View {
        let sentFile = ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "", .sndSent, false, true, false),
            content: .sndMsgContent(msgContent: .file("")),
            quotedItem: nil,
            file: CIFile.getSample(fileStatus: .sndStored)
        )
        let fileChatItemWtFile = ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "", .rcvRead, false, false, false),
            content: .rcvMsgContent(msgContent: .file("")),
            quotedItem: nil,
            file: nil
        )
        Group{
            ChatItemView(chatItem: sentFile)
            ChatItemView(chatItem: ChatItem.getFileMsgContentSample())
            ChatItemView(chatItem: ChatItem.getFileMsgContentSample(fileName: "some_long_file_name_here", fileStatus: .rcvInvitation))
            ChatItemView(chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvAccepted))
            ChatItemView(chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvTransfer))
            ChatItemView(chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvCancelled))
            ChatItemView(chatItem: ChatItem.getFileMsgContentSample(fileSize: 2000000, fileStatus: .rcvInvitation))
            ChatItemView(chatItem: ChatItem.getFileMsgContentSample(text: "Hello there", fileStatus: .rcvInvitation))
            ChatItemView(chatItem: ChatItem.getFileMsgContentSample(text: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.", fileStatus: .rcvInvitation))
            ChatItemView(chatItem: fileChatItemWtFile)
        }
        .previewLayout(.fixed(width: 360, height: 360))
    }
}
