//
//  LargeLinkPreviewView.swift
//  SimpleX
//
//  Created by JRoberts on 28/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct CIFileView: View {
    @Environment(\.colorScheme) var colorScheme
    let file: CIFile?

    var body: some View {
        Button(action: fileAction) {
            HStack(alignment: .center, spacing: 6) {
                fileIndicator()
                if let file = file {
                    VStack(alignment: .leading) {
                        Text(file.fileName)
                            .foregroundColor(.primary)
                        Text(formatBytes(bytes: file.fileSize))
                            .font(.caption)
                            .foregroundColor(.secondary)
                    }
                }
            }
            .padding(.top, 8)
            .padding(.horizontal, 12)
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
                        message: "Your contact sent file that is larger than currently supported maximum size (\(maxFileSize) bytes)."
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
            case .rcvInvitation: if fileSizeValid() { fileIcon("arrow.down.doc.fill") } else { largeFileIcon() }
            case .rcvAccepted: fileIcon("doc.fill.badge.ellipsis")
            case .rcvTransfer: ProgressView().frame(width: 40, height: 40)
            case .rcvCancelled: fileIcon("x.circle.fill")
            default: fileIcon("doc.fill")
            }
        } else {
            fileIcon("doc.circle.fill")
        }
    }

    func fileIcon(_ icon: String) -> some View {
        Image(systemName: icon)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: 29, height: 29)
            .foregroundColor(.secondary)
    }

    func largeFileIcon() -> some View {
        ZStack(alignment: .center) {
            Image(systemName: "doc.fill")
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 29, height: 29)
                .foregroundColor(.orange)
            Image(systemName: "exclamationmark")
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(maxHeight: 16)
                .frame(width: 29, height: 29)
                .foregroundColor(.white)
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
