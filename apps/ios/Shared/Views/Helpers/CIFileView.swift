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
        Button(action: processFile) {
            HStack(alignment: .center, spacing: 6) {
                fileIndicator()
                if let file = file {
                    VStack(alignment: .leading) {
                        Text(file.fileName)
                            .foregroundColor(.primary)
                        Text(formatBytes(bytes: file.fileSize))
                            .font(.caption)
                            .foregroundColor(.primary)
                    }
                }
            }
            .padding(.top, 8)
            .padding(.horizontal, 12)
        }
        .disabled(file == nil || (file?.fileStatus != .rcvInvitation && file?.fileStatus != .rcvComplete))
    }

    func fileSizeValid() -> Bool {
        if let file = file,
           file.fileSize <= maxFileSize {
            return true
        } else {
            return false
        }
    }

    func processFile() {
        logger.debug("CIFileView processFile")
        if let file = file {
            switch (file.fileStatus) {
            case .rcvInvitation:
                if fileSizeValid() {
                    Task {
                        logger.debug("CIFileView processFile - in .rcvInvitation, in Task")
                        do {
                            try await receiveFile(fileId: file.fileId)
                        } catch {
                            logger.error("CIFileView.processFile - in .rcvInvitation error: \(error.localizedDescription)")
                        }
                    }
                } else {
                    AlertManager.shared.showAlertMsg(
                        title: "Invalid file size",
                        message: "Your contact wants to send a file larger than supported size (\(maxFileSize) bytes)."
                    )
                }
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
            case .rcvInvitation: fileIcon(fileSizeValid() ? "arrow.down.circle.fill" : "exclamationmark.triangle.fill")
            case .rcvAccepted: fileIcon("link.circle.fill")
            case .rcvTransfer: ProgressView().progressViewStyle(.circular).frame(width: 40, height: 40) // TODO pretty spinner
            case .rcvCancelled: fileIcon("x.circle.fill")
            default: fileIcon("doc.circle.fill")
            }
        } else {
            fileIcon("doc.circle.fill")
        }
    }

    func fileIcon(_ icon: String) -> some View {
        Image(systemName: icon)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: 40, height: 40)
            .foregroundColor(fileSizeValid() ? .accentColor : .red)
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
