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
                Image(systemName: determineIcon())
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: 40, height: 40)
                    .foregroundColor(.accentColor)
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

    func processFile() {
        logger.debug("CIFileView processFile")
        if let file = file {
            switch (file.fileStatus) {
            case .rcvInvitation:
                Task {
                    logger.debug("CIFileView processFile - in .rcvInvitation, in Task")
                    do {
                        try await receiveFile(fileId: file.fileId)
                    } catch {
                        logger.error("CIFileView.processFile - in .rcvInvitation error: \(error.localizedDescription)")
                    }
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

    func determineIcon() -> String {
        var icon = "doc.circle.fill"
        switch file?.fileStatus {
        case .rcvInvitation: icon = "arrow.down.circle.fill"
        case .rcvTransfer: icon = "ellipsis.circle.fill" // TODO animation
        case .rcvCancelled: icon = "x.circle.fill"
        default: icon = "doc.circle.fill"
        }
        return icon
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
