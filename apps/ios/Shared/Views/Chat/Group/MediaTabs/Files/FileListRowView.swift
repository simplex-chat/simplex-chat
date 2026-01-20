//
//  FileListRowView.swift
//  SimpleX
//
//  Created by Suren Poghosyan on 20.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct FileListRowView: View {
    let chatItem: ChatItem
    @EnvironmentObject var theme: AppTheme
    
    var body: some View {
        HStack(spacing: 12) {
            ZStack {
                Circle()
                    .fill(Color(.systemGray5))
                    .frame(width: 40, height: 40)
                
                Image(systemName: "doc.fill")
                    .font(.system(size: 20))
                    .foregroundColor(theme.colors.primary)
            }
            
            VStack(alignment: .leading, spacing: 4) {
                Text(itemTitle)
                    .font(.body)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)
                
                HStack(spacing: 8) {
                    Text(formatTimestampChatInfo(chatItem.meta.itemTs))
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                    
                    if let file = chatItem.file {
                        Text("• \(formatFileSize(file.fileSize))")
                            .font(.caption)
                            .foregroundColor(theme.colors.secondary)
                    }
                }
            }
            
            Spacer()
            
            Image(systemName: "chevron.right")
                .font(.system(size: 14))
                .foregroundColor(theme.colors.secondary)
        }
        .padding(.vertical, 8)
    }
    
    private var itemTitle: String {
        if let file = chatItem.file {
            return file.fileName
        }
        return "File"
    }
    
    private func formatFileSize(_ bytes: Int64) -> String {
        let formatter = ByteCountFormatter()
        formatter.countStyle = .file
        return formatter.string(fromByteCount: bytes)
    }
}
