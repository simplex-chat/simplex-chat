//
//  VoiceListRowView.swift
//  SimpleX
//
//  Created by Suren Poghosyan on 20.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct VoiceListRowView: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
  
    var chatItem: ChatItem
    
    var body: some View {
        HStack(spacing: 12) {
            if let recordingFile = chatItem.file, let duration = voiceDuration {
                VoicePlayerCell(
                    chat: chat,
                    chatItem: chatItem,
                    recordingFile: recordingFile,
                    duration: duration
                )
            } else {
                ZStack {
                    Circle()
                        .fill(chatItemFrameColor(chatItem, theme))
                        .frame(width: 40, height: 40)
                    
                    Image(systemName: "waveform")
                        .font(.system(size: 20))
                        .foregroundColor(theme.colors.primary)
                }
            }
            
            VStack(alignment: .leading, spacing: 4) {
                Text("Voice Message")
                    .font(.body)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)
                
                HStack(spacing: 8) {
                    Text(formatTimestampChatInfo(chatItem.meta.itemTs))
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                    
                    if let duration = voiceDuration {
                        Text("• \(formatDuration(duration))")
                            .font(.caption)
                            .foregroundColor(theme.colors.secondary)
                    }
                }
            }
            
            Spacer()
        }
        .padding(.vertical, 8)
    }
    
    private var voiceDuration: Int? {
        if case let .voice(_, duration) = chatItem.content.msgContent {
            return duration
        }
        return nil
    }
    
    private func formatDuration(_ seconds: Int) -> String {
        let mins = seconds / 60
        let secs = seconds % 60
        return String(format: "%d:%02d", mins, secs)
    }
}
