//
//  VoicePlayerCell.swift
//  SimpleX
//
//  Created by Suren Poghosyan on 20.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct VoicePlayerCell: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    
    @State var audioPlayer: AudioPlayer? = nil
    @State var playbackState: VoiceMessagePlaybackState = .noPlayback
    @State var playbackTime: TimeInterval? = nil
    @State var allowMenu: Bool = true
    @State private var seek: (TimeInterval) -> Void = { _ in }
    
    var chatItem: ChatItem
    var recordingFile: CIFile
    var duration: Int
    
    var body: some View {
        ZStack {
            Circle()
                .fill(chatItemFrameColor(chatItem, theme))
                .frame(width: 40, height: 40)
            
            VoiceMessagePlayer(
                chat: chat,
                chatItem: chatItem,
                recordingFile: recordingFile,
                recordingTime: TimeInterval(duration),
                showBackground: false,
                seek: $seek,
                audioPlayer: $audioPlayer,
                playbackState: $playbackState,
                playbackTime: $playbackTime,
                allowMenu: $allowMenu,
                sizeMultiplier: 0.71
            )
        }
        .id("\(chatItem.id)_\(playbackTime ?? 0)")
    }
}
