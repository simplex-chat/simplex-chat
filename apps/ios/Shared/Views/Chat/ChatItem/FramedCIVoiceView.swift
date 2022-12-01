//
//  FramedCIVoiceView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 22.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

import SwiftUI
import SimpleXChat

struct FramedCIVoiceView: View {
    var chatItem: ChatItem
    let recordingFile: CIFile?
    let duration: Int
    @State var playbackState: VoiceMessagePlaybackState = .noPlayback
    @State var playbackTime: TimeInterval?

    var body: some View {
        HStack {
            VoiceMessagePlayer(
                chatItem: chatItem,
                recordingFile: recordingFile,
                recordingTime: TimeInterval(duration),
                showBackground: false,
                playbackState: $playbackState,
                playbackTime: $playbackTime
            )
            VoiceMessagePlayerTime(
                recordingTime: TimeInterval(duration),
                playbackState: $playbackState,
                playbackTime: $playbackTime
            )
            .foregroundColor(.secondary)
            .frame(width: 50, alignment: .leading)
        }
        .padding(.top, 6)
        .padding(.leading, 6)
        .padding(.trailing, 12)
        .padding(.bottom, chatItem.content.text.isEmpty ? 10 : 0)
    }
}

struct FramedCIVoiceView_Previews: PreviewProvider {
    static var previews: some View {
        let sentVoiceMessage: ChatItem = ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "", .sndSent, false, true, false),
            content: .sndMsgContent(msgContent: .voice(text: "Hello there", duration: 30)),
            quotedItem: nil,
            file: CIFile.getSample(fileStatus: .sndComplete)
        )
        let voiceMessageWithQuote: ChatItem = ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "", .sndSent, false, true, false),
            content: .sndMsgContent(msgContent: .voice(text: "", duration: 30)),
            quotedItem: CIQuote.getSample(1, .now, "Hi", chatDir: .directRcv),
            file: CIFile.getSample(fileStatus: .sndComplete)
        )
        Group {
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: sentVoiceMessage, revealed: Binding.constant(false))
                .environmentObject(ChatModel())
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getVoiceMsgContentSample(text: "Hello there"), revealed: Binding.constant(false))
                .environmentObject(ChatModel())
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getVoiceMsgContentSample(text: "Hello there", fileStatus: .rcvTransfer), revealed: Binding.constant(false))
                .environmentObject(ChatModel())
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getVoiceMsgContentSample(text: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), revealed: Binding.constant(false))
                .environmentObject(ChatModel())
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: voiceMessageWithQuote, revealed: Binding.constant(false))
                .environmentObject(ChatModel())
        }
        .previewLayout(.fixed(width: 360, height: 360))
    }
}
