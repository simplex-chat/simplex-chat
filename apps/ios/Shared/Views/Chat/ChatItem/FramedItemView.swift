//
//  FramedItemView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 01.12.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct FramedItemView<Content: View>: View {
    @Environment(\.colorScheme) var colorScheme
    var chatItem: ChatItem
    @ViewBuilder var content: Content
    
    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            if chatItem.meta.itemDeleted {
                HStack(alignment: .bottom, spacing: 0) {
                    Text("marked deleted")
                        .font(.caption)
                        .foregroundColor(.secondary)
                        .italic()
                }
                .padding(.horizontal, 8)
                .padding(.top, 6)
            }
            content
                .padding(.horizontal, 4)
                .padding(.bottom, 4)
        }
        .background(chatItemFrameColor(chatItem, colorScheme))
        .cornerRadius(18)
    }
}

struct FramedItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent, true, false), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent, true, false), revealed: Binding.constant(true))
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
