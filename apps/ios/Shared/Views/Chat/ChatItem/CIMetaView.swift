//
//  CIMetaView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct CIMetaView: View {
    var chatItem: ChatItem
    var metaColor = Color.secondary

    var body: some View {
        HStack(alignment: .center, spacing: 4) {
            if !chatItem.isDeletedContent() {
                if chatItem.meta.itemEdited {
                    statusImage("pencil", metaColor, 9)
                }

                switch chatItem.meta.itemStatus {
                case .sndSent:
                    statusImage("checkmark", metaColor)
                case .sndErrorAuth:
                    statusImage("multiply", .red)
                case .sndError:
                    statusImage("exclamationmark.triangle.fill", .yellow)
                case .rcvNew:
                    statusImage("circlebadge.fill", Color.accentColor)
                default: EmptyView()
                }
            }

            chatItem.timestampText
                .font(.caption)
                .foregroundColor(metaColor)
        }
    }

    private func statusImage(_ systemName: String, _ color: Color, _ maxHeight: CGFloat = 8) -> some View {
        Image(systemName: systemName)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .foregroundColor(color)
            .frame(maxHeight: maxHeight)
    }
}

struct CIMetaView_Previews: PreviewProvider {
    static var previews: some View {
        return Group {
            CIMetaView(chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent))
            CIMetaView(chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent, false, true))
            CIMetaView(chatItem: ChatItem.getDeletedContentSample())
        }
        .previewLayout(.fixed(width: 360, height: 100))
    }
}
