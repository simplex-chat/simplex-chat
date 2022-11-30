//
//  MarkedDeletedItemView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 30.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct MarkedDeletedItemView: View {
    var chatItem: ChatItem
    var showMember = false

    var body: some View {
        let metaReserve = chatItem.meta.itemEdited
          ? "                     "
          : "                 "
        ZStack(alignment: .bottomTrailing) {
            VStack(alignment: .leading) {
                HStack(alignment: .bottom, spacing: 0) {
                    if showMember, let member = chatItem.memberDisplayName {
                        Text(member).fontWeight(.medium) + Text(": ")
                    }
                    Text("marked deleted")
                        .foregroundColor(.secondary)
                        .italic()
                }
                HStack {
                    Text("Tap to view")
                        .font(.caption)
                        .foregroundColor(.accentColor)
                    Text(metaReserve)
                }
            }
            CIMetaView(chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .padding(.leading, 12)
        .padding(.vertical, 6)
        .background(Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(18)
        .textSelection(.disabled)
    }
}

struct MarkedDeletedItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            MarkedDeletedItemView(chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent, true, false))
        }
        .previewLayout(.fixed(width: 360, height: 200))
    }
}
