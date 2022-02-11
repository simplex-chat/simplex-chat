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

    var body: some View {
        HStack(alignment: .center, spacing: 4) {
            switch chatItem.meta.itemStatus {
            case .sndSent:
                Image(systemName: "checkmark")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .foregroundColor(.secondary)
                    .frame(maxHeight: 8)
            case .rcvNew:
                Image(systemName: "circlebadge.fill")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .foregroundColor(Color.accentColor)
                    .frame(maxHeight: 8)
            default:
                EmptyView()
            }

            Text(chatItem.timestampText)
                .font(.caption)
                .foregroundColor(.secondary)
        }
    }
}

struct CIMetaView_Previews: PreviewProvider {
    static var previews: some View {
        CIMetaView(chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent))
    }
}
