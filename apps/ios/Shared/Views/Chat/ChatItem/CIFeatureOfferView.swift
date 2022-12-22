//
//  CIFeatureOfferView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIFeatureOfferView: View {
    var chatItem: ChatItem
    var feature: Feature
    var param: Int?

    var body: some View {
        HStack(alignment: .bottom, spacing: 4) {
            Image(systemName: feature.icon)
                .foregroundColor(.secondary)
            Text(CIContent.featureOfferText(feature, param))
                .font(.caption)
                .foregroundColor(.secondary)
                .fontWeight(.light)
            Button("Enable") {

            }
            .font(.caption)
            chatItem.timestampText
                .font(.caption)
                .foregroundColor(Color.secondary)
                .fontWeight(.light)
        }
        .padding(.leading, 6)
        .padding(.bottom, 6)
        .textSelection(.disabled)
    }
}

struct CIFeatureOfferView_Previews: PreviewProvider {
    static var previews: some View {
        let content = CIContent.rcvFeatureOffer(feature: .timedMessages, param: 30)
        let chatItem = ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, content.text, .rcvRead, false, false, false),
            content: content,
            quotedItem: nil,
            file: nil
        )
        CIFeatureOfferView(chatItem: chatItem, feature: ChatFeature.timedMessages, param: 30)
    }
}
