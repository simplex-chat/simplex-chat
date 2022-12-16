//
//  CIChatFeatureView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIChatFeatureView: View {
    var chatItem: ChatItem
    var feature: Feature
    var iconColor: Color

    var body: some View {
        HStack(alignment: .bottom, spacing: 4) {
            Image(systemName: feature.iconFilled)
                .foregroundColor(iconColor)
            chatEventText(chatItem)
        }
        .padding(.leading, 6)
        .padding(.bottom, 6)
        .textSelection(.disabled)
    }
}

struct CIChatFeatureView_Previews: PreviewProvider {
    static var previews: some View {
        let enabled = FeatureEnabled(forUser: false, forContact: false)
        CIChatFeatureView(chatItem: ChatItem.getChatFeatureSample(.fullDelete, enabled), feature: ChatFeature.fullDelete, iconColor: enabled.iconColor)
    }
}
