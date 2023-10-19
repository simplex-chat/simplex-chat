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
    var icon: String? = nil
    var iconColor: Color
    var mergedRange: CIMergedRange?

    var body: some View {
        if let merged = mergedRange, merged.many {
            let m = ChatModel.shared
            HStack {
                ForEach(m.reversedChatItems[merged.range], id: \.viewId, content: featureView)
            }
            .padding(.horizontal, 6)
            .padding(.vertical, 6)
        } else {
            fullFeatureView
        }
    }

    @ViewBuilder private func featureView(_ ci: ChatItem) -> some View {
        switch ci.content {
        case let .rcvChatFeature(feature, enabled, _): featureIconView(feature, enabled.iconColor)
        case let .sndChatFeature(feature, enabled, _): featureIconView(feature, enabled.iconColor)
        case let .rcvGroupFeature(feature, preference, _): featureIconView(feature, preference.enable.iconColor)
        case let .sndGroupFeature(feature, preference, _): featureIconView(feature, preference.enable.iconColor)
        default: EmptyView()
        }
    }

    private func featureIconView(_ f: Feature, _ color: Color) -> some View {
        Image(systemName: f.iconFilled)
            .foregroundColor(color)
            .scaleEffect(f.iconScale)
    }

    private var fullFeatureView: some View {
        HStack(alignment: .bottom, spacing: 4) {
            Image(systemName: icon ?? feature.iconFilled)
                .foregroundColor(iconColor)
                .scaleEffect(feature.iconScale)
            chatEventText(chatItem)
        }
        .padding(.horizontal, 6)
        .padding(.vertical, 4)
        .textSelection(.disabled)
    }
}

struct CIChatFeatureView_Previews: PreviewProvider {
    static var previews: some View {
        let enabled = FeatureEnabled(forUser: false, forContact: false)
        CIChatFeatureView(chatItem: ChatItem.getChatFeatureSample(.fullDelete, enabled), feature: ChatFeature.fullDelete, iconColor: enabled.iconColor)
    }
}
