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
            let fs = mergedFeautures(merged)
            if fs.count > 1 {
                HStack {
                    ForEach(fs, content: featureIconView)
                }
                .padding(.horizontal, 6)
                .padding(.vertical, 6)
            } else {
                fullFeatureView
            }
        } else {
            fullFeatureView
        }
    }

    private struct FeatureInfo: Identifiable {
        var icon: String
        var scale: CGFloat
        var color: Color
        var param: String?

        init(_ f: Feature, _ color: Color, _ param: Int?) {
            self.icon = f.iconFilled
            self.scale = f.iconScale
            self.color = color
            self.param = f.hasParam && param != nil ? timeText(param) : nil
        }

        var id: String {
            "\(icon) \(color) \(param ?? "")"
        }
    }

    private func mergedFeautures(_ merged: CIMergedRange) -> [FeatureInfo] {
        var fs: [FeatureInfo] = []
        var i = merged.prevMerged
        let m = ChatModel.shared
        while i >= merged.currIndex {
            if i < m.reversedChatItems.count,
               let f = featureInfo(m.reversedChatItems[i]) {
                if let j = fs.firstIndex(where: { $0.icon == f.icon }) {
                    fs.remove(at: j)
                }
                fs.insert(f, at: 0)
            }
            i -= 1
        }
        return fs
    }

    private func featureInfo(_ ci: ChatItem) -> FeatureInfo? {
        switch ci.content {
        case let .rcvChatFeature(feature, enabled, param): FeatureInfo(feature, enabled.iconColor, param)
        case let .sndChatFeature(feature, enabled, param): FeatureInfo(feature, enabled.iconColor, param)
        case let .rcvGroupFeature(feature, preference, param): FeatureInfo(feature, preference.enable.iconColor, param)
        case let .sndGroupFeature(feature, preference, param): FeatureInfo(feature, preference.enable.iconColor, param)
        default: nil
        }
    }

    @ViewBuilder private func featureIconView(_ f: FeatureInfo) -> some View {
        let i = Image(systemName: f.icon)
            .foregroundColor(f.color)
            .scaleEffect(f.scale)
        if let param = f.param {
            HStack {
                i
                chatEventText(Text(param)).lineLimit(1)
            }
        } else {
            i
        }
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
