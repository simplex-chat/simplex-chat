//
//  CIChatFeatureView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/chat-view.md

import SwiftUI
import SimpleXChat

// Spec: spec/client/chat-view.md#CIChatFeatureView
struct CIChatFeatureView: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.revealed) var revealed: Bool
    @Environment(\.terminalLayout) private var terminal
    @ObservedObject var chat: Chat
    @ObservedObject var im: ItemsModel
    @EnvironmentObject var theme: AppTheme
    var chatItem: ChatItem
    var feature: Feature
    var icon: String? = nil
    var iconColor: Color

    var body: some View {
        if !revealed, let fs = mergedFeatures() {
            HStack {
                ForEach(fs, content: featureIconView)
            }
            .if(terminal) { $0.frame(height: TerminalLineBox.lineHeight(UIFont.preferredFont(forTextStyle: .caption1), theme.colors.secondary, theme)) }
            .padding(terminal ? terminalEventPadding : EdgeInsets(top: 6, leading: 6, bottom: 6, trailing: 6))
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

    private func mergedFeatures() -> [FeatureInfo]? {
        var fs: [FeatureInfo] = []
        var icons: Set<String> = []
        if var i = m.getChatItemIndex(im, chatItem) {
            while i < im.reversedChatItems.count,
                  let f = featureInfo(im.reversedChatItems[i]) {
                if !icons.contains(f.icon) {
                    fs.insert(f, at: 0)
                    icons.insert(f.icon)
                }
                i += 1
            }
        }
        return fs.count > 1 ? fs : nil
    }

    private func featureInfo(_ ci: ChatItem) -> FeatureInfo? {
        switch ci.content {
        case let .rcvChatFeature(feature, enabled, param): FeatureInfo(feature, enabled.iconColor(theme.colors.secondary), param)
        case let .sndChatFeature(feature, enabled, param): FeatureInfo(feature, enabled.iconColor(theme.colors.secondary), param)
        case let .rcvGroupFeature(feature, preference, param, role): FeatureInfo(feature, preference.enabled(role, for: chat.chatInfo.groupInfo?.membership).iconColor(theme.colors.secondary), param)
        case let .sndGroupFeature(feature, preference, param, role): FeatureInfo(feature, preference.enabled(role, for: chat.chatInfo.groupInfo?.membership).iconColor(theme.colors.secondary), param)
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
                chatEventText(Text(param), theme.colors.secondary).lineLimit(1)
            }
        } else {
            i
        }
    }

    private var fullFeatureView: some View {
        ChatEventWithMeta(chatItem: chatItem) { ts in
            HStack(alignment: terminal ? .center : .bottom, spacing: 4) {
                Image(systemName: icon ?? feature.iconFilled)
                    .foregroundColor(iconColor)
                    .scaleEffect(feature.iconScale)
                    .if(terminal) { $0.frame(height: 0) }
                chatEventText(chatItem, theme.colors.secondary, ts: ts)
                    .if(terminal) { $0.modifier(TerminalLineBox(UIFont.preferredFont(forTextStyle: .caption1), theme.colors.secondary, theme)) }
            }
            .padding(terminal ? terminalEventPadding : EdgeInsets(top: 4, leading: 6, bottom: 4, trailing: 6))
            .textSelection(.disabled)
        }
    }
}

struct CIChatFeatureView_Previews: PreviewProvider {
    static var previews: some View {
        let enabled = FeatureEnabled(forUser: false, forContact: false)
        CIChatFeatureView(
            chat: Chat.sampleData,
            im: ItemsModel.shared,
            chatItem: ChatItem.getChatFeatureSample(.fullDelete, enabled), feature: ChatFeature.fullDelete, iconColor: enabled.iconColor(.secondary)
        ).environment(\.revealed, true)
    }
}
