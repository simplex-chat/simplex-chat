//
//  CIMemberCreatedContactView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 19.09.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/chat-view.md

import SwiftUI
import SimpleXChat

// Spec: spec/client/chat-view.md#CIMemberCreatedContactView
struct CIMemberCreatedContactView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.terminalLayout) private var terminal
    var chatItem: ChatItem

    var body: some View {
        ChatEventWithMeta(chatItem: chatItem) { ts in
            HStack(alignment: .bottom, spacing: 0) {
                switch chatItem.chatDir {
                case let .groupRcv(groupMember):
                    if let contactId = groupMember.memberContactId {
                        memberCreatedContactView(openText: "Open", ts: ts)
                            .simultaneousGesture(TapGesture().onEnded {
                                ItemsModel.shared.loadOpenChat("@\(contactId)") {
                                    dismissAllSheets(animated: true)
                                }
                            })
                    } else {
                        memberCreatedContactView(ts: ts)
                    }
                default:
                    EmptyView()
                }
            }
            .if(terminal) { $0.lineBox(terminalLineBox(UIFont.preferredFont(forTextStyle: .caption1), theme.colors.secondary, theme)) }
            .padding(terminal ? terminalEventPadding : EdgeInsets(top: 0, leading: 6, bottom: 6, trailing: 0))
            .textSelection(.disabled)
        }
    }

    private func memberCreatedContactView(openText: LocalizedStringKey? = nil, ts: Text) -> some View {
        var r = eventText()
        if let openText {
            r = r
            + Text(openText)
                .fontWeight(.medium)
                .foregroundColor(theme.colors.primary)
            + Text(verbatim: "  ")
        }
        r = r + ts
            .fontWeight(.light)
            .foregroundColor(theme.colors.secondary)
        return r.font(.caption)
    }
    
    private func eventText() -> Text {
        if let member = chatItem.memberDisplayName {
            return Text(member + " " + chatItem.content.text + "  ")
                .fontWeight(.light)
                .foregroundColor(theme.colors.secondary)
        } else {
            return Text(chatItem.content.text + "  ")
                .fontWeight(.light)
                .foregroundColor(theme.colors.secondary)
        }
    }
}

struct CIMemberCreatedContactView_Previews: PreviewProvider {
    static var previews: some View {
        let content = CIContent.rcvGroupEvent(rcvGroupEvent: .memberCreatedContact)
        let chatItem = ChatItem(
            chatDir: .groupRcv(groupMember: GroupMember.sampleData),
            meta: CIMeta.getSample(1, .now, content.text, .rcvRead),
            content: content,
            quotedItem: nil,
            file: nil
        )
        CIMemberCreatedContactView(chatItem: chatItem)
    }
}
