//
//  CIMemberCreatedContactView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 19.09.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIMemberCreatedContactView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    var chatItem: ChatItem
    
    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            switch chatItem.chatDir {
            case let .groupRcv(groupMember):
                if let contactId = groupMember.memberContactId {
                    memberCreatedContactView(openText: "Open")
                        .onTapGesture {
                            dismissAllSheets(animated: true)
                            DispatchQueue.main.async {
                                m.chatId = "@\(contactId)"
                            }
                        }
                } else {
                    memberCreatedContactView()
                }
            default:
                EmptyView()
            }
        }
        .padding(.leading, 6)
        .padding(.bottom, 6)
        .textSelection(.disabled)
    }
    
    private func memberCreatedContactView(openText: LocalizedStringKey? = nil) -> some View {
        var r = eventText()
        if let openText {
            r = r
            + Text(openText)
                .fontWeight(.medium)
                .foregroundColor(theme.colors.primary)
            + Text("  ")
        }
        r = r + chatItem.timestampText
            .fontWeight(.light)
            .foregroundColor(.secondary)
        return r.font(.caption)
    }
    
    private func eventText() -> Text {
        if let member = chatItem.memberDisplayName {
            return Text(member + " " + chatItem.content.text + "  ")
                .fontWeight(.light)
                .foregroundColor(.secondary)
        } else {
            return Text(chatItem.content.text + "  ")
                .fontWeight(.light)
                .foregroundColor(.secondary)
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
