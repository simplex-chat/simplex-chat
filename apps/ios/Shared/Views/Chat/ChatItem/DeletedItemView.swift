//
//  FramedItemView.swift
//  SimpleX
//
//  Created by JRoberts on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct DeletedItemView: View {
    @Environment(\.colorScheme) var colorScheme
    var chatItem: ChatItem
    @State var msgWidth: CGFloat = 0

    var body: some View {
        ZStack(alignment: .bottomTrailing) {
            VStack(alignment: .leading, spacing: 0) {
                Text(chatItem.content.text)
                    .padding(.vertical, 6)
                    .padding(.horizontal, 12)
                    .padding(.bottom, 18)
                    .frame(minWidth: 0, alignment: .leading)
                    .textSelection(.disabled)
            }
            CIMetaView(chatItem: chatItem)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
        }
        .background(Color(uiColor: .systemBackground))
        .overlay(
            RoundedRectangle(cornerRadius: 18)
                .stroke(.secondary, lineWidth: 1)
        )
    }
}

struct DeletedItemView_Previews: PreviewProvider {
    static var previews: some View {
        DeletedItemView(chatItem: ChatItem.getDeletedContentSample())
    }
}
