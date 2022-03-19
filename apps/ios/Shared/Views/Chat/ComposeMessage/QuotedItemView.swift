//
//  QuotedItemView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct QuotedItemView: View {
    @Environment(\.colorScheme) var colorScheme
    @Binding var quotedItem: ChatItem?

    var body: some View {
        if let qi = quotedItem {
            HStack {
                quoteText(qi).lineLimit(3)
                Spacer()
                Button {
                    withAnimation { quotedItem = nil }
                } label: {
                    Image(systemName: "multiply")
                }
            }
            .padding(12)
            .frame(maxWidth: .infinity)
            .background(chatItemFrameColor(qi, colorScheme))
            .padding(.top, 8)
        } else {
            EmptyView()
        }
    }
    
    func quoteText(_ qi: ChatItem) -> some View {
        if let s = qi.memberDisplayName {
            return (Text(s).fontWeight(.medium) + Text(": \(qi.content.text)"))
        } else {
            return Text(qi.content.text)
        }
    }
}

struct QuotedItemView_Previews: PreviewProvider {
    static var previews: some View {
        @State var quotedItem: ChatItem? = ChatItem.getSample(1, .directSnd, .now, "hello")
        return QuotedItemView(quotedItem: $quotedItem)
    }
}
