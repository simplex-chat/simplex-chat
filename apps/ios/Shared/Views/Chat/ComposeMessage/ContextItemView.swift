//
//  ContextItemView.swift
//  SimpleX
//
//  Created by JRoberts on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ContextItemView: View {
    @Environment(\.colorScheme) var colorScheme
    @Binding var contextItem: ChatItem?

    var body: some View {
        if let cxtItem = contextItem {
            HStack {
                contextText(cxtItem).lineLimit(3)
                Spacer()
                Button {
                    withAnimation { contextItem = nil }
                } label: {
                    Image(systemName: "multiply")
                }
            }
            .padding(12)
            .frame(maxWidth: .infinity)
            .background(chatItemFrameColor(cxtItem, colorScheme))
            .padding(.top, 8)
        } else {
            EmptyView()
        }
    }
    
    func contextText(_ cxtItem: ChatItem) -> some View {
        if let s = cxtItem.memberDisplayName {
            return (Text(s).fontWeight(.medium) + Text(": \(cxtItem.content.text)"))
        } else {
            return Text(cxtItem.content.text)
        }
    }
}

struct ContextItemView_Previews: PreviewProvider {
    static var previews: some View {
        @State var contextItem: ChatItem? = ChatItem.getSample(1, .directSnd, .now, "hello")
        return ContextItemView(contextItem: $contextItem)
    }
}
