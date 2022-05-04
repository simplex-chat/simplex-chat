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
    let contextItem: ChatItem
    let cancelContextItem: () -> Void

    var body: some View {
        HStack {
            contextText(contextItem).lineLimit(3)
            Spacer()
            Button {
                withAnimation {
                    cancelContextItem()
                }
            } label: {
                Image(systemName: "multiply")
            }
        }
        .padding(12)
        .frame(maxWidth: .infinity)
        .background(chatItemFrameColor(contextItem, colorScheme))
        .padding(.top, 8)
    }
    
    func contextText(_ cxtItem: ChatItem) -> some View {
        if let s = cxtItem.memberDisplayName {
            return (Text(s).fontWeight(.medium) + Text(": \(cxtItem.text)"))
        } else {
            return Text(cxtItem.text)
        }
    }
}

struct ContextItemView_Previews: PreviewProvider {
    static var previews: some View {
        let contextItem: ChatItem = ChatItem.getSample(1, .directSnd, .now, "hello")
        return ContextItemView(contextItem: contextItem, cancelContextItem: {})
    }
}
