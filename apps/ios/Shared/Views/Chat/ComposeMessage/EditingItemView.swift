//
//  EditingItemView.swift
//  SimpleX
//
//  Created by JRoberts on 23.03.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct EditingItemView: View {
    @Environment(\.colorScheme) var colorScheme
    @Binding var editingItem: ChatItem?

    var body: some View {
        if let ei = editingItem {
            HStack {
                Text(ei.content.text).lineLimit(3)
                Spacer()
                Button {
                    withAnimation { editingItem = nil }
                } label: {
                    Image(systemName: "multiply")
                }
            }
            .padding(12)
            .frame(maxWidth: .infinity)
            .background(chatItemFrameColor(ei, colorScheme))
            .padding(.top, 8)
        } else {
            EmptyView()
        }
    }
}

struct EditingItemView_Previews: PreviewProvider {
    static var previews: some View {
        @State var editingItem: ChatItem? = ChatItem.getSample(1, .directSnd, .now, "hello")
        return EditingItemView(editingItem: $editingItem)
    }
}
