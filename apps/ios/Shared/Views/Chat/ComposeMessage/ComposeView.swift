//
//  ComposeView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ComposeView: View {
    @Binding var quotedItem: ChatItem?
    var sendMessage: (String) -> Void
    var inProgress: Bool = false
    @FocusState.Binding var keyboardVisible: Bool

    var body: some View {
        VStack(spacing: 0) {
            QuotedItemView(quotedItem: $quotedItem)
                .transition(.move(edge: .bottom))
            SendMessageView(
                sendMessage: sendMessage,
                inProgress: inProgress,
                keyboardVisible: $keyboardVisible
            )
            .background(.background)
        }
    }
}

struct ComposeView_Previews: PreviewProvider {
    static var previews: some View {
        @FocusState var keyboardVisible: Bool
        @State var quotedItem: ChatItem? = ChatItem.getSample(1, .directSnd, .now, "hello")

        return ComposeView(
            quotedItem: $quotedItem,
            sendMessage: { print ($0) },
            keyboardVisible: $keyboardVisible
        )
    }
}
