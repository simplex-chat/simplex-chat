//
//  ComposeView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

// TODO
//enum ComposeState {
//    case plain
//    case quoted(quotedItem: ChatItem)
//    case editing(editingItem: ChatItem)
//}

struct ComposeView: View {
    @Binding var message: String
    @Binding var quotedItem: ChatItem?
    @Binding var editingItem: ChatItem?
    var sendMessage: (String) -> Void
    var resetMessage: () -> Void
    var inProgress: Bool = false
    @FocusState.Binding var keyboardVisible: Bool
    @State var editing: Bool = false

    var body: some View {
        VStack(spacing: 0) {
            if (quotedItem != nil) {
                ContextItemView(contextItem: $quotedItem, editing: $editing)
            } else if (editingItem != nil) {
                ContextItemView(contextItem: $editingItem, editing: $editing, resetMessage: resetMessage)
            }
            SendMessageView(
                sendMessage: sendMessage,
                inProgress: inProgress,
                message: $message,
                keyboardVisible: $keyboardVisible,
                editing: $editing
            )
            .background(.background)
        }
        .onChange(of: editingItem == nil) { _ in
            editing = (editingItem != nil)
        }
    }
}

struct ComposeView_Previews: PreviewProvider {
    static var previews: some View {
        @State var message: String = ""
        @FocusState var keyboardVisible: Bool
        @State var item: ChatItem? = ChatItem.getSample(1, .directSnd, .now, "hello")
        @State var nilItem: ChatItem? = nil

        return Group {
            ComposeView(
                message: $message,
                quotedItem: $item,
                editingItem: $nilItem,
                sendMessage: { print ($0) },
                resetMessage: {},
                keyboardVisible: $keyboardVisible
            )
            ComposeView(
                message: $message,
                quotedItem: $nilItem,
                editingItem: $item,
                sendMessage: { print ($0) },
                resetMessage: {},
                keyboardVisible: $keyboardVisible
            )
        }
    }
}
