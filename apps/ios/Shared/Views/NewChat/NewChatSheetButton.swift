//
//  NewChatSheetButton.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct NewChatSheetButton: View {
    @Binding var showNewChatSheet: Bool

    var body: some View {
        Button {
            showNewChatSheet = true
        } label: {
            Image(systemName: "square.and.pencil")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
        }
        .sheet(isPresented: $showNewChatSheet) {
            NewChatView(selection: .invite)
        }
    }
}

//#Preview {
//    NewChatSheetButton()
//}
