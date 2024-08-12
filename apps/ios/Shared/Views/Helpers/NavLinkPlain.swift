//
//  NavLinkPlain.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct NavLinkPlain<Label: View>: View {
    @State var tag: ChatId
    @Binding var selection: ChatId?
    @ViewBuilder var label: () -> Label
    var disabled = false

    var body: some View {
        ZStack {
            Button("") {
                let navigationTimeout = Task {
                    try? await Task.sleep(nanoseconds: NSEC_PER_SEC / 2)
                    await MainActor.run { selection = tag }
                }
                Task {
                    if let chat = ChatModel.shared.getChat(tag) {
                        await loadChat(chat: chat)
                        navigationTimeout.cancel()
                        await MainActor.run { selection = tag }
                    }
                }
            }
            .disabled(disabled)
            label()
        }
    }
}

//struct NavLinkPlain_Previews: PreviewProvider {
//    static var previews: some View {
//        NavLinkPlain()
//    }
//}
