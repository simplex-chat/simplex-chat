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
                    do {
                        try await Task.sleep(nanoseconds: NSEC_PER_SEC * 1)
                        print("task running")
                        await MainActor.run { selection = tag }
                    } catch {
                        print("task cancelled")
                    }
                }
                Task {
                    if let chat = ChatModel.shared.getChat(tag) {
                        try? await Task.sleep(nanoseconds: NSEC_PER_SEC * 5)
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
