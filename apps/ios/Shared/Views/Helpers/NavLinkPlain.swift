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
    let chatId: ChatId
    @Binding var selection: ChatId?
    @ViewBuilder var label: () -> Label
    var disabled = false

    var body: some View {
        ZStack {
            Button("") { ItemsModel.shared.loadOpenChat(chatId) }
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
