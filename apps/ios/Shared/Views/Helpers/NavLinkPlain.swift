//
//  NavLinkPlain.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct NavLinkPlain<V: Hashable, Label: View>: View {
    @State var tag: V
    @Binding var selection: V?
    @ViewBuilder var label: () -> Label
    var disabled = false

    var body: some View {
        ZStack {
            Button("") { DispatchQueue.main.async { selection = tag } }
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
