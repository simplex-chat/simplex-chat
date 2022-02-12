//
//  NavLinkPlain.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct NavLinkPlain<V: Hashable, Destination: View, Label: View>: View {
    @State var tag: V
    @Binding var selection: V?
    @ViewBuilder var destination: () -> Destination
    @ViewBuilder var label: () -> Label
    var disabled = false

    var body: some View {
        ZStack {
            Button("") { selection = tag }
                .disabled(disabled)
            label()
        }
        .background {
            NavigationLink("", tag: tag, selection: $selection, destination: destination)
                .hidden()
        }
    }
}

//struct NavLinkPlain_Previews: PreviewProvider {
//    static var previews: some View {
//        NavLinkPlain()
//    }
//}
