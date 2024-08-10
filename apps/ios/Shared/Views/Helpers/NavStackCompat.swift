//
//  NavStackCompat.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 23/01/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct NavStackCompat <C: View, D: View>: View {
    let isActive: Binding<Bool>
    let destination: () -> D
    let content: () -> C

    var body: some View {
        if #available(iOS 16, *) {
            NavigationStack(path: Binding(
                get: { isActive.wrappedValue ? [true] : [] },
                set: { if $0.isEmpty { isActive.wrappedValue = false } }
            )) {
                ZStack {
                    NavigationLink(value: true) { EmptyView() }
                    content()
                }
                .navigationDestination(for: Bool.self) { show in
                    if show { destination() }
                }
            }
        } else {
            NavigationView {
                ZStack {
                    NavigationLink(
                        destination: destination(),
                        isActive: isActive
                    ) { EmptyView() }
                    content()
                }
            }
            .navigationViewStyle(.stack)
        }
    }
}
