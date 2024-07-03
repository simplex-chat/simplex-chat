//
//  ViewModifiers.swift
//  SimpleX (iOS)
//
//  Created by Avently on 12.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

extension View {
    @ViewBuilder func `if`<Content: View>(_ condition: @autoclosure () -> Bool, transform: (Self) -> Content) -> some View {
        if condition() {
            transform(self)
        } else {
            self
        }
    }
}
