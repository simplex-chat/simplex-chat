//
//  Test.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 31/08/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

extension View {
    @ViewBuilder
    func invertedForegroundStyle(enabled: Bool = true) -> some View {
        if enabled {
            foregroundStyle(Material.ultraThin)
                .environment(\.colorScheme, .dark)
                .grayscale(1)
                .brightness(0.00)
                .contrast(-20)
        } else { self }
    }
}

extension Text {
    // Applying `foregroundColor(nil)` breaks `.invertedForegroundStyle` modifier
   func foreground(_ color: Color?) -> Text {
        if let color {
           foregroundColor(color)
        } else {
            self
        }
    }
}
