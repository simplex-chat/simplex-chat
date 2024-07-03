//
//  TranslateSheet.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 03/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import Translation

extension View {
    @ViewBuilder func translateSheet(text: Binding<String?>) -> some View {
        if #available(iOS 17.4, *) {
            translationPresentation(
                isPresented: Binding(
                    get: { text.wrappedValue != nil },
                    set: { if !$0 { text.wrappedValue = nil } }
                ),
                text: text.wrappedValue ?? String()
            )
        } else {
            self
        }
    }
}
