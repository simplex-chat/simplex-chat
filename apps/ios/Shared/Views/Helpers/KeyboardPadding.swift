//
//  KeyboardPadding.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/07/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

extension View {
    @ViewBuilder func keyboardPadding() -> some View {
//        if #available(iOS 17.0, *) {
//            GeometryReader { g in
//                self.padding(.bottom, max(0, ChatModel.shared.keyboardHeight - g.safeAreaInsets.bottom))
//            }
//        } else {
            self
//        }
    }
}
