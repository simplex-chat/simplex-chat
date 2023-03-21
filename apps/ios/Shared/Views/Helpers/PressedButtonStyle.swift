//
// Created by Avently on 16.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct PressedButtonStyle: ButtonStyle {
    var defaultColor: Color
    var pressedColor: Color
    func makeBody(configuration: Self.Configuration) -> some View {
        configuration.label
        .background(configuration.isPressed ? pressedColor : defaultColor)
    }
}
