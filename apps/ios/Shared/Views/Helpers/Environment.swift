//
//  Environment.swift
//  SimpleX (iOS)
//
//  Created by Avently on 26.07.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct FontSizeMultiplierKey: EnvironmentKey {
    static let defaultValue: CGFloat = 1
}

struct FontSizeSqrtMultiplierKey: EnvironmentKey {
    static let defaultValue: CGFloat = 1
}

extension EnvironmentValues {
    var fontSizeMultiplier: CGFloat {
        get { self[FontSizeMultiplierKey.self] }
        set { self[FontSizeMultiplierKey.self] = newValue }
    }
    var fontSizeSqrtMultiplier: CGFloat {
        get { self[FontSizeSqrtMultiplierKey.self] }
        set { self[FontSizeSqrtMultiplierKey.self] = newValue }
    }
}
