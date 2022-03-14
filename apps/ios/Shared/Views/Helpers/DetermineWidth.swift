//
//  DetermineWidth.swift
//  SimpleX
//
//  Created by Evgeny on 14/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct DetermineWidth: View {
    typealias Key = MaximumWidthPreferenceKey
    var body: some View {
        GeometryReader {
            proxy in
            Color.clear
                .anchorPreference(key: Key.self, value: .bounds) {
                    anchor in proxy[anchor].size.width
                }
        }
    }
}

struct MaximumWidthPreferenceKey: PreferenceKey {
    static var defaultValue: CGFloat = 0
    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
        value = max(value, nextValue())
    }
}

struct DetermineWidth_Previews: PreviewProvider {
    static var previews: some View {
        DetermineWidth()
    }
}
