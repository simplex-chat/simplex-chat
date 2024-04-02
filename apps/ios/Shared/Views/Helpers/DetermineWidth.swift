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
        GeometryReader { proxy in
            Color.clear
                .preference(
                    key: MaximumWidthPreferenceKey.self,
                    value: proxy.size.width
                )
        }
    }
}

struct DetermineWidthImageVideoItem: View {
    typealias Key = MaximumWidthImageVideoPreferenceKey
    var body: some View {
        GeometryReader { proxy in
            Color.clear
                .preference(
                    key: MaximumWidthImageVideoPreferenceKey.self,
                    value: proxy.size.width
                )
        }
    }
}

struct MaximumWidthPreferenceKey: PreferenceKey {
    static var defaultValue: CGFloat = 0
    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
        value = max(value, nextValue())
    }
}

struct MaximumWidthImageVideoPreferenceKey: PreferenceKey {
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
