//
//  DetermineWidth.swift
//  SimpleX
//
//  Created by Evgeny on 14/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct DetermineWidth<K: PreferenceKey>: View where K.Value == CGFloat {
    var body: some View {
        GeometryReader { proxy in
            Color.clear
                .preference(
                    key: K.self,
                    value: proxy.size.width
                )
        }
    }
}
