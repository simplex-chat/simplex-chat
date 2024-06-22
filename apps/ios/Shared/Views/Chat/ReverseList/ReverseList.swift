//
//  ReverseList.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 11/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

/// A List, which displays it's items in reverse order - from bottom to top
struct ReverseList<Item: Hashable & Sendable, Content: View>: UIViewControllerRepresentable {

    let items: Array<Item>

    @Binding var scroll: Scroll

    /// Closure, that returns user interface for a given item
    let content: (Item) -> Content

    /// Callback for when a new cell is loaded
    let loadedCell: (IndexPath) -> Void

    func makeUIViewController(context: Context) -> Controller {
        Controller(representer: self)
    }

    func updateUIViewController(_ controller: Controller, context: Context) {
        if case .scrollingToBottom = scroll {
            controller.scroll()
        } else {
            controller.update(items: items)
        }
    }
}

extension ReverseList {
    /// Represents Scroll State of ``ReverseList``
    enum Scroll: Equatable {
        case scrollingToBottom
        case isNearBottom(Bool)

        var isAtBottom: Bool {
            switch self {
            case .scrollingToBottom: false
            case let .isNearBottom(bool): bool
            }
        }
    }
}
