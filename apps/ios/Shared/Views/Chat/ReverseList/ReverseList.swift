//
//  ReverseList.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 11/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

/// A List, which displays it's items in reverse order - from bottom to top
struct ReverseList<Item: Identifiable & Hashable & Sendable, Content: View>: UIViewControllerRepresentable {

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
        if case let .scrollingTo(destination) = scroll, !items.isEmpty {
            switch destination {
            case let .item(id):
                controller.scroll(to: items.firstIndex(where: { $0.id == id }))
            case .bottom:
                controller.scroll(to: .zero)
            }
        } else {
            controller.update(items: items)
        }
    }
}

extension ReverseList {
    /// Represents Scroll State of ``ReverseList``
    enum Scroll: Equatable {
        enum Destination: Equatable {
            case item(Item.ID)
            case bottom
        }

        case scrollingTo(Destination)
        case isNearBottom(Bool)

        var isNearBottom: Bool {
            switch self {
            case .scrollingTo: false
            case let .isNearBottom(bool): bool
            }
        }
    }
}
