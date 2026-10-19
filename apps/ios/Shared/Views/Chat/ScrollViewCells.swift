//
//  ScrollViewCells.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 27.01.2025.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

protocol ReusableView {
    func prepareForReuse()
}

/// `UIHostingConfiguration` back-port for iOS14 and iOS15
/// Implemented as a `UIView` that wraps and manages a generic `UIHostingController`
final class HostingCell<Hosted: View>: UIView, ReusableView {
    private let hostingController = UIHostingController<Hosted?>(rootView: nil)

    /// Updates content of the cell
    /// For reference: https://noahgilmore.com/blog/swiftui-self-sizing-cells/
    func set(content: Hosted, parent: UIViewController) {
        hostingController.view.backgroundColor = .clear
        hostingController.rootView = content
        if let hostingView = hostingController.view {
            hostingView.invalidateIntrinsicContentSize()
            if hostingController.parent != parent { parent.addChild(hostingController) }
            if !subviews.contains(hostingController.view) {
                addSubview(hostingController.view)
                hostingView.translatesAutoresizingMaskIntoConstraints = false
                NSLayoutConstraint.activate([
                    hostingView.leadingAnchor
                        .constraint(equalTo: leadingAnchor),
                    hostingView.trailingAnchor
                        .constraint(equalTo: trailingAnchor),
                    hostingView.topAnchor
                        .constraint(equalTo: topAnchor),
                    hostingView.bottomAnchor
                        .constraint(equalTo: bottomAnchor)
                ])
            }
            if hostingController.parent != parent { hostingController.didMove(toParent: parent) }
        } else {
            fatalError("Hosting View not loaded \(hostingController)")
        }
    }

    func prepareForReuse() {
        //super.prepareForReuse()
        hostingController.rootView = nil
    }
}
