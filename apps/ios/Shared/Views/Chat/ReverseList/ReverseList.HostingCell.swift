//
//  ReverseList.HostingCell.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 12/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

extension ReverseList {
    /// `UIHostingConfiguration` back-port for iOS14 and iOS15
    /// Implemented as a `UITableViewCell` that wraps and manages a generic `UIHostingController`
    final class HostingCell<Hosted: View>: UITableViewCell {
        private let hostingController = UIHostingController<Hosted?>(rootView: nil)

        /// Updates content of the cell
        /// For reference: https://noahgilmore.com/blog/swiftui-self-sizing-cells/
        func set(content: Hosted, parent: UIViewController) {
            hostingController.rootView = content
            if let hostingView = hostingController.view {
                hostingView.invalidateIntrinsicContentSize()
                if hostingController.parent != parent { parent.addChild(hostingController) }
                if !contentView.subviews.contains(hostingController.view) {
                    contentView.addSubview(hostingController.view)
                    hostingView.translatesAutoresizingMaskIntoConstraints = false
                    NSLayoutConstraint.activate([
                        hostingView.leadingAnchor
                            .constraint(equalTo: contentView.leadingAnchor),
                        hostingView.trailingAnchor
                            .constraint(equalTo: contentView.trailingAnchor),
                        hostingView.topAnchor
                            .constraint(equalTo: contentView.topAnchor),
                        hostingView.bottomAnchor
                            .constraint(equalTo: contentView.bottomAnchor)
                    ])
                }
                if hostingController.parent != parent { hostingController.didMove(toParent: parent) }
            } else {
                fatalError("Hosting View not loaded \(hostingController)")
            }
        }
    }
}
