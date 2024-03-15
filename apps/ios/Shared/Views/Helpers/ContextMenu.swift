//
//  ContextMenu2.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 09/08/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit
import SwiftUI

extension View {
    func uiKitContextMenu(maxWidth: CGFloat, menu: Binding<UIMenu>, allowMenu: Binding<Bool>) -> some View {
        InteractionView(content: self, maxWidth: maxWidth, menu: menu, allowMenu: allowMenu)
        .fixedSize(horizontal: true, vertical: false)
    }
}

private class HostingViewHolder: UIView {
    var contentSize: CGSize = CGSizeMake(0, 0)
    override var intrinsicContentSize: CGSize { get { contentSize } }
}

struct InteractionView<Content: View>: UIViewRepresentable {
    let content: Content
    var maxWidth: CGFloat
    @Binding var menu: UIMenu
    @Binding var allowMenu: Bool

    func makeUIView(context: Context) -> UIView {
        let view = HostingViewHolder()
        view.contentSize = CGSizeMake(maxWidth, .infinity)
        view.backgroundColor = .clear
        let hostView = UIHostingController(rootView: content)
        hostView.view.translatesAutoresizingMaskIntoConstraints = false
        let constraints = [
            hostView.view.topAnchor.constraint(equalTo: view.topAnchor),
            hostView.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            hostView.view.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            hostView.view.bottomAnchor.constraint(equalTo: view.bottomAnchor),
            hostView.view.widthAnchor.constraint(equalTo: view.widthAnchor),
            hostView.view.heightAnchor.constraint(equalTo: view.heightAnchor)
        ]
        view.addSubview(hostView.view)
        view.addConstraints(constraints)
        view.layer.cornerRadius = 18
        let menuInteraction = UIContextMenuInteraction(delegate: context.coordinator)
        view.addInteraction(menuInteraction)
        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) {
        (uiView as! HostingViewHolder).contentSize = uiView.subviews[0].sizeThatFits(CGSizeMake(maxWidth, .infinity))
        if allowMenu && uiView.interactions.isEmpty {
            let menuInteraction = UIContextMenuInteraction(delegate: context.coordinator)
            uiView.addInteraction(menuInteraction)
        } else if !allowMenu, let interaction = uiView.interactions.last {
            uiView.removeInteraction(interaction)
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    class Coordinator: NSObject, UIContextMenuInteractionDelegate {
        let parent: InteractionView<Content>

        init(_ parent: InteractionView<Content>) {
            self.parent = parent
        }

        func contextMenuInteraction(
            _ interaction: UIContextMenuInteraction,
            configurationForMenuAtLocation location: CGPoint
        ) -> UIContextMenuConfiguration? {
            UIContextMenuConfiguration(
                identifier: nil,
                previewProvider: nil,
                actionProvider: { [weak self] _ in
                    guard let self = self else { return nil }
                    return self.parent.menu
                }
            )
        }

    //    func contextMenuInteraction(
    //        _ interaction: UIContextMenuInteraction,
    //        willPerformPreviewActionForMenuWith configuration: UIContextMenuConfiguration,
    //        animator: UIContextMenuInteractionCommitAnimating
    //    ) {
    //        animator.addCompletion {
    //            print("user tapped")
    //        }
    //    }
    }
}
