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
    func uiKitContextMenu(title: String = "", actions: [UIAction]) -> some View {
        self.overlay(Color(uiColor: .systemBackground))
        .overlay(
            InteractionView(content: self, menu: UIMenu(title: title, children: actions))
        )
    }
}

private struct InteractionConfig<Content: View> {
    let content: Content
    let menu: UIMenu
}

private struct InteractionView<Content: View>: UIViewRepresentable {
    let content: Content
    let menu: UIMenu

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        view.backgroundColor = .clear
        DispatchQueue.main.async {
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
        }
        let menuInteraction = UIContextMenuInteraction(delegate: context.coordinator)
        view.addInteraction(menuInteraction)
        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) {}

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
