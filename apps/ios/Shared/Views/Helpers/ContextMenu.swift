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
    func uiKitContextMenu(title: String = "", actions: [UIAction], size: Binding<CGSize>) -> some View {
        self.overlay(
            InteractionView(config: InteractionConfig(
                view: self,
                menu: UIMenu(title: title, children: actions),
                size: size
            ))
        )
    }
}

private struct InteractionConfig<Content: View> {
    let view: Content
    let menu: UIMenu
    let size: Binding<CGSize>
}

private struct InteractionView<Content: View>: UIViewRepresentable {
    let config: InteractionConfig<Content>
    let view = UIView()

    func makeUIView(context: Context) -> UIView {
        view.backgroundColor = .clear
        let menuInteraction = UIContextMenuInteraction(delegate: context.coordinator)
        view.addInteraction(menuInteraction)
        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) { }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    class Coordinator: NSObject, UIContextMenuInteractionDelegate {
        let parent: InteractionView<Content>

        private var preview: some View {
            let s = parent.config.size.wrappedValue
            return parent.config.view.frame(width: s.width, height: s.height)
        }

        init(_ parent: InteractionView<Content>) {
            self.parent = parent
        }

        func contextMenuInteraction(
            _ interaction: UIContextMenuInteraction,
            configurationForMenuAtLocation location: CGPoint
        ) -> UIContextMenuConfiguration? {
            UIContextMenuConfiguration(
                identifier: nil,
                previewProvider: { [weak self] () -> UIViewController? in
                    guard let self = self else { return nil }
                    return SizedHostingController(rootView: self.preview)
                },
                actionProvider: { [weak self] _ in
                    guard let self = self else { return nil }
                    return self.parent.config.menu
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

        func contextMenuInteraction(
            _ interaction: UIContextMenuInteraction,
            previewForHighlightingMenuWithConfiguration configuration: UIContextMenuConfiguration
        ) -> UITargetedPreview? {
            targetedPreview
        }

        func contextMenuInteraction(
            _ interaction: UIContextMenuInteraction,
            previewForDismissingMenuWithConfiguration configuration: UIContextMenuConfiguration
        ) -> UITargetedPreview? {
            targetedPreview
        }

        private var targetedPreview: UITargetedPreview? {
            let parameters = UIPreviewParameters()
            parameters.backgroundColor = .clear
            let target = UIPreviewTarget(container: parent.view, center: parent.view.center)
            return UITargetedPreview(view: UIImageView(image: snapshot), parameters: parameters, target: target)
        }

        private var snapshot: UIImage {
            let controller = UIHostingController(rootView: preview)
            let view = controller.view
            let size = controller.view.intrinsicContentSize
            view?.bounds = CGRect(origin: .zero, size: size)
            view?.backgroundColor = .clear
            return UIGraphicsImageRenderer(size: size).image { _ in
                view?.drawHierarchy(in: controller.view.bounds, afterScreenUpdates: false)
            }
        }
    }
}

private final class SizedHostingController<Content: View>: UIHostingController<Content> {
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        preferredContentSize = view.intrinsicContentSize
    }

    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        preferredContentSize = view.intrinsicContentSize
    }
}
