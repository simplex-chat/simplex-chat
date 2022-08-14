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
    func contextMenuWithPreview(title: String = "", actions: [UIAction], size: Binding<CGSize>) -> some View {
        self.overlay(
            InteractionView(config: InteractionConfig(
                preview: self,
                menu: UIMenu(title: title, children: actions),
                size: size
            ))
        )
    }
}

private struct InteractionConfig<Content: View> {
    let preview: Content
    let menu: UIMenu
    let size: Binding<CGSize>
}

private struct InteractionView<Content: View>: UIViewRepresentable {
    let config: InteractionConfig<Content>

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        view.backgroundColor = .clear
        let menuInteraction = UIContextMenuInteraction(delegate: context.coordinator)
        view.addInteraction(menuInteraction)
        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) { }

    func makeCoordinator() -> InteractionViewCoordinator<Content> {
        InteractionViewCoordinator(interactionView: self, config: config)
    }
}

private class InteractionViewCoordinator<Content: View>: NSObject, UIContextMenuInteractionDelegate {
    let interactionView: InteractionView<Content>
    let config: InteractionConfig<Content>

    init(interactionView: InteractionView<Content>, config: InteractionConfig<Content>) {
        self.interactionView = interactionView
        self.config = config
    }

    func contextMenuInteraction(
        _ interaction: UIContextMenuInteraction,
        configurationForMenuAtLocation location: CGPoint
    ) -> UIContextMenuConfiguration? {
        UIContextMenuConfiguration(
            identifier: nil,
            previewProvider: { [weak self] () -> UIViewController? in
                guard let self = self else { return nil }
                let s = self.config.size.wrappedValue
                return PreviewHostingController(rootView: self.config.preview.frame(width: s.width, height: s.height))
            },
            actionProvider: { [weak self] _ in
                guard let self = self else { return nil }
                return self.config.menu
            }
        )
    }

//    func contextMenuInteraction(
//        _ interaction: UIContextMenuInteraction,
//        willPerformPreviewActionForMenuWith configuration: UIContextMenuConfiguration,
//        animator: UIContextMenuInteractionCommitAnimating
//    ) {
//        animator.preferredCommitStyle = .pop
//        animator.addCompletion(self.didTapPreview)
//    }

//    func contextMenuInteraction(
//        _ interaction: UIContextMenuInteraction,
//        previewForHighlightingMenuWithConfiguration configuration: UIContextMenuConfiguration
//    ) -> UITargetedPreview? {
//        targetedPreview
//    }
//
//    func contextMenuInteraction(
//        _ interaction: UIContextMenuInteraction,
//        previewForDismissingMenuWithConfiguration configuration: UIContextMenuConfiguration
//    ) -> UITargetedPreview? {
//        targetedPreview
//    }
//
//    private var targetedPreview: UITargetedPreview? {
//        let parameters = UIPreviewParameters()
//        parameters.backgroundColor = .clear
//        let target = UIPreviewTarget(container: UIHostingController(rootView: preview).view, center: self.view.center)
//        return UITargetedPreview(view: UIImageView(image: preview.snapshot), parameters: parameters, target: target)
//    }
}

private final class PreviewHostingController<Content: View>: UIHostingController<Content> {
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        preferredContentSize = view.intrinsicContentSize
    }
}

extension View {
    var snapshot: UIImage {
        let controller = UIHostingController(rootView: self)
        let view = controller.view

        let targetSize = controller.view.intrinsicContentSize
        view?.bounds = CGRect(origin: .zero, size: targetSize)
        view?.backgroundColor = .clear

        let renderer = UIGraphicsImageRenderer(size: targetSize)

        return renderer.image { _ in
            view?.drawHierarchy(in: controller.view.bounds, afterScreenUpdates: true)
        }
    }
}

