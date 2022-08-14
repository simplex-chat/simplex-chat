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
    let view = MyView()

    func makeUIView(context: Context) -> UIView {
        view.backgroundColor = .clear
        let menuInteraction = UIContextMenuInteraction(delegate: context.coordinator)
        view.addInteraction(menuInteraction)
        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) { }

    func makeCoordinator() -> InteractionViewCoordinator<Content> {
        InteractionViewCoordinator(interactionView: view, config: config)
    }
}

private class MyView: UIView {
    override func gestureRecognizerShouldBegin(_ gestureRecognizer: UIGestureRecognizer) -> Bool {
        !(gestureRecognizer is UITapGestureRecognizer)
    }
}

private class InteractionViewCoordinator<Content: View>: NSObject, UIContextMenuInteractionDelegate {
    let interactionView: MyView
    let config: InteractionConfig<Content>

    private var preview: some View {
        let s = self.config.size.wrappedValue
        return self.config.preview.frame(width: s.width, height: s.height)
    }

    init(interactionView: MyView, config: InteractionConfig<Content>) {
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
                return PreviewHostingController(rootView: self.preview)
            },
            actionProvider: { [weak self] _ in
                guard let self = self else { return nil }
                return self.config.menu
            }
        )
    }

    func contextMenuInteraction(
        _ interaction: UIContextMenuInteraction,
        willPerformPreviewActionForMenuWith configuration: UIContextMenuConfiguration,
        animator: UIContextMenuInteractionCommitAnimating
    ) {
        animator.addCompletion {
            print("user tapped")
        }
    }

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
        let target = UIPreviewTarget(container: interactionView, center: interactionView.center)
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

private final class PreviewHostingController<Content: View>: UIHostingController<Content> {
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        preferredContentSize = view.intrinsicContentSize
    }

    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        preferredContentSize = view.intrinsicContentSize
    }
}
