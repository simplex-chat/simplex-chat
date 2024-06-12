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
    func uiKitContextMenu(hasImageOrVideo: Bool, maxWidth: CGFloat, itemWidth: Binding<CGFloat>, menu: Binding<UIMenu>, allowMenu: Binding<Bool>, backgroundColor: Color) -> some View {
        Group {
            if allowMenu.wrappedValue {
                if hasImageOrVideo {
                    InteractionView(content:
                        self.environmentObject(ChatModel.shared)
                        .overlay(DetermineWidthImageVideoItem())
                        .onPreferenceChange(DetermineWidthImageVideoItem.Key.self) { itemWidth.wrappedValue = $0 == 0 ? maxWidth : $0 }
                                    , maxWidth: maxWidth, itemWidth: itemWidth, menu: menu, backgroundColor: backgroundColor)
                    .frame(maxWidth: itemWidth.wrappedValue)
                } else {
                    InteractionView(content: self.environmentObject(ChatModel.shared), maxWidth: maxWidth, itemWidth: itemWidth, menu: menu, backgroundColor: backgroundColor)
                        .fixedSize(horizontal: true, vertical: false)
                }
            } else {
                self
            }
        }
    }
}

private class HostingViewHolder: UIView {
    var contentSize: CGSize = CGSizeMake(0, 0)
    override var intrinsicContentSize: CGSize { get { contentSize } }
}

struct InteractionView<Content: View>: UIViewRepresentable {
    let content: Content
    var maxWidth: CGFloat
    var itemWidth: Binding<CGFloat>
    @Binding var menu: UIMenu
    var backgroundColor: Color

    func makeUIView(context: Context) -> UIView {
        let view = HostingViewHolder()
        view.backgroundColor = .clear
        let hostView = UIHostingController(rootView: content)
//        hostView.view.backgroundColor = UIColor(backgroundColor)
        hostView.view.backgroundColor = .clear
        view.contentSize = hostView.view.intrinsicContentSize
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
        hostView.view.layer.cornerRadius = 18
        let menuInteraction = UIContextMenuInteraction(delegate: context.coordinator)
        view.addInteraction(menuInteraction)
        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) {
        let was = (uiView as! HostingViewHolder).contentSize
        (uiView as! HostingViewHolder).contentSize = uiView.subviews[0].sizeThatFits(CGSizeMake(itemWidth.wrappedValue, .infinity))
        if was != (uiView as! HostingViewHolder).contentSize {
            uiView.invalidateIntrinsicContentSize()
        }
        //uiView.backgroundColor = UIColor(backgroundColor)
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
