//
//  StickyScrollView.swift
//  SimpleX (iOS)
//
//  Created by user on 20/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct StickyScrollView<Content: View>: UIViewRepresentable {
    @ViewBuilder let content: () -> Content

    func makeUIView(context: Context) -> UIScrollView {
        let hc = context.coordinator.hostingController
        let sv = UIScrollView()
        sv.showsHorizontalScrollIndicator = false
        sv.addSubview(hc.view)
        sv.delegate = context.coordinator
        hc.view.translatesAutoresizingMaskIntoConstraints = false
        NSLayoutConstraint.activate([
            hc.view.leadingAnchor.constraint(equalTo: sv.leadingAnchor),
            hc.view.trailingAnchor.constraint(equalTo: sv.trailingAnchor),
            hc.view.topAnchor.constraint(equalTo: sv.topAnchor),
            hc.view.bottomAnchor.constraint(equalTo: sv.bottomAnchor)
        ])
        return sv
    }

    func updateUIView(_ scrollView: UIScrollView, context: Context) {
        context.coordinator.hostingController.rootView = content()
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(content: content())
    }

    class Coordinator: NSObject, UIScrollViewDelegate {
        let hostingController: UIHostingController<Content>

        init(content: Content) {
            self.hostingController = UIHostingController(rootView: content)
        }

        func scrollViewWillEndDragging(
            _ scrollView: UIScrollView,
            withVelocity velocity: CGPoint,
            targetContentOffset: UnsafeMutablePointer<CGPoint>
        ) {
            if targetContentOffset.pointee.x < 100 {
                targetContentOffset.pointee.x = 0
            }
        }
    }
}
