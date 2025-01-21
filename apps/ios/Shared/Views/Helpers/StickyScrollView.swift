//
//  StickyScrollView.swift
//  SimpleX (iOS)
//
//  Created by user on 20/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct StickyScrollView<Content: View>: UIViewRepresentable {
    @Binding var resetScroll: ResetScrollAction
    @ViewBuilder let content: () -> Content

    func makeUIView(context: Context) -> UIScrollView {
        let hc = context.coordinator.hostingController
        hc.view.backgroundColor = .clear
        let sv = UIScrollView()
        sv.showsHorizontalScrollIndicator = false
        sv.addSubview(hc.view)
        sv.delegate = context.coordinator
        DispatchQueue.main.async {
            resetScroll = ResetScrollAction { sv.setContentOffset(.zero, animated: false) }
        }
        return sv
    }

    func updateUIView(_ scrollView: UIScrollView, context: Context) {
        let hc = context.coordinator.hostingController
        hc.rootView = content()
        hc.view.frame.size = hc.view.intrinsicContentSize
        scrollView.contentSize = hc.view.intrinsicContentSize
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
            if targetContentOffset.pointee.x < 32 {
                targetContentOffset.pointee.x = 0
            }
        }
    }
}

struct ResetScrollAction {
    var action = { }
    func callAsFunction() { action() }
}
