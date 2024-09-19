//
//  PagedScrollView.swift
//  SimpleX (iOS)
//
//  Created by user on 19/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import Foundation

// TODO: Inject padding and cleanup
class UIScrollViewViewController: UIViewController {
    lazy var scrollView: UIScrollView = {
        let v = UIScrollView()
        v.isPagingEnabled = true
        v.contentInsetAdjustmentBehavior = .never
        v.contentInset = .zero
        v.showsHorizontalScrollIndicator = false
        v.showsHorizontalScrollIndicator = false
        v.clipsToBounds = false
        return v
    }()

    var hostingController: UIHostingController<AnyView> = UIHostingController(rootView: AnyView(EmptyView()))

    override func viewDidLoad() {
        super.viewDidLoad()
        view.addSubview(self.scrollView)
        pinEdges(of: self.scrollView, to: self.view, constant: 24)
        hostingController.willMove(toParent: self)
        scrollView.addSubview(self.hostingController.view)
        pinEdges(of: self.hostingController.view, to: self.scrollView)
        hostingController.didMove(toParent: self)
        scrollView.clipsToBounds = false
        parent?.view.clipsToBounds = false
    }

    func pinEdges(of viewA: UIView, to viewB: UIView, constant: CGFloat = 0) {
        viewA.translatesAutoresizingMaskIntoConstraints = false
        viewB.addConstraints([
            viewA.leadingAnchor.constraint(equalTo: viewB.leadingAnchor, constant: constant),
            viewA.trailingAnchor.constraint(equalTo: viewB.trailingAnchor, constant: -constant),
            viewA.topAnchor.constraint(equalTo: viewB.topAnchor),
            viewA.bottomAnchor.constraint(equalTo: viewB.bottomAnchor),
        ])
    }
}

struct PagedScrollView<Content: View>: UIViewControllerRepresentable {

    @ViewBuilder
    var content: () -> Content

    func makeUIViewController(context: Context) -> UIScrollViewViewController {
        let vc = UIScrollViewViewController()
        vc.hostingController.rootView = AnyView(self.content())
        return vc
    }

    func updateUIViewController(_ viewController: UIScrollViewViewController, context: Context) {
        viewController.hostingController.rootView = AnyView(self.content())
    }
}
