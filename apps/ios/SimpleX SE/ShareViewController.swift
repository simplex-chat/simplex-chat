//
//  ShareViewController.swift
//  SimpleX SE
//
//  Created by Levitating Pineapple on 08/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import UIKit
import SwiftUI
import SimpleXChat

/// Extension Entry point
/// System will create this controller each time share sheet is invoked
/// using `NSExtensionPrincipalClass` in the info.plist
@objc(ShareViewController)
class ShareViewController: UIHostingController<ShareView> {
    let model = ShareModel()

    @objc init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
        super.init(rootView: ShareView(model: model))
    }

    @available(*, unavailable)
    required init?(coder aDecoder: NSCoder) { fatalError() }

    override func viewDidLoad() {
        ShareModel.CompletionHandler.isEventLoopEnabled = false
        if let extensionContext,
           let item = extensionContext.inputItems.first as? NSExtensionItem {
            model.completion = {
                ShareModel.CompletionHandler.isEventLoopEnabled = false
                extensionContext.completeRequest(returningItems: [item])
            }
            Task { await MainActor.run { model.item = item } }
        }
    }
}

extension ShareViewController: UIAdaptivePresentationControllerDelegate {
    public func presentationControllerDidDismiss(_ presentationController: UIPresentationController) {
        ShareModel.CompletionHandler.isEventLoopEnabled = false
    }
}
