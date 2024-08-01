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
    private let model = ShareModel()
    // Assuming iOS continues to only allow single share sheet to be presented at once
    static var isVisible: Bool = false

    @objc init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
        super.init(rootView: ShareView(model: model))
    }

    @available(*, unavailable)
    required init?(coder aDecoder: NSCoder) { fatalError() }

    override func viewDidLoad() {
        ShareModel.CompletionHandler.isEventLoopEnabled = false
        model.setup(context: extensionContext!)
    }

    override func viewWillAppear(_ animated: Bool) {
        logger.debug("ShareSheet will appear")
        super.viewWillAppear(animated)
        Self.isVisible = true
    }

    override func viewWillDisappear(_ animated: Bool) {
        logger.debug("ShareSheet will dissappear")
        super.viewWillDisappear(animated)
        ShareModel.CompletionHandler.isEventLoopEnabled = false
        Self.isVisible = false
    }
}
