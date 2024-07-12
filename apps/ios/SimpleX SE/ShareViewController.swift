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

@objc(ShareViewController)
class ShareViewController: UIHostingController<ShareView> {
    let model = ShareModel()

    @objc init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
        super.init(rootView: ShareView(model: model))
    }

    @available(*, unavailable)
    required init?(coder aDecoder: NSCoder) { fatalError() }

    override func viewDidLoad() {
        if let item = extensionContext?.inputItems.first as? NSExtensionItem {
            model.completion = { self.extensionContext!.completeRequest(returningItems: [item]) }
            Task { await MainActor.run { model.item = item } }
        }
    }
}
