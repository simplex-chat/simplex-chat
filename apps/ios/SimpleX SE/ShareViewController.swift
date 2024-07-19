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
        if let item = extensionContext?.inputItems.first as? NSExtensionItem {
            model.completion = { error in
                if let error {
                    self.extensionContext!.cancelRequest(withError: error)
                } else {
                    self.extensionContext!.completeRequest(returningItems: [item]) { expired in
                        self.handleShutDown(expired: expired)
                    }
                }
            }
            Task { await MainActor.run { model.item = item } }
        }
    }

    override func viewDidDisappear(_ animated: Bool) {
        // In case user dismisses the sheet, while sending a file
        if let item = extensionContext?.inputItems.first as? NSExtensionItem {
            if model.bottomBar.isLoading {
                self.extensionContext!.completeRequest(returningItems: [item]) { expired in
                    self.handleShutDown(expired: expired)
                }
            }
        }
    }

    private func handleShutDown(expired: Bool) {
        if expired {
            // TODO: Send Notification that the main app needs to be opened to finish upload
        } else {
            // TODO: Close database connection and shut down the chat
            let _ = sendSimpleXCmd(.apiStopChat)
        }
    }
}
