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
            model.completion = {
                self.extensionContext!.completeRequest(returningItems: [item]) { _ in
                    let _ = sendSimpleXCmd(.apiStopChat)
                }
            }
            Task { await MainActor.run { model.item = item } }
        }
    }
}

// TODO: Database access management
/// Handle database access using existing (not-ideal) model, since
///
/// 1. Subscribe to lifecycle notifications:
///     - `NSExtensionHostDidBecomeActive`
///     - `NSExtensionHostWillResignActive`
///     - `NSExtensionHostDidEnterBackground`
///     - `NSExtensionHostWillEnterForeground`
///
/// 2. Add `SEState` using `SharedFileSubscriber`
/// and suspend the chat in case there is a NSE Notification, while share sheet process is active.
///
/// 3. Handle the case, where share-sheet and app is open simultaneously by sending
/// a notification over `CFNotificationCenterGetDarwinNotifyCenter` 
/// so the main app can register database changes and update it's `ChatModel`
///
/// 4. Consider protecting all database connection creations with additional as described in
/// https://swiftpackageindex.com/groue/grdb.swift/v6.28.0/documentation/grdb/databasesharing#Use-the-WAL-mode 
