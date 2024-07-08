//
//  ShareViewController.swift
//  SimpleX SE
//
//  Created by Levitating Pineapple on 08/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import UIKit
import OSLog
import SwiftUI
import SimpleXChat

let logger = Logger()

@objc(ShareViewController)
class ShareViewController: UIHostingController<ShareView> {

    @objc
    init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
        super.init(rootView: ShareView())
        initialise()
    }
    
    @available(*, unavailable)
    required init?(coder aDecoder: NSCoder) {
        fatalError()
    }

    private func initialise() {
        haskell_init_se()
        let (_, dbStatus) = chatMigrateInit(
            confirmMigrations: defaultMigrationConfirmation(),
            backgroundMode: false
        )
        logger.debug("\(String(describing: dbStatus))") // ok
        let activeUser = sendSimpleXCmd(.showActiveUser)
        logger.debug("\(activeUser)") // .noActiveUser
    }
}

struct ShareView: View {
    var body: some View {
        Text("Share Extension")
    }
}
