//
//  ShareSheet.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

func showShareSheet(items: [Any], completed: (() -> Void)? = nil) {
    let keyWindowScene = UIApplication.shared.connectedScenes.first { $0.activationState == .foregroundActive } as? UIWindowScene
    if let keyWindow = keyWindowScene?.windows.filter(\.isKeyWindow).first,
       let presentedViewController = keyWindow.rootViewController?.presentedViewController ?? keyWindow.rootViewController {
        let activityViewController = UIActivityViewController(activityItems: items, applicationActivities: nil)
        if let completed = completed {
            let handler: UIActivityViewController.CompletionWithItemsHandler = { _,_,_,_ in completed() }
            activityViewController.completionWithItemsHandler = handler
        }
        presentedViewController.present(activityViewController, animated: true)
    }
}
