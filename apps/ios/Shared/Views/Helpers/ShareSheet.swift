//
//  ShareSheet.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

func showShareSheet(items: [Any], completed: (() -> Void)? = nil) {
    if let rootViewController = UIApplication.shared.windows.first(where: { $0.isKeyWindow })?.rootViewController {
        let topViewController = getTopViewController(from: rootViewController)
        
        let activityViewController = UIActivityViewController(activityItems: items, applicationActivities: nil)
        
        if let completed = completed {
            let handler: UIActivityViewController.CompletionWithItemsHandler = { _, _, _, _ in completed() }
            activityViewController.completionWithItemsHandler = handler
        }
        
        topViewController.present(activityViewController, animated: true)
    }
}

private func getTopViewController(from rootViewController: UIViewController) -> UIViewController {
    var topViewController = rootViewController
    
    // Navigate through presented view controllers
    while let presentedViewController = topViewController.presentedViewController {
        topViewController = presentedViewController
    }
    
    // If it's a UINavigationController, get the visible view controller
    if let navigationController = topViewController as? UINavigationController {
        topViewController = navigationController.visibleViewController ?? topViewController
    }
    
    // If it's a UITabBarController, get the selected view controller
    if let tabBarController = topViewController as? UITabBarController {
        topViewController = tabBarController.selectedViewController ?? topViewController
    }
    
    return topViewController
}
