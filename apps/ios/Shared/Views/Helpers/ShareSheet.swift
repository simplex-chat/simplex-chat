//
//  ShareSheet.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

func getTopViewController() -> UIViewController? {
    let keyWindowScene = UIApplication.shared.connectedScenes.first { $0.activationState == .foregroundActive } as? UIWindowScene
    if let keyWindow = keyWindowScene?.windows.filter(\.isKeyWindow).first,
       let rootViewController = keyWindow.rootViewController {
        // Find the top-most presented view controller
        var topController = rootViewController
        while let presentedViewController = topController.presentedViewController {
            topController = presentedViewController
        }
        return topController
    }
    return nil
}

func showShareSheet(items: [Any], completed: (() -> Void)? = nil) {
    if let topController = getTopViewController() {
        let activityViewController = UIActivityViewController(activityItems: items, applicationActivities: nil)
        if let completed = completed {
            activityViewController.completionWithItemsHandler = { _, _, _, _ in
                completed()
            }
        }        
        topController.present(activityViewController, animated: true)
    }
}

func showAlert(
    title: String,
    message: String? = nil,
    buttonTitle: String,
    buttonAction: @escaping () -> Void,
    cancelButton: Bool
) -> Void {
    if let topController = getTopViewController() {
        let alert = UIAlertController(title: title, message: message, preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: buttonTitle, style: .default) { _ in
            buttonAction()
        })
        if cancelButton {
            alert.addAction(cancelAlertAction)
        }
        topController.present(alert, animated: true)
    }
}

func showAlert(
    _ title: String,
    message: String? = nil,
    actions: () -> [UIAlertAction] = { [okAlertAction] }
) {
    if let topController = getTopViewController() {
        let alert = UIAlertController(title: title, message: message, preferredStyle: .alert)
        for action in actions() { alert.addAction(action) }
        topController.present(alert, animated: true)
    }
}

let okAlertAction = UIAlertAction(title: NSLocalizedString("Ok", comment: "alert button"), style: .default)

let cancelAlertAction = UIAlertAction(title: NSLocalizedString("Cancel", comment: "alert button"), style: .cancel)
