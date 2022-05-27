//
//  LocalAuthenticationUtils.swift
//  SimpleX (iOS)
//
//  Created by Efim Poberezkin on 26.05.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import LocalAuthentication

enum LAResult {
    case success
    case failed(authError: String?)
    case unavailable(authError: String?)
}

func authenticate(completed: @escaping (LAResult) -> Void) {
    let laContext = LAContext()
    var authAvailabilityError: NSError?
    if laContext.canEvaluatePolicy(.deviceOwnerAuthentication, error: &authAvailabilityError) {
        let reason = "Access chats"
        laContext.evaluatePolicy(.deviceOwnerAuthentication, localizedReason: reason) { success, authError in
            DispatchQueue.main.async {
                if success {
                    completed(LAResult.success)
                } else {
                    logger.error("authentication error: \(authError.debugDescription)")
                    completed(LAResult.failed(authError: authError?.localizedDescription))
                }
            }
        }
    } else {
        logger.error("authentication availability error: \(authAvailabilityError.debugDescription)")
        completed(LAResult.unavailable(authError: authAvailabilityError?.localizedDescription))
    }
}

func laFailedAlert() {
    AlertManager.shared.showAlertMsg(
        title: "Authentication failed",
        message: "You could not be verified; please try again."
    )
}

func laUnavailableAlert() {
    AlertManager.shared.showAlertMsg(
        title: "Authentication unavailable",
        message: "Your device is not configured for authentication."
    )
}
