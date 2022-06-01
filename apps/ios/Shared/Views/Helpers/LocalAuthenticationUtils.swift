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

func authenticate(reason: String, completed: @escaping (LAResult) -> Void) {
    let laContext = LAContext()
    var authAvailabilityError: NSError?
    if laContext.canEvaluatePolicy(.deviceOwnerAuthentication, error: &authAvailabilityError) {
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

func laTurnedOnAlert() -> Alert {
    mkAlert(
        title: "SimpleX Lock turned on",
        message: "You will be required to authenticate when you start or resume the app after 30 seconds in background."
    )
}

func laFailedAlert() -> Alert {
    mkAlert(
        title: "Authentication failed",
        message: "You could not be verified; please try again."
    )
}

func laUnavailableInstructionAlert() -> Alert {
    mkAlert(
        title: "Authentication unavailable",
        message: "Device authentication is not enabled. You can turn on SimpleX Lock via Settings, once you enable device authentication."
    )
}

func laUnavailableTurningOffAlert() -> Alert {
    mkAlert(
        title: "Authentication unavailable",
        message: "Device authentication is disabled. Turning off SimpleX Lock."
    )
}
