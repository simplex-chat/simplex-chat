//
//  LocalAuthenticationUtils.swift
//  SimpleX (iOS)
//
//  Created by Efim Poberezkin on 26.05.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import LocalAuthentication
import SimpleXChat

enum LAResult {
    case success
    case failed(authError: String?)
    case unavailable(authError: String?)
}

func authorize(_ text: String, _ authorized: Binding<Bool>) {
    authenticate(reason: text) { laResult in
        switch laResult {
        case .success: authorized.wrappedValue = true
        case .unavailable: authorized.wrappedValue = true
        case .failed: authorized.wrappedValue = false
        }
    }
}

struct LocalAuthRequest {
    var reason: String
    var password: String
    var completed: (LAResult) -> Void

    static var sample = LocalAuthRequest(reason: "Authenticate", password: "", completed: { _ in })
}

func authenticate(reason: String, completed: @escaping (LAResult) -> Void) {
    logger.debug("authenticate")
    switch privacyLocalAuthModeDefault.get() {
    case .system: systemAuthenticate(reason, completed)
    case .password:
        if let password = kcAppPassword.get() {
            DispatchQueue.main.async {
                ChatModel.shared.laRequest = LocalAuthRequest(reason: reason, password: password, completed: completed)
            }
        } else {
            completed(.unavailable(authError: NSLocalizedString("No app password", comment: "Authentication unavailable")))
        }
    }
}

func systemAuthenticate(_ reason: String, _ completed: @escaping (LAResult) -> Void) {
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
