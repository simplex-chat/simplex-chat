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

    var title: LocalizedStringKey? // if title is null, reason is shown

    var reason: String

    var password: String

    var selfDestruct: Bool

    var completed: (LAResult) -> Void


    static var sample = LocalAuthRequest(title: "Enter Passcode", reason: "Authenticate", password: "", selfDestruct: false, completed: { _ in })

}


func authenticate(title: LocalizedStringKey? = nil, reason: String, selfDestruct: Bool = false, completed: @escaping (LAResult) -> Void) {

    logger.debug("DEBUGGING: authenticate")

    switch privacyLocalAuthModeDefault.get() {

    case .system: systemAuthenticate(reason, completed)

    case .passcode:

        if let password = kcAppPassword.get() {

            DispatchQueue.main.async {

                ChatModel.shared.laRequest = LocalAuthRequest(

                    title: title,

                    reason: reason,

                    password: password,

                    selfDestruct: selfDestruct && UserDefaults.standard.bool(forKey: DEFAULT_LA_SELF_DESTRUCT),

                    completed: completed

                )

            }

        } else {

            completed(.unavailable(authError: NSLocalizedString("No app password", comment: "Authentication unavailable")))

        }

    }

}


func systemAuthenticate(_ reason: String, _ completed: @escaping (LAResult) -> Void) {

    logger.debug("DEBUGGING: systemAuthenticate")


    if !ensureBiometricAuthSecret() {

        logger.error("DEBUGGING: systemAuthenticate: failed to ensure biometric auth secret")

        completed(LAResult.unavailable(authError: NSLocalizedString("Failed to set up secure authentication", comment: "Authentication unavailable")))

        return

    }


    let laContext = LAContext()

    laContext.localizedReason = reason


    var authAvailabilityError: NSError?

    guard laContext.canEvaluatePolicy(.deviceOwnerAuthentication, error: &authAvailabilityError) else {

        logger.error("DEBUGGING: authentication availability error: \(authAvailabilityError.debugDescription)")

        completed(LAResult.unavailable(authError: authAvailabilityError?.localizedDescription))

        return

    }


    logger.debug("DEBUGGING: systemAuthenticate: attempting keychain-based authentication")

    DispatchQueue.global(qos: .userInitiated).async {

        let secret = getBiometricAuthSecret(context: laContext)

        DispatchQueue.main.async {

            if secret != nil {

                logger.debug("DEBUGGING: systemAuthenticate: keychain authentication succeeded")

                completed(LAResult.success)

            } else {

                logger.error("DEBUGGING: systemAuthenticate: keychain authentication failed")

                completed(LAResult.failed(authError: NSLocalizedString("Authentication failed", comment: "Authentication error")))

            }

        }

    }

}


func laTurnedOnAlert() -> Alert {

    mkAlert(

        title: "SimpleX Lock turned on",

        message: "You will be required to authenticate when you start or resume the app after 30 seconds in background."

    )

}


func laPasscodeNotSetAlert() -> Alert {

    mkAlert(

        title: "SimpleX Lock not enabled!",

        message: "You can turn on SimpleX Lock via Settings."

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