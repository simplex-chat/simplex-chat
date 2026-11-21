//
//  SEChatState.swift
//  SimpleX SE
//
//  Created by User on 18/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// SEStateGroupDefault must not be used in the share extension directly, only via this singleton
class SEChatState {
    static let shared = SEChatState()
    private var value_ = seStateGroupDefault.get()

    var value: SEState {
        value_
    }

    func set(_ state: SEState) {
        seStateGroupDefault.set(state)
        sendSEState(state)
        value_ = state
    }
}

/// Waits for other processes to set their state to suspended
/// Will wait for maximum of two seconds, since they might not be running
func waitForOtherProcessesToSuspend() async {
    let startTime = CFAbsoluteTimeGetCurrent()
    while CFAbsoluteTimeGetCurrent() - startTime < 2 {
        try? await Task.sleep(nanoseconds: 100 * NSEC_PER_MSEC)
        if appStateGroupDefault.get() == .suspended &&
           nseStateGroupDefault.get() == .suspended {
            break
        }
    }
}
