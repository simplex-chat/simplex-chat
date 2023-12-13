//
//  NSESubscriber.swift
//  SimpleXChat
//
//  Created by Evgeny on 09/12/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

private var nseSubscribers: [UUID:NSESubscriber] = [:]

// timeout for active notification service extension going into "suspending" state.
// If in two seconds the state does not change, we assume that it was not running and proceed with app activation/answering call.
private let SUSPENDING_TIMEOUT: TimeInterval = 2

// timeout should be larger than SUSPENDING_TIMEOUT
func waitNSESuspended(timeout: TimeInterval, dispatchQueue: DispatchQueue = DispatchQueue.main, suspended: @escaping (Bool) -> Void) {
    if timeout <= SUSPENDING_TIMEOUT {
        logger.warning("waitNSESuspended: small timeout \(timeout), using \(SUSPENDING_TIMEOUT + 1)")
    }
    var state = nseStateGroupDefault.get()
    if case .suspended = state {
        dispatchQueue.async { suspended(true) }
        return
    }
    let id = UUID()
    var suspendedCalled = false
    checkTimeout()
    nseSubscribers[id] = nseMessageSubscriber { msg in
        if case let .state(newState) = msg {
            state = newState
            logger.debug("waitNSESuspended state: \(state.rawValue)")
            if case .suspended = newState {
                notifySuspended(true)
            }
        }
    }
    return

    func notifySuspended(_ ok: Bool) {
        logger.debug("waitNSESuspended notifySuspended: \(ok)")
        if !suspendedCalled {
            logger.debug("waitNSESuspended notifySuspended: calling suspended(\(ok))")
            suspendedCalled = true
            nseSubscribers.removeValue(forKey: id)
            dispatchQueue.async { suspended(ok) }
        }
    }

    func checkTimeout() {
        if !suspending() {
            checkSuspendingTimeout()
        } else if state == .suspending {
            checkSuspendedTimeout()
        }
    }

    func suspending() -> Bool {
        suspendedCalled || state == .suspended || state == .suspending
    }

    func checkSuspendingTimeout() {
        DispatchQueue.global().asyncAfter(deadline: .now() + SUSPENDING_TIMEOUT) {
            logger.debug("waitNSESuspended check suspending timeout")
            if !suspending() {
                notifySuspended(false)
            } else if state != .suspended {
                checkSuspendedTimeout()
            }
        }
    }

    func checkSuspendedTimeout() {
        DispatchQueue.global().asyncAfter(deadline: .now() + min(timeout - SUSPENDING_TIMEOUT, 1)) {
            logger.debug("waitNSESuspended check suspended timeout")
            if state != .suspended {
                notifySuspended(false)
            }
        }
    }
}
