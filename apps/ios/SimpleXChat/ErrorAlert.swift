//
//  ErrorAlert.swift
//  SimpleXChat
//
//  Created by Levitating Pineapple on 20/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

public struct ErrorAlert: Error {
    public let title: LocalizedStringKey
    public let message: LocalizedStringKey?

    public init(
        title: LocalizedStringKey,
        message: LocalizedStringKey? = nil
    ) {
        self.title = title
        self.message = message
    }

    public init<A: View>(
        title: LocalizedStringKey,
        message: LocalizedStringKey? = nil,
        @ViewBuilder actions: @escaping () -> A
    ) {
        self.title = title
        self.message = message
    }

    public init(_ title: LocalizedStringKey) {
        self = ErrorAlert(title: title)
    }

    public init(_ error: any Error) {
        self = if let e = error as? ChatError {
            ErrorAlert(e)
        } else {
            ErrorAlert("\(error.localizedDescription)")
        }
    }
}

extension LocalizedStringKey: @unchecked Sendable { }

extension View {
    /// Bridges ``ErrorAlert`` to the generic alert API.
    /// - Parameters:
    ///   - errorAlert: Binding to the Error, which is rendered in the alert
    ///   - actions: View Builder containing action buttons.
    ///   System defaults to `Ok` dismiss error action, when no actions are provided.
    ///   System implicitly adds `Cancel` action, if a destructive action is present
    ///
    /// - Returns: View, which displays ErrorAlert?, when set.
    @ViewBuilder public func alert<A: View>(
        _ errorAlert: Binding<ErrorAlert?>,
        @ViewBuilder actions: (ErrorAlert) -> A = { _ in EmptyView() }
    ) -> some View {
        alert(
            errorAlert.wrappedValue?.title ?? "",
            isPresented: Binding<Bool>(
                get: { errorAlert.wrappedValue != nil },
                set: { if !$0 { errorAlert.wrappedValue = nil } }
            ),
            actions: {
                if let alert = errorAlert.wrappedValue { actions(alert) }
            },
            message: {
                if let message = errorAlert.wrappedValue?.message {
                    Text(message)
                }
            }
        )
    }
}

public func getNetworkErrorAlert(_ e: ChatError) -> (title: String, message: String?)? {
    switch e {
    case let .errorAgent(.BROKER(addr, .TIMEOUT)):
        (
            title: NSLocalizedString("Connection timeout", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Please check your network connection with %@ and try again.", comment: ""), serverHostname(addr))
        )
    case let .errorAgent(.BROKER(addr, .NETWORK(.unknownCAError))):
        (
            title: NSLocalizedString("Connection error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Fingerprint in server address does not match certificate: %@.", comment: ""), serverHostname(addr))
        )
    case let .errorAgent(.BROKER(addr, .NETWORK)):
        (
            title: NSLocalizedString("Connection error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Please check your network connection with %@ and try again.", comment: ""), serverHostname(addr))
        )
    case let .errorAgent(.BROKER(addr, .HOST)):
        (
            title: NSLocalizedString("Connection error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Server address is incompatible with network settings: %@.", comment: ""), serverHostname(addr))
        )
    case let .errorAgent(.BROKER(addr, .TRANSPORT(.version))):
        (
            title: NSLocalizedString("Connection error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Server version is incompatible with your app: %@.", comment: ""), serverHostname(addr))
        )
    case let .errorAgent(.SMP(serverAddress, .PROXY(proxyErr))):
        smpProxyErrorAlert(proxyErr, serverAddress)
    case let .errorAgent(.PROXY(proxyServer, relayServer, .protocolError(.PROXY(proxyErr)))):
        proxyDestinationErrorAlert(proxyErr, proxyServer, relayServer)
    default: nil
    }
}

private func smpProxyErrorAlert(_ proxyErr: ProxyError, _ srvAddr: String) -> (title: String, message: String?)? {
    switch proxyErr {
    case .BROKER(brokerErr: .TIMEOUT):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Error connecting to forwarding server %@. Please try later.", comment: ""), serverHostname(srvAddr))
        )
    case .BROKER(brokerErr: .NETWORK(.unknownCAError)):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Fingerprint in forwarding server address does not match certificate: %@.", comment: ""), serverHostname(srvAddr))
        )
    case .BROKER(brokerErr: .NETWORK):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Error connecting to forwarding server %@. Please try later.", comment: ""), serverHostname(srvAddr))
        )
    case .BROKER(brokerErr: .HOST):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Forwarding server address is incompatible with network settings: %@.", comment: ""), serverHostname(srvAddr))
        )
    case .BROKER(brokerErr: .TRANSPORT(.version)):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Forwarding server version is incompatible with network settings: %@.", comment: ""), serverHostname(srvAddr))
        )
    default:
        nil
    }
}

private func proxyDestinationErrorAlert(_ proxyErr: ProxyError, _ proxyServer: String, _ relayServer: String) -> (title: String, message: String?)? {
    switch proxyErr {
    case .BROKER(brokerErr: .TIMEOUT):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Forwarding server %@ failed to connect to destination server %@. Please try later.", comment: ""), serverHostname(proxyServer), serverHostname(relayServer))
        )
    case .BROKER(brokerErr: .NETWORK(.unknownCAError)):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Fingerprint in destination server address does not match certificate: %@.", comment: ""), serverHostname(relayServer))
        )
    case .BROKER(brokerErr: .NETWORK):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Forwarding server %@ failed to connect to destination server %@. Please try later.", comment: ""), serverHostname(proxyServer), serverHostname(relayServer))
        )
    case .NO_SESSION:
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Forwarding server %@ failed to connect to destination server %@. Please try later.", comment: ""), serverHostname(proxyServer), serverHostname(relayServer))
        )
    case .BROKER(brokerErr: .HOST):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Destination server address of %@ is incompatible with forwarding server %@ settings.", comment: ""), serverHostname(relayServer), serverHostname(proxyServer))
        )
    case .BROKER(brokerErr: .TRANSPORT(.version)):
        (
            title: NSLocalizedString("Private routing error", comment: ""),
            message: String.localizedStringWithFormat(NSLocalizedString("Destination server version of %@ is incompatible with forwarding server %@.", comment: ""), serverHostname(relayServer), serverHostname(proxyServer))
        )
    default:
        nil
    }
}

public func serverHostname(_ srv: String) -> String {
    parseServerAddress(srv)?.hostnames.first ?? srv
}

public func mtrErrorDescription(_ err: MTRError) -> LocalizedStringKey {
    switch err {
    case let .noDown(dbMigrations):
        "database version is newer than the app, but no down migration for: \(dbMigrations.joined(separator: ", "))"
    case let .different(appMigration, dbMigration):
        "different migration in the app/database: \(appMigration) / \(dbMigration)"
    }
}
