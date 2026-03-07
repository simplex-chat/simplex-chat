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

    public init(_ chatError: ChatError) {
        self = if let networkErrorAlert = getNetworkErrorAlert(chatError) {
            networkErrorAlert
        } else {
            ErrorAlert("\(chatErrorString(chatError))")
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

public func getNetworkErrorAlert(_ e: ChatError) -> ErrorAlert? {
    switch e {
    case let .errorAgent(.BROKER(addr, .TIMEOUT)):
        ErrorAlert(title: "Connection timeout", message: "Please check your network connection with \(serverHostname(addr)) and try again.")
    case let .errorAgent(.BROKER(addr, .NETWORK(.unknownCAError))):
        ErrorAlert(title: "Connection error", message: "Fingerprint in router address does not match certificate: \(serverHostname(addr)).")
    case let .errorAgent(.BROKER(addr, .NETWORK)):
        ErrorAlert(title: "Connection error", message: "Please check your network connection with \(serverHostname(addr)) and try again.")
    case let .errorAgent(.BROKER(addr, .HOST)):
        ErrorAlert(title: "Connection error", message: "Router address is incompatible with network settings: \(serverHostname(addr)).")
    case let .errorAgent(.BROKER(addr, .TRANSPORT(.version))):
        ErrorAlert(title: "Connection error", message: "Router version is incompatible with your app: \(serverHostname(addr)).")
    case let .errorAgent(.SMP(serverAddress, .PROXY(proxyErr))):
        smpProxyErrorAlert(proxyErr, serverAddress)
    case let .errorAgent(.PROXY(proxyServer, relayServer, .protocolError(.PROXY(proxyErr)))):
        proxyDestinationErrorAlert(proxyErr, proxyServer, relayServer)
    default: nil
    }
}

private func smpProxyErrorAlert(_ proxyErr: ProxyError, _ srvAddr: String) -> ErrorAlert? {
    switch proxyErr {
    case .BROKER(brokerErr: .TIMEOUT):
        return ErrorAlert(title: "Private routing error", message: "Error connecting to forwarding router \(serverHostname(srvAddr)). Please try later.")
    case .BROKER(brokerErr: .NETWORK(.unknownCAError)):
        return ErrorAlert(title: "Private routing error", message: "Fingerprint in forwarding router address does not match certificate: \(serverHostname(srvAddr)).")
    case .BROKER(brokerErr: .NETWORK):
        return ErrorAlert(title: "Private routing error", message: "Error connecting to forwarding router \(serverHostname(srvAddr)). Please try later.")
    case .BROKER(brokerErr: .HOST):
        return ErrorAlert(title: "Private routing error", message: "Forwarding router address is incompatible with network settings: \(serverHostname(srvAddr)).")
    case .BROKER(brokerErr: .TRANSPORT(.version)):
        return ErrorAlert(title: "Private routing error", message: "Forwarding router version is incompatible with network settings: \(serverHostname(srvAddr)).")
    default:
        return nil
    }
}

private func proxyDestinationErrorAlert(_ proxyErr: ProxyError, _ proxyServer: String, _ relayServer: String) -> ErrorAlert? {
    switch proxyErr {
    case .BROKER(brokerErr: .TIMEOUT):
        return ErrorAlert(title: "Private routing error", message: "Forwarding router \(serverHostname(proxyServer)) failed to connect to destination router \(serverHostname(relayServer)). Please try later.")
    case .BROKER(brokerErr: .NETWORK(.unknownCAError)):
        return ErrorAlert(title: "Private routing error", message: "Fingerprint in destination router address does not match certificate: \(serverHostname(relayServer)).")
    case .BROKER(brokerErr: .NETWORK):
        return ErrorAlert(title: "Private routing error", message: "Forwarding router \(serverHostname(proxyServer)) failed to connect to destination router \(serverHostname(relayServer)). Please try later.")
    case .NO_SESSION:
        return ErrorAlert(title: "Private routing error", message: "Forwarding router \(serverHostname(proxyServer)) failed to connect to destination router \(serverHostname(relayServer)). Please try later.")
    case .BROKER(brokerErr: .HOST):
        return ErrorAlert(title: "Private routing error", message: "Destination router address of \(serverHostname(relayServer)) is incompatible with forwarding router \(serverHostname(proxyServer)) settings.")
    case .BROKER(brokerErr: .TRANSPORT(.version)):
        return ErrorAlert(title: "Private routing error", message: "Destination router version of \(serverHostname(relayServer)) is incompatible with forwarding router \(serverHostname(proxyServer)).")
    default:
        return nil
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
