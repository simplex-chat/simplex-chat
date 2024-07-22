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

    public init(title: LocalizedStringKey, message: LocalizedStringKey?) {
        self.title = title
        self.message = message
    }

    public init(_ title: LocalizedStringKey) {
        self.title = title
        self.message = nil
    }

    public init(_ error: any Error) {
        self = if let chatResponse = error as? ChatResponse {
            ErrorAlert(chatResponse)
        } else {
            ErrorAlert(LocalizedStringKey(error.localizedDescription))
        }
    }

    public init(_ chatError: ChatError) {
        self = ErrorAlert("\(chatErrorString(chatError))")
    }

    public init(_ chatResponse: ChatResponse) {
        self = if let networkErrorAlert = getNetworkErrorAlert(chatResponse) {
            networkErrorAlert
        } else {
            ErrorAlert("\(responseError(chatResponse))")
        }
    }
}

extension LocalizedStringKey: @unchecked Sendable { }

extension View {
    /// Bridges ``ErrorAlert`` to the generic alert API.
    @ViewBuilder public func alert<A: View>(
        _ errorAlert: ErrorAlert?,
        @ViewBuilder actions: (ErrorAlert) -> A
    ) -> some View {
        if let alert = errorAlert {
            if let message = alert.message {
                self.alert(
                    alert.title,
                    isPresented: .constant(errorAlert != nil),
                    actions: { actions(alert) },
                    message: { Text(message) }
                )
            } else {
                self.alert(
                    alert.title,
                    isPresented: .constant(errorAlert != nil),
                    actions: { actions(alert) }
                )
            }
        } else { self }
    }
}

public func getNetworkErrorAlert(_ r: ChatResponse) -> ErrorAlert? {
    switch r {
    case let .chatCmdError(_, .errorAgent(.BROKER(addr, .TIMEOUT))):
        return ErrorAlert(title: "Connection timeout", message: "Please check your network connection with \(serverHostname(addr)) and try again.")
    case let .chatCmdError(_, .errorAgent(.BROKER(addr, .NETWORK))):
        return ErrorAlert(title: "Connection error", message: "Please check your network connection with \(serverHostname(addr)) and try again.")
    case let .chatCmdError(_, .errorAgent(.BROKER(addr, .HOST))):
        return ErrorAlert(title: "Connection error", message: "Server address is incompatible with network settings: \(serverHostname(addr)).")
    case let .chatCmdError(_, .errorAgent(.BROKER(addr, .TRANSPORT(.version)))):
        return ErrorAlert(title: "Connection error", message: "Server version is incompatible with your app: \(serverHostname(addr)).")
    case let .chatCmdError(_, .errorAgent(.SMP(serverAddress, .PROXY(proxyErr)))):
        return smpProxyErrorAlert(proxyErr, serverAddress)
    case let .chatCmdError(_, .errorAgent(.PROXY(proxyServer, relayServer, .protocolError(.PROXY(proxyErr))))):
        return proxyDestinationErrorAlert(proxyErr, proxyServer, relayServer)
    default:
        return nil
    }
}

private func smpProxyErrorAlert(_ proxyErr: ProxyError, _ srvAddr: String) -> ErrorAlert? {
    switch proxyErr {
    case .BROKER(brokerErr: .TIMEOUT):
        return ErrorAlert(title: "Private routing error", message: "Error connecting to forwarding server \(serverHostname(srvAddr)). Please try later.")
    case .BROKER(brokerErr: .NETWORK):
        return ErrorAlert(title: "Private routing error", message: "Error connecting to forwarding server \(serverHostname(srvAddr)). Please try later.")
    case .BROKER(brokerErr: .HOST):
        return ErrorAlert(title: "Private routing error", message: "Forwarding server address is incompatible with network settings: \(serverHostname(srvAddr)).")
    case .BROKER(brokerErr: .TRANSPORT(.version)):
        return ErrorAlert(title: "Private routing error", message: "Forwarding server version is incompatible with network settings: \(serverHostname(srvAddr)).")
    default:
        return nil
    }
}

private func proxyDestinationErrorAlert(_ proxyErr: ProxyError, _ proxyServer: String, _ relayServer: String) -> ErrorAlert? {
    switch proxyErr {
    case .BROKER(brokerErr: .TIMEOUT):
        return ErrorAlert(title: "Private routing error", message: "Forwarding server \(serverHostname(proxyServer)) failed to connect to destination server \(serverHostname(relayServer)). Please try later.")
    case .BROKER(brokerErr: .NETWORK):
        return ErrorAlert(title: "Private routing error", message: "Forwarding server \(serverHostname(proxyServer)) failed to connect to destination server \(serverHostname(relayServer)). Please try later.")
    case .NO_SESSION:
        return ErrorAlert(title: "Private routing error", message: "Forwarding server \(serverHostname(proxyServer)) failed to connect to destination server \(serverHostname(relayServer)). Please try later.")
    case .BROKER(brokerErr: .HOST):
        return ErrorAlert(title: "Private routing error", message: "Destination server address of \(serverHostname(relayServer)) is incompatible with forwarding server \(serverHostname(proxyServer)) settings.")
    case .BROKER(brokerErr: .TRANSPORT(.version)):
        return ErrorAlert(title: "Private routing error", message: "Destination server version of \(serverHostname(relayServer)) is incompatible with forwarding server \(serverHostname(proxyServer)).")
    default:
        return nil
    }
}

public func serverHostname(_ srv: String) -> String {
    parseServerAddress(srv)?.hostnames.first ?? srv
}
