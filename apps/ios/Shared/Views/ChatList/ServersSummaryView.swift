//
//  ServersSummaryView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 25.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ServersSummaryView: View {
    @State private var serversSummary: PresentedServersSummary? = nil
    @State private var selectedUserCategory: PresentedUserCategory = .allUsers
    @State private var selectedServerType: PresentedServerType = .smp
    @State private var selectedSMPServer: String? = nil

    enum PresentedUserCategory {
        case currentUser
        case allUsers
    }

    enum PresentedServerType {
        case smp
        case xftp
    }

    var body: some View {
        NavigationView {
            viewBody()
                .navigationTitle("Servers info")
                .navigationBarTitleDisplayMode(.inline)
                .toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        reloadButton()
                    }
                    ToolbarItem(placement: .navigationBarTrailing) {
                        shareButton()
                    }
                }
        }
        .onAppear {
            getServersSummary()
        }
    }

    private func shareButton() -> some View {
        Button {
            if let serversSummary = serversSummary {
                showShareSheet(items: [encodeJSON(serversSummary)]) // TODO prettyJSON
            }
        } label: {
            Image(systemName: "square.and.arrow.up")
        }
        .disabled(serversSummary == nil)
    }

    private func reloadButton() -> some View {
        Button {
            getServersSummary()
        } label: {
            Image(systemName: "arrow.counterclockwise")
        }
    }

    @ViewBuilder private func viewBody() -> some View {
        if let summ = serversSummary {
            List {
                Group {
                    Picker("User selection", selection: $selectedUserCategory) {
                        Text("All users").tag(PresentedUserCategory.allUsers)
                        Text("Current user").tag(PresentedUserCategory.currentUser)
                    }
                    .pickerStyle(.segmented)

                    Picker("Server type", selection: $selectedServerType) {
                        Text("SMP").tag(PresentedServerType.smp)
                        Text("XFTP").tag(PresentedServerType.xftp)
                    }
                    .pickerStyle(.segmented)
                }
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

                switch (selectedUserCategory, selectedServerType) {
                case (.allUsers, .smp):
                    if summ.allUsedSMP.count > 0 || summ.allPrevSMP.count > 0 || summ.allProxSMP.count > 0 {
                        if summ.allUsedSMP.count > 0 {
                            smpServersListView(summ.allUsedSMP, showReconnectButton: true, summ.statsStartedAt, "Currently used")
                        }
                        if summ.allPrevSMP.count > 0 {
                            smpServersListView(summ.allPrevSMP, showReconnectButton: false, summ.statsStartedAt, "Previously used")
                        }
                        if summ.allProxSMP.count > 0 {
                            smpServersListView(summ.allProxSMP, showReconnectButton: false, summ.statsStartedAt, "Proxied", "You are not connected to these servers directly.")
                        }
                    } else {
                        noCategoryInfoText()
                    }
                case (.currentUser, .smp):
                    if summ.userUsedSMP.count > 0 || summ.userPrevSMP.count > 0 || summ.userProxSMP.count > 0 {
                        if summ.userUsedSMP.count > 0 {
                            smpServersListView(summ.userUsedSMP, showReconnectButton: true, summ.statsStartedAt, "Currently used")
                        }
                        if summ.userPrevSMP.count > 0 {
                            smpServersListView(summ.userPrevSMP, showReconnectButton: false, summ.statsStartedAt, "Previously used")
                        }
                        if summ.userProxSMP.count > 0 {
                            smpServersListView(summ.userProxSMP, showReconnectButton: false, summ.statsStartedAt, "Proxied", "You are not connected to these servers directly.")
                        }
                    } else {
                        noCategoryInfoText()
                    }
                case (.allUsers, .xftp):
                    if summ.allUsedXFTP.count > 0 || summ.allPrevXFTP.count > 0 {
                        if summ.allUsedXFTP.count > 0 {
                            xftpServersListView(summ.allUsedXFTP, "Currently used")
                        }
                        if summ.allPrevXFTP.count > 0 {
                            xftpServersListView(summ.allPrevXFTP, "Previously used")
                        }
                    } else {
                        noCategoryInfoText()
                    }
                case (.currentUser, .xftp):
                    if summ.userUsedXFTP.count > 0 || summ.userPrevXFTP.count > 0 {
                        if summ.userUsedXFTP.count > 0 {
                            xftpServersListView(summ.userUsedXFTP, "Currently used")
                        }
                        if summ.userPrevXFTP.count > 0 {
                            xftpServersListView(summ.userPrevXFTP, "Previously used")
                        }
                    } else {
                        noCategoryInfoText()
                    }
                }
            }
        } else {
            Text("No info, try to reload")
        }
    }

    @ViewBuilder private func smpServersListView(
        _ servers: [SMPServerSummary],
        showReconnectButton: Bool,
        _ statsStartedAt: Date,
        _ header: LocalizedStringKey? = nil,
        _ footer: LocalizedStringKey? = nil
    ) -> some View {
        let sortedServers = servers.sorted { serverAddress($0.smpServer).compare(serverAddress($1.smpServer)) == .orderedAscending }
        Section {
            ForEach(sortedServers) { server in
                smpServerView(server, showReconnectButton, statsStartedAt)
            }
        } header: {
            if let header = header {
                Text(header)
            }
        } footer: {
            if let footer = footer {
                Text(footer)
            }
        }
    }

    private func smpServerView(_ server: SMPServerSummary, _ showReconnectButton: Bool, _ statsStartedAt: Date) -> some View {
        NavigationLink(tag: server.id, selection: $selectedSMPServer) {
            SMPServerSummaryView(
                summary: server,
                showReconnectButton: showReconnectButton,
                statsStartedAt: statsStartedAt
            )
            .navigationBarTitle("SMP server")
            .navigationBarTitleDisplayMode(.large)
        } label: {
            Text(serverAddress(server.smpServer))
                .lineLimit(1)
        }
    }

    private func serverAddress(_ server: String) -> String {
        parseServerAddress(server)?.hostnames.first ?? server
    }

    @ViewBuilder private func xftpServersListView(
        _ servers: [XFTPServerSummary],
        _ header: LocalizedStringKey? = nil,
        _ footer: LocalizedStringKey? = nil
    ) -> some View {
        let sortedServers = servers.sorted { serverAddress($0.xftpServer).compare(serverAddress($1.xftpServer)) == .orderedAscending }
        Section {
            ForEach(sortedServers) { server in
                xftpServer(server)
            }
        } header: {
            if let header = header {
                Text(header)
            }
        } footer: {
            if let footer = footer {
                Text(footer)
            }
        }
    }

    private func xftpServer(_ server: XFTPServerSummary) -> some View {
        Text(serverAddress(server.xftpServer))
            .lineLimit(1)
    }

    private func noCategoryInfoText() -> some View {
        ZStack {
            Rectangle()
                .aspectRatio(contentMode: .fill)
                .foregroundColor(Color.clear)
            Text("No info")
                .foregroundColor(.secondary)
        }
        .listRowBackground(Color.clear)
        .listRowSeparator(.hidden)
    }

    private func getServersSummary() {
        do {
            serversSummary = try getAgentServersSummary()
        } catch let error {
            logger.error("getAgentServersSummary error: \(responseError(error))")
        }
    }
}

struct SMPServerSummaryView: View {
    var summary: SMPServerSummary
    var showReconnectButton: Bool
    var statsStartedAt: Date

    var body: some View {
        List {
            Section {
                Text(summary.smpServer)
                    .textSelection(.enabled)
                if let known = summary.known, !known {
                    Button {
                        // TODO
                    } label: {
                        Text("TODO Add as known")
                    }
                }
            } header: {
                Text("Server address")
            } footer: {
                if let known = summary.known, known {
                    // TODO open settings?
                    Text("Server is configured in **Settings** -> **Network & servers** -> **SMP servers**.")
                }
            }

            if showReconnectButton {
                Section {
                    Button {
                        // TODO
                    } label: {
                        Text("TODO Reconnect")
                    }
                }
            }

            if let sess = summary.sessions {
                Section("Sessions") {
                    infoRow("Connected", "\(sess.ssConnected)")
                    infoRow("Errors", "\(sess.ssErrors)")
                    infoRow("Connecting", "\(sess.ssConnecting)")
                }
            }

            if let subs = summary.subs {
                Section("Subscriptions") {
                    infoRow("Active", "\(subs.ssActive)")
                    infoRow("Pending", "\(subs.ssPending)")
                }
            }

            if let stats = summary.stats {
                Section("Statistics") {
                    infoRow("Messages sent directly", "\(stats._sentDirect)")
                    infoRow("    attempts", "\(stats._sentDirectAttempts)")
                    infoRow("Messages sent via proxy", "\(stats._sentViaProxy)")
                    infoRow("    attempts", "\(stats._sentViaProxyAttempts)")
                    infoRow("Messages sent to proxy", "\(stats._sentProxied)")
                    infoRow("    attempts", "\(stats._sentProxiedAttempts)")
                    infoRow("Send AUTH errors", "\(stats._sentAuthErrs)")
                    infoRow("    QUOTA errors", "\(stats._sentQuotaErrs)")
                    infoRow("    expired", "\(stats._sentExpiredErrs)")
                    infoRow("    other errors", "\(stats._sentOtherErrs)")
                    infoRow("Messages received", "\(stats._recvMsgs)")
                    infoRow("    duplicates", "\(stats._recvDuplicates)")
                    infoRow("    decryption", "\(stats._recvCryptoErrs)")
                    infoRow("    other errors", "\(stats._recvErrs)")
                    infoRow("Connections created", "\(stats._connCreated)")
                    infoRow("    secured", "\(stats._connSecured)")
                    infoRow("    completed", "\(stats._connCompleted)")
                    infoRow("Connections deleted", "\(stats._connDeleted)")
                    infoRow("Connections subscribed", "\(stats._connSubscribed)")
                    infoRow("    attempts", "\(stats._connSubAttempts)")
                    infoRow("    errors", "\(stats._connSubErrs)")
                    infoRow("From", localTimestamp(statsStartedAt))
                }
            }
        }
    }
}

#Preview {
    ServersSummaryView()
}
