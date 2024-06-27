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
    @State private var selectedXFTPServer: String? = nil
    @State private var alert: SomeAlert?

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
        .alert(item: $alert) { $0.alert }
    }

    private func shareButton() -> some View {
        Button {
            if let serversSummary = serversSummary {
                showShareSheet(items: [encodePrettyPrinted(serversSummary)])
            }
        } label: {
            Image(systemName: "square.and.arrow.up")
        }
        .disabled(serversSummary == nil)
    }

    public func encodePrettyPrinted<T: Encodable>(_ value: T) -> String {
        let encoder = jsonEncoder
        encoder.outputFormatting = .prettyPrinted
        let data = try! encoder.encode(value)
        return String(decoding: data, as: UTF8.self)
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
                            smpServersListView(summ.allUsedSMP, showReconnectButton: true, summ.statsStartedAt, "Current session")
                        }
                        if summ.allPrevSMP.count > 0 {
                            smpServersListView(summ.allPrevSMP, showReconnectButton: false, summ.statsStartedAt, "Previously used")
                        }
                        if summ.allProxSMP.count > 0 {
                            smpServersListView(summ.allProxSMP, showReconnectButton: false, summ.statsStartedAt, "Proxied", "You are not connected to these servers directly.")
                        }
                        resetStatsButtonSection()
                    } else {
                        noCategoryInfoText()
                    }
                case (.currentUser, .smp):
                    if summ.userUsedSMP.count > 0 || summ.userPrevSMP.count > 0 || summ.userProxSMP.count > 0 {
                        if summ.userUsedSMP.count > 0 {
                            smpServersListView(summ.userUsedSMP, showReconnectButton: true, summ.statsStartedAt, "Current session")
                        }
                        if summ.userPrevSMP.count > 0 {
                            smpServersListView(summ.userPrevSMP, showReconnectButton: false, summ.statsStartedAt, "Previously used")
                        }
                        if summ.userProxSMP.count > 0 {
                            smpServersListView(summ.userProxSMP, showReconnectButton: false, summ.statsStartedAt, "Proxied", "You are not connected to these servers directly.")
                        }
                        resetStatsButtonSection()
                    } else {
                        noCategoryInfoText()
                    }
                case (.allUsers, .xftp):
                    if summ.allUsedXFTP.count > 0 || summ.allPrevXFTP.count > 0 {
                        if summ.allUsedXFTP.count > 0 {
                            xftpServersListView(summ.allUsedXFTP, summ.statsStartedAt, "Current session")
                        }
                        if summ.allPrevXFTP.count > 0 {
                            xftpServersListView(summ.allPrevXFTP, summ.statsStartedAt, "Previously used")
                        }
                        resetStatsButtonSection()
                    } else {
                        noCategoryInfoText()
                    }
                case (.currentUser, .xftp):
                    if summ.userUsedXFTP.count > 0 || summ.userPrevXFTP.count > 0 {
                        if summ.userUsedXFTP.count > 0 {
                            xftpServersListView(summ.userUsedXFTP, summ.statsStartedAt, "Current session")
                        }
                        if summ.userPrevXFTP.count > 0 {
                            xftpServersListView(summ.userPrevXFTP, summ.statsStartedAt, "Previously used")
                        }
                        resetStatsButtonSection()
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
            HStack {
                if let subs = server.subs {
                    SubscriptionStatusView(activeSubs: subs.ssActive, pendingSubs: subs.ssPending)
                        .frame(width: 16, alignment: .center)
                        .padding(.trailing, 4)
                }
                Text(serverAddress(server.smpServer))
                    .lineLimit(1)
            }
        }
    }

    private func serverAddress(_ server: String) -> String {
        parseServerAddress(server)?.hostnames.first ?? server
    }

    @ViewBuilder private func xftpServersListView(
        _ servers: [XFTPServerSummary],
        _ statsStartedAt: Date,
        _ header: LocalizedStringKey? = nil,
        _ footer: LocalizedStringKey? = nil
    ) -> some View {
        let sortedServers = servers.sorted { serverAddress($0.xftpServer).compare(serverAddress($1.xftpServer)) == .orderedAscending }
        Section {
            ForEach(sortedServers) { server in
                xftpServerView(server, statsStartedAt)
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

    private func xftpServerView(_ server: XFTPServerSummary, _ statsStartedAt: Date) -> some View {
        NavigationLink(tag: server.id, selection: $selectedXFTPServer) {
            XFTPServerSummaryView(
                summary: server,
                statsStartedAt: statsStartedAt
            )
            .navigationBarTitle("XFTP server")
            .navigationBarTitleDisplayMode(.large)
        } label: {
            Text(serverAddress(server.xftpServer))
                .lineLimit(1)
        }
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

    private func resetStatsButtonSection() -> some View {
        Section {
            Button {
                alert = SomeAlert(
                    alert: Alert(
                        title: Text("Reset servers statistics?"),
                        message: Text("Servers statistics will be reset - this cannot be undone!"),
                        primaryButton: .destructive(Text("Reset")) {
                            Task {
                                do {
                                    try await resetAgentServersStats()
                                    getServersSummary()
                                } catch let error {
                                    alert = SomeAlert(
                                        alert: mkAlert(
                                            title: "Error resetting statistics",
                                            message: "\(responseError(error))"
                                        ),
                                        id: "error resetting statistics"
                                    )
                                }
                            }
                        },
                        secondaryButton: .cancel()
                    ),
                    id: "reset statistics question"
                )
            } label: {
                Text("Reset statistics")
            }
        }
    }

    private func getServersSummary() {
        do {
            serversSummary = try getAgentServersSummary()
        } catch let error {
            logger.error("getAgentServersSummary error: \(responseError(error))")
        }
    }
}

struct SubscriptionStatusView: View {
    @EnvironmentObject var m: ChatModel
    var activeSubs: Int
    var pendingSubs: Int
    var variableValueAsPercentage: Bool = false

    var body: some View {
        let netInfo = m.networkInfo
        if netInfo.online {
            let (image, color, variableValue, opacity) = networkOnlineImage(netInfo.networkType)
            if #available(iOS 16.0, *) {
                Image(systemName: image, variableValue: variableValue)
                    .foregroundColor(color)
            } else {
                Image(systemName: image)
                    .foregroundColor(color.opacity(opacity))
            }
        } else {
            Image(systemName: "wifi.slash")
                .foregroundColor(.secondary)
        }
    }

    func networkOnlineImage(_ networkType: UserNetworkType) -> (String, Color, Double, Double) {
        switch networkType {
        case .cellular:
            let (color, variableValue, opacity) = cellularbarsColor
            return ("cellularbars", color, variableValue, opacity)
        default:
            let (color, variableValue, opacity) = wifiColor
            return ("wifi", color, variableValue, opacity)
        }
    }

    // We manipulate variableValue so all "wifi" sections are filled only with 100% active subs,
    // unless variableValueAsPercentage is true; same for cellularbarsColor
    var wifiColor: (Color, Double, Double) {
        if activeSubs > 0 {
            let wifiVariableValue = (
                variableValueAsPercentage ? activeSubsPercentage
                :   ( // "wifi" has 3 sections
                    activeSubsPercentage >= 1 ? 1
                    : (activeSubsPercentage >= 0.5 && activeSubsPercentage < 1) ? 0.6
                    : (activeSubsPercentage > 0 && activeSubsPercentage < 0.5) ? 0.3
                    : 0
                    )
            )
            return (.accentColor, wifiVariableValue, activeSubsPercentage)
        } else {
            return (.secondary, 1, 1)
        }
    }

    var cellularbarsColor: (Color, Double, Double) {
        if activeSubs > 0 {
            let wifiVariableValue = (
                variableValueAsPercentage ? activeSubsPercentage
                : ( // "cellularbars" has 4 sections
                    activeSubsPercentage >= 1 ? 1
                    : (activeSubsPercentage >= 0.67 && activeSubsPercentage < 1) ? 0.7
                    : (activeSubsPercentage >= 0.33 && activeSubsPercentage < 0.67) ? 0.45
                    : (activeSubsPercentage > 0 && activeSubsPercentage < 0.33) ? 0.2
                    : 0
                  )
            )
            return (.accentColor, wifiVariableValue, activeSubsPercentage)
        } else {
            return (.secondary, 1, 1)
        }
    }

    var activeSubsPercentage: Double {
        let total = activeSubs + pendingSubs
        guard total != 0 else { return 0.0 }
        return Double(activeSubs) / Double(total)
    }
}

struct SMPServerSummaryView: View {
    var summary: SMPServerSummary
    var showReconnectButton: Bool
    var statsStartedAt: Date
    @State private var alert: SomeAlert?

    var body: some View {
        List {
            Section {
                Text(summary.smpServer)
                    .textSelection(.enabled)
                if let known = summary.known, !known {
                    Button {
                        addKnownServer()
                    } label: {
                        Text("Add as known")
                    }
                }
            } header: {
                Text("Server address")
            } footer: {
                if let known = summary.known, known {
                    Text("Server is configured in **Settings** → **Network & servers**.")
                }
            }

            if showReconnectButton {
                reconnectButtonSection()
            }

            if let subs = summary.subs {
                subsSection(subs)
            }

            if let sess = summary.sessions {
                sessionsSection(sess)
            }

            if let stats = summary.stats {
                statsSection(stats)
            }
        }
        .alert(item: $alert) { $0.alert }
    }

    private func reconnectButtonSection() -> some View {
        Section {
            Button {
                alert = SomeAlert(
                    alert: Alert(
                        title: Text("Reconnect server?"),
                        message: Text("Reconnect server to force message delivery. It uses additional traffic."),
                        primaryButton: .default(Text("Ok")) {
                            Task {
                                do {
                                    try await reconnectServer(smpServer: summary.smpServer)
                                } catch let error {
                                    alert = SomeAlert(
                                        alert: mkAlert(
                                            title: "Error reconnecting server",
                                            message: "\(responseError(error))"
                                        ),
                                        id: "error reconnecting server"
                                    )
                                }
                            }
                        },
                        secondaryButton: .cancel()
                    ),
                    id: "reconnect server question"
                )
            } label: {
                Text("Reconnect")
            }
        }
    }

    private func subsSection(_ subs: SMPServerSubs) -> some View {
        Section {
            infoRow("Active", "\(subs.ssActive)")
            infoRow("Pending", "\(subs.ssPending)")
        } header: {
            HStack {
                Text("Subscriptions")
                SubscriptionStatusView(activeSubs: subs.ssActive, pendingSubs: subs.ssPending)
            }
        }
    }

    private func sessionsSection(_ sess: ServerSessions) -> some View {
        Section("Sessions") {
            infoRow("Connected", "\(sess.ssConnected)")
            infoRow("Errors", "\(sess.ssErrors)")
            infoRow("Connecting", "\(sess.ssConnecting)")
        }
    }

    private func statsSection(_ stats: AgentSMPServerStatsData) -> some View {
        Section("Statistics") {
            infoRow("Starting from", localTimestamp(statsStartedAt))
            infoRow("Messages sent directly", "\(stats._sentDirect)")
            indentedInfoRow("attempts", "\(stats._sentDirectAttempts)")
            infoRow("Messages sent via proxy", "\(stats._sentViaProxy)")
            indentedInfoRow("attempts", "\(stats._sentViaProxyAttempts)")
            infoRow("Messages sent to proxy", "\(stats._sentProxied)")
            indentedInfoRow("attempts", "\(stats._sentProxiedAttempts)")
            infoRow("Sending AUTH errors", "\(stats._sentAuthErrs)")
            indentedInfoRow("QUOTA errors", "\(stats._sentQuotaErrs)")
            indentedInfoRow("expired", "\(stats._sentExpiredErrs)")
            indentedInfoRow("other errors", "\(stats._sentOtherErrs)")
            infoRow("Messages received", "\(stats._recvMsgs)")
            indentedInfoRow("duplicates", "\(stats._recvDuplicates)")
            indentedInfoRow("decryption errors", "\(stats._recvCryptoErrs)")
            indentedInfoRow("other errors", "\(stats._recvErrs)")
            infoRow("Messages acknowledged", "\(stats._ackMsgs)")
            indentedInfoRow("attempts", "\(stats._ackAttempts)")
            infoRow("Connections created", "\(stats._connCreated)")
            indentedInfoRow("secured", "\(stats._connSecured)")
            indentedInfoRow("completed", "\(stats._connCompleted)")
            infoRow("Connections deleted", "\(stats._connDeleted)")
            infoRow("Connections subscribed", "\(stats._connSubscribed)")
            indentedInfoRow("attempts", "\(stats._connSubAttempts)")
            indentedInfoRow("errors", "\(stats._connSubErrs)")
        }
    }

    func addKnownServer() {
        Task {
            do {
                try await addKnownProtoServer(server: summary.smpServer)
                await MainActor.run {
                    // TODO disable button, etc.
                }
            } catch let error {
                await MainActor.run {
                    alert = errorAddingServerAlert(error)
                }
            }
        }
    }
}

private func indentedInfoRow(_ title: LocalizedStringKey, _ value: String) -> some View {
    HStack {
        Text(title)
            .padding(.leading, 24)
        Spacer()
        Text(value)
            .foregroundStyle(.secondary)
    }
}

private func errorAddingServerAlert(_ error: Error) -> SomeAlert {
    SomeAlert(
        alert: mkAlert(
            title: "Error adding server",
            message: "Make sure server address is in correct format (\(responseError(error)))."
        ),
        id: "error saving server"
    )
}

struct XFTPServerSummaryView: View {
    var summary: XFTPServerSummary
    var statsStartedAt: Date
    @State private var alert: SomeAlert?

    var body: some View {
        List {
            Section {
                Text(summary.xftpServer)
                    .textSelection(.enabled)
                if let known = summary.known, !known {
                    Button {
                        addKnownServer()
                    } label: {
                        Text("Add as known")
                    }
                }
            } header: {
                Text("Server address")
            } footer: {
                if let known = summary.known, known {
                    Text("Server is configured in **Settings** → **Network & servers**.")
                }
            }

            if let sess = summary.sessions {
                sessionsSection(sess)
            }

            inProgressSection()

            if let stats = summary.stats {
                statsSection(stats)
            }
        }
        .alert(item: $alert) { $0.alert }
    }

    private func sessionsSection(_ sess: ServerSessions) -> some View {
        Section("Sessions") {
            infoRow("Connected", "\(sess.ssConnected)")
            infoRow("Errors", "\(sess.ssErrors)")
            infoRow("Connecting", "\(sess.ssConnecting)")
        }
    }

    private func inProgressSection() -> some View {
        Section("In progress") {
            localizedInfoRow("Download", boolYesNo(summary.rcvInProgress))
            localizedInfoRow("Upload", boolYesNo(summary.sndInProgress))
            localizedInfoRow("Deletion", boolYesNo(summary.delInProgress))
        }
    }

    private func boolYesNo(_ b: Bool) -> LocalizedStringKey {
        b ? "yes" : "no"
    }

    private func statsSection(_ stats: AgentXFTPServerStatsData) -> some View {
        Section("Statistics") {
            infoRow("Starting from", localTimestamp(statsStartedAt))
            infoRow("Chunks uploaded", "\(stats._uploads)")
            indentedInfoRow("attempts", "\(stats._uploadAttempts)")
            indentedInfoRow("errors", "\(stats._uploadErrs)")
            infoRow("Chunks downloaded", "\(stats._downloads)")
            indentedInfoRow("attempts", "\(stats._downloadAttempts)")
            indentedInfoRow("AUTH errors", "\(stats._downloadAuthErrs)")
            indentedInfoRow("other errors", "\(stats._downloadErrs)")
            infoRow("Chunks deleted", "\(stats._deletions)")
            indentedInfoRow("attempts", "\(stats._deleteAttempts)")
            indentedInfoRow("errors", "\(stats._deleteErrs)")
        }
    }

    func addKnownServer() {
        Task {
            do {
                try await addKnownProtoServer(server: summary.xftpServer)
                await MainActor.run {
                    // TODO disable button, etc.
                }
            } catch let error {
                await MainActor.run {
                    alert = errorAddingServerAlert(error)
                }
            }
        }
    }
}

#Preview {
    ServersSummaryView()
}
