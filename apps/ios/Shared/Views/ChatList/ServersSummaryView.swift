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
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State private var serversSummary: PresentedServersSummary? = nil
    @State private var selectedUserCategory: PresentedUserCategory = .allUsers
    @State private var selectedServerType: PresentedServerType = .smp
    @State private var selectedSMPServer: String? = nil
    @State private var selectedXFTPServer: String? = nil
    @State private var timer: Timer? = nil
    @State private var alert: SomeAlert?

    @AppStorage(DEFAULT_SHOW_SUBSCRIPTION_PERCENTAGE) private var showSubscriptionPercentage = false

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
                .navigationBarTitleDisplayMode(.large)
                .modifier(ThemedBackground(grouped: true))
                .toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        shareButton()
                    }
                }
        }
        .onAppear {
            if m.users.filter({ u in u.user.activeUser || !u.user.hidden }).count == 1 {
                selectedUserCategory = .currentUser
            }
            getServersSummary()
            startTimer()
        }
        .onDisappear {
            stopTimer()
        }
        .alert(item: $alert) { $0.alert }
    }

    private func startTimer() {
        timer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { _ in
            if AppChatState.shared.value == .active {
                getServersSummary()
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

    private func stopTimer() {
        timer?.invalidate()
        timer = nil
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

    @ViewBuilder private func viewBody() -> some View {
        if let summ = serversSummary {
            List {
                Group {
                    if m.users.filter({ u in u.user.activeUser || !u.user.hidden }).count > 1 {
                        Picker("User selection", selection: $selectedUserCategory) {
                            Text("All profiles").tag(PresentedUserCategory.allUsers)
                            Text("Current profile").tag(PresentedUserCategory.currentUser)
                        }
                        .pickerStyle(.segmented)
                    }

                    Picker("Server type", selection: $selectedServerType) {
                        Text("Messages").tag(PresentedServerType.smp)
                        Text("Files").tag(PresentedServerType.xftp)
                    }
                    .pickerStyle(.segmented)
                }
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

                switch (selectedUserCategory, selectedServerType) {
                case (.allUsers, .smp):
                    let smpSumm = summ.allUsersSMP
                    let (totals, curr, prev, prox) = (smpSumm.smpTotals, smpSumm.currentlyUsedSMPServers, smpSumm.previouslyUsedSMPServers, smpSumm.onlyProxiedSMPServers)

                    SMPStatsView(stats: totals.stats, statsStartedAt: summ.statsStartedAt)

                    smpSubsSection(totals)

                    if curr.count > 0 {
                        smpServersListView(curr, summ.statsStartedAt, "Connected servers")
                    }
                    if prev.count > 0 {
                        smpServersListView(prev, summ.statsStartedAt, "Previously connected servers")
                    }
                    if prox.count > 0 {
                        smpServersListView(prox, summ.statsStartedAt, "Proxied servers", "You are not connected to these servers. Private routing is used to deliver messages to them.")
                    }

                    ServerSessionsView(sess: totals.sessions)
                case (.currentUser, .smp):
                    let smpSumm = summ.currentUserSMP
                    let (totals, curr, prev, prox) = (smpSumm.smpTotals, smpSumm.currentlyUsedSMPServers, smpSumm.previouslyUsedSMPServers, smpSumm.onlyProxiedSMPServers)

                    SMPStatsView(stats: totals.stats, statsStartedAt: summ.statsStartedAt)

                    smpSubsSection(totals)

                    if curr.count > 0 {
                        smpServersListView(curr, summ.statsStartedAt, "Connected servers")
                    }
                    if prev.count > 0 {
                        smpServersListView(prev, summ.statsStartedAt, "Previously connected servers")
                    }
                    if prox.count > 0 {
                        smpServersListView(prox, summ.statsStartedAt, "Proxied servers", "You are not connected to these servers. Private routing is used to deliver messages to them.")
                    }

                    ServerSessionsView(sess: totals.sessions)
                case (.allUsers, .xftp):
                    let xftpSumm = summ.allUsersXFTP
                    let (totals, curr, prev) = (xftpSumm.xftpTotals, xftpSumm.currentlyUsedXFTPServers, xftpSumm.previouslyUsedXFTPServers)

                    XFTPStatsView(stats: totals.stats, statsStartedAt: summ.statsStartedAt)

                    if curr.count > 0 {
                        xftpServersListView(curr, summ.statsStartedAt, "Connected servers")
                    }
                    if prev.count > 0 {
                        xftpServersListView(prev, summ.statsStartedAt, "Previously connected servers")
                    }

                    ServerSessionsView(sess: totals.sessions)
                case (.currentUser, .xftp):
                    let xftpSumm = summ.currentUserXFTP
                    let (totals, curr, prev) = (xftpSumm.xftpTotals, xftpSumm.currentlyUsedXFTPServers, xftpSumm.previouslyUsedXFTPServers)

                    XFTPStatsView(stats: totals.stats, statsStartedAt: summ.statsStartedAt)

                    if curr.count > 0 {
                        xftpServersListView(curr, summ.statsStartedAt, "Connected servers")
                    }
                    if prev.count > 0 {
                        xftpServersListView(prev, summ.statsStartedAt, "Previously connected servers")
                    }

                    ServerSessionsView(sess: totals.sessions)
                }

                Section {
                    reconnectAllButton()
                    resetStatsButton()
                }
            }
        } else {
            Text("No info, try to reload")
                .foregroundColor(theme.colors.secondary)
                .background(theme.colors.background)
        }
    }

    private func smpSubsSection(_ totals: SMPTotals) -> some View {
        Section {
            infoRow("Active connections", numOrDash(totals.subs.ssActive))
            infoRow("Total", numOrDash(totals.subs.total))
            Toggle("Show percentage", isOn: $showSubscriptionPercentage)
        } header: {
            HStack {
                Text("Message reception")
                SubscriptionStatusIndicatorView(subs: totals.subs, hasSess: totals.sessions.hasSess)
                if showSubscriptionPercentage {
                    SubscriptionStatusPercentageView(subs: totals.subs, hasSess: totals.sessions.hasSess)
                }
            }
        }
    }

    private func reconnectAllButton() -> some View {
        Button {
            alert = SomeAlert(
                alert: Alert(
                    title: Text("Reconnect all servers?"),
                    message: Text("Reconnect all connected servers to force message delivery. It uses additional traffic."),
                    primaryButton: .default(Text("Ok")) {
                        Task {
                            do {
                                try await reconnectAllServers()
                            } catch let error {
                                alert = SomeAlert(
                                    alert: mkAlert(
                                        title: "Error reconnecting servers",
                                        message: "\(responseError(error))"
                                    ),
                                    id: "error reconnecting servers"
                                )
                            }
                        }
                    },
                    secondaryButton: .cancel()
                ),
                id: "reconnect servers question"
            )
        } label: {
            Text("Reconnect all servers")
        }
    }

    @ViewBuilder private func smpServersListView(
        _ servers: [SMPServerSummary],
        _ statsStartedAt: Date,
        _ header: LocalizedStringKey? = nil,
        _ footer: LocalizedStringKey? = nil
    ) -> some View {
        let sortedServers = servers.sorted {
            $0.hasSubs == $1.hasSubs
            ? serverAddress($0.smpServer) < serverAddress($1.smpServer)
            : $0.hasSubs && !$1.hasSubs
        }
        Section {
            ForEach(sortedServers) { server in
                smpServerView(server, statsStartedAt)
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

    private func smpServerView(_ srvSumm: SMPServerSummary, _ statsStartedAt: Date) -> some View {
        NavigationLink(tag: srvSumm.id, selection: $selectedSMPServer) {
            SMPServerSummaryView(
                summary: srvSumm,
                statsStartedAt: statsStartedAt
            )
            .navigationBarTitle("SMP server")
            .navigationBarTitleDisplayMode(.large)
            .modifier(ThemedBackground(grouped: true))
        } label: {
            HStack {
                Text(serverAddress(srvSumm.smpServer))
                    .lineLimit(1)
                if let subs = srvSumm.subs {
                    Spacer()
                    if showSubscriptionPercentage {
                        SubscriptionStatusPercentageView(subs: subs, hasSess: srvSumm.sessionsOrNew.hasSess)
                    }
                    SubscriptionStatusIndicatorView(subs: subs, hasSess: srvSumm.sessionsOrNew.hasSess)
                } else if let sess = srvSumm.sessions {
                    Spacer()
                    Image(systemName: "arrow.up.circle")
                        .symbolRenderingMode(.palette)
                        .foregroundStyle(sessIconColor(sess), Color.clear)
                }
            }
        }
    }

    private func serverAddress(_ server: String) -> String {
        parseServerAddress(server)?.hostnames.first ?? server
    }

    private func sessIconColor(_ sess: ServerSessions) -> Color {
        let online = m.networkInfo.online
        return (
            online && sess.ssConnected > 0
            ? sessionActiveColor
            : Color(uiColor: .tertiaryLabel)
        )
    }

    private var sessionActiveColor: Color {
        let onionHosts = networkUseOnionHostsGroupDefault.get()
        return onionHosts == .require ? .indigo : .accentColor
    }

    @ViewBuilder private func xftpServersListView(
        _ servers: [XFTPServerSummary],
        _ statsStartedAt: Date,
        _ header: LocalizedStringKey? = nil,
        _ footer: LocalizedStringKey? = nil
    ) -> some View {
        let sortedServers = servers.sorted { serverAddress($0.xftpServer) < serverAddress($1.xftpServer) }
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

    private func xftpServerView(_ srvSumm: XFTPServerSummary, _ statsStartedAt: Date) -> some View {
        NavigationLink(tag: srvSumm.id, selection: $selectedXFTPServer) {
            XFTPServerSummaryView(
                summary: srvSumm,
                statsStartedAt: statsStartedAt
            )
            .navigationBarTitle("XFTP server")
            .navigationBarTitleDisplayMode(.large)
            .modifier(ThemedBackground(grouped: true))
        } label: {
            HStack {
                Text(serverAddress(srvSumm.xftpServer))
                    .lineLimit(1)
                if let inProgressIcon = inProgressIcon(srvSumm) {
                    Spacer()
                    Image(systemName: inProgressIcon)
                        .symbolRenderingMode(.palette)
                        .foregroundStyle(sessionActiveColor, Color.clear)
                }
            }
        }
    }

    private func inProgressIcon(_ srvSumm: XFTPServerSummary) -> String? {
        switch (srvSumm.rcvInProgress, srvSumm.sndInProgress, srvSumm.delInProgress) {
        case (false, false, false): nil
        case (true, false, false): "arrow.down.circle"
        case (false, true, false): "arrow.up.circle"
        case (false, false, true): "trash.circle"
        default: "arrow.up.arrow.down.circle"
        }
    }

    private func resetStatsButton() -> some View {
        Button {
            alert = SomeAlert(
                alert: Alert(
                    title: Text("Reset all statistics?"),
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
            Text("Reset all statistics")
        }
    }
}

struct SubscriptionStatusIndicatorView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    var subs: SMPServerSubs
    var hasSess: Bool

    var body: some View {
        let (color, variableValue, opacity, _) = subscriptionStatusColorAndPercentage(
            online: m.networkInfo.online,
            usesProxy: networkUseOnionHostsGroupDefault.get() != .no || groupDefaults.string(forKey: GROUP_DEFAULT_NETWORK_SOCKS_PROXY) != nil,
            subs: subs,
            hasSess: hasSess,
            primaryColor: theme.colors.primary
        )
        if #available(iOS 16.0, *) {
            Image(systemName: "dot.radiowaves.up.forward", variableValue: variableValue)
                .foregroundColor(color)
        } else {
            Image(systemName: "dot.radiowaves.up.forward")
                .foregroundColor(color.opacity(opacity))
        }
    }
}

struct SubscriptionStatusPercentageView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    var subs: SMPServerSubs
    var hasSess: Bool

    var body: some View {
        let (_, _, _, statusPercent) = subscriptionStatusColorAndPercentage(
            online: m.networkInfo.online,
            usesProxy: networkUseOnionHostsGroupDefault.get() != .no || groupDefaults.string(forKey: GROUP_DEFAULT_NETWORK_SOCKS_PROXY) != nil,
            subs: subs,
            hasSess: hasSess,
            primaryColor: theme.colors.primary
        )
        Text(verbatim: "\(Int(floor(statusPercent * 100)))%")
            .foregroundColor(.secondary)
            .font(.caption)
    }
}

func subscriptionStatusColorAndPercentage(online: Bool, usesProxy: Bool, subs: SMPServerSubs, hasSess: Bool, primaryColor: Color) -> (Color, Double, Double, Double) {
    func roundedToQuarter(_ n: Double) -> Double {
        n >= 1 ? 1
        : n <= 0 ? 0
        : (n * 4).rounded() / 4
    }

    let activeColor: Color = usesProxy ? .indigo : primaryColor
    let noConnColorAndPercent: (Color, Double, Double, Double) = (Color(uiColor: .tertiaryLabel), 1, 1, 0)
    let activeSubsRounded = roundedToQuarter(subs.shareOfActive)

    return !online
    ? noConnColorAndPercent
    : (
        subs.total == 0 && !hasSess
        ? (activeColor, 0, 0.33, 0) // On freshly installed app (without chats) and on app start
        : (
            subs.ssActive == 0
            ? (
                hasSess ? (activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive) : noConnColorAndPercent
            )
            : ( // ssActive > 0
                hasSess
                ? (activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
                : (.orange, activeSubsRounded, subs.shareOfActive, subs.shareOfActive) // This would mean implementation error
              )
        )
    )
}

struct SMPServerSummaryView: View {
    var summary: SMPServerSummary
    var statsStartedAt: Date
    @State private var alert: SomeAlert?

    @AppStorage(DEFAULT_SHOW_SUBSCRIPTION_PERCENTAGE) private var showSubscriptionPercentage = false

    var body: some View {
        List {
            Section("Server address") {
                Text(summary.smpServer)
                    .textSelection(.enabled)
                if summary.known == true {
                    NavigationLink {
                        ProtocolServersView(serverProtocol: .smp)
                            .navigationTitle("Your SMP servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Open server settings")
                    }
                }
            }

            if let stats = summary.stats {
                SMPStatsView(stats: stats, statsStartedAt: statsStartedAt)
            }

            if let subs = summary.subs {
                smpSubsSection(subs)
            }

            if let sess = summary.sessions {
                ServerSessionsView(sess: sess)
            }
        }
        .alert(item: $alert) { $0.alert }
    }

    private func smpSubsSection(_ subs: SMPServerSubs) -> some View {
        Section {
            infoRow("Active connections", numOrDash(subs.ssActive))
            infoRow("Pending", numOrDash(subs.ssPending))
            infoRow("Total", numOrDash(subs.total))
            reconnectButton()
        } header: {
            HStack {
                Text("Message reception")
                SubscriptionStatusIndicatorView(subs: subs, hasSess: summary.sessionsOrNew.hasSess)
                if showSubscriptionPercentage {
                    SubscriptionStatusPercentageView(subs: subs, hasSess: summary.sessionsOrNew.hasSess)
                }
            }
        }
    }

    private func reconnectButton() -> some View {
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

struct ServerSessionsView: View {
    var sess: ServerSessions

    var body: some View {
        Section("Transport sessions") {
            infoRow("Connected", numOrDash(sess.ssConnected))
            infoRow("Errors", numOrDash(sess.ssErrors))
            infoRow("Connecting", numOrDash(sess.ssConnecting))
        }
    }
}

struct SMPStatsView: View {
    var stats: AgentSMPServerStatsData
    var statsStartedAt: Date

    var body: some View {
        Section {
            infoRow("Messages sent", numOrDash(stats._sentDirect + stats._sentViaProxy))
            infoRow("Messages received", numOrDash(stats._recvMsgs))
            NavigationLink {
                DetailedSMPStatsView(stats: stats, statsStartedAt: statsStartedAt)
                    .navigationTitle("Detailed statistics")
                    .navigationBarTitleDisplayMode(.large)
                    .modifier(ThemedBackground(grouped: true))
            } label: {
                Text("Details")
            }
        } header: {
            Text("Statistics")
        } footer: {
            Text("Starting from \(localTimestamp(statsStartedAt)).") + Text("\n") + Text("All data is private to your device.")
        }
    }
}

private func numOrDash(_ n: Int) -> String {
    n == 0 ? "-" : "\(n)"
}

struct DetailedSMPStatsView: View {
    var stats: AgentSMPServerStatsData
    var statsStartedAt: Date

    var body: some View {
        List {
            Section("Sent messages") {
                infoRow("Sent total", numOrDash(stats._sentDirect + stats._sentViaProxy))
                infoRowTwoValues("Sent directly", "attempts", stats._sentDirect, stats._sentDirectAttempts)
                infoRowTwoValues("Sent via proxy", "attempts", stats._sentViaProxy, stats._sentViaProxyAttempts)
                infoRowTwoValues("Proxied", "attempts", stats._sentProxied, stats._sentProxiedAttempts)
                Text("Send errors")
                infoRow(Text(verbatim: "AUTH"), numOrDash(stats._sentAuthErrs)).padding(.leading, 24)
                infoRow(Text(verbatim: "QUOTA"), numOrDash(stats._sentQuotaErrs)).padding(.leading, 24)
                infoRow("expired", numOrDash(stats._sentExpiredErrs)).padding(.leading, 24)
                infoRow("other", numOrDash(stats._sentOtherErrs)).padding(.leading, 24)
            }
            Section("Received messages") {
                infoRow("Received total", numOrDash(stats._recvMsgs))
                Text("Receive errors")
                infoRow("duplicates", numOrDash(stats._recvDuplicates)).padding(.leading, 24)
                infoRow("decryption errors", numOrDash(stats._recvCryptoErrs)).padding(.leading, 24)
                infoRow("other errors", numOrDash(stats._recvErrs)).padding(.leading, 24)
                infoRowTwoValues("Acknowledged", "attempts", stats._ackMsgs, stats._ackAttempts)
                Text("Acknowledgement errors")
                infoRow(Text(verbatim: "NO_MSG errors"), numOrDash(stats._ackNoMsgErrs)).padding(.leading, 24)
                infoRow("other errors", numOrDash(stats._ackOtherErrs)).padding(.leading, 24)
            }
            Section("Connections") {
                infoRow("Created", numOrDash(stats._connCreated))
                infoRow("Secured", numOrDash(stats._connCreated))
                infoRow("Completed", numOrDash(stats._connCompleted))
                infoRowTwoValues("Deleted", "attempts", stats._connDeleted, stats._connDelAttempts)
                infoRow("Deletion errors", numOrDash(stats._connDelErrs))
                infoRowTwoValues("Subscribed", "attempts", stats._connSubscribed, stats._connSubAttempts)
                infoRow("Subscriptions ignored", numOrDash(stats._connSubIgnored))
                infoRow("Subscription errors", numOrDash(stats._connSubErrs))
            }
            Section {
                infoRowTwoValues("Enabled", "attempts", stats._ntfKey, stats._ntfKeyAttempts)
                infoRowTwoValues("Disabled", "attempts", stats._ntfKeyDeleted, stats._ntfKeyDeleteAttempts)
            } header: {
                Text("Connection notifications")
            } footer: {
                Text("Starting from \(localTimestamp(statsStartedAt)).")
            }
        }
    }
}

private func infoRowTwoValues(_ title: LocalizedStringKey, _ title2: LocalizedStringKey, _ value: Int, _ value2: Int) -> some View {
    HStack {
        Text(title) + Text(verbatim: " / ").font(.caption2) + Text(title2).font(.caption2)
        Spacer()
        Group {
            if value == 0 && value2 == 0 {
                Text(verbatim: "-")
            } else {
                Text(numOrDash(value)) + Text(verbatim: " / ").font(.caption2) + Text(numOrDash(value2)).font(.caption2)
            }
        }
        .foregroundStyle(.secondary)
    }
}

struct XFTPServerSummaryView: View {
    var summary: XFTPServerSummary
    var statsStartedAt: Date

    var body: some View {
        List {
            Section("Server address") {
                Text(summary.xftpServer)
                    .textSelection(.enabled)
                if summary.known == true {
                    NavigationLink {
                        ProtocolServersView(serverProtocol: .xftp)
                            .navigationTitle("Your XFTP servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Open server settings")
                    }
                }
            }

            if let stats = summary.stats {
                XFTPStatsView(stats: stats, statsStartedAt: statsStartedAt)
            }

            if let sess = summary.sessions {
                ServerSessionsView(sess: sess)
            }
        }
    }
}

struct XFTPStatsView: View {
    var stats: AgentXFTPServerStatsData
    var statsStartedAt: Date
    @State private var expanded = false

    var body: some View {
        Section {
            infoRow("Uploaded", prettySize(stats._uploadsSize))
            infoRow("Downloaded", prettySize(stats._downloadsSize))
            NavigationLink {
                DetailedXFTPStatsView(stats: stats, statsStartedAt: statsStartedAt)
                    .navigationTitle("Detailed statistics")
                    .navigationBarTitleDisplayMode(.large)
                    .modifier(ThemedBackground(grouped: true))
            } label: {
                Text("Details")
            }
        } header: {
            Text("Statistics")
        } footer: {
            Text("Starting from \(localTimestamp(statsStartedAt)).") + Text("\n") + Text("All data is private to your device.")
        }
    }
}

private func prettySize(_ sizeInKB: Int64) -> String {
    let kb: Int64 = 1024
    return sizeInKB == 0 ? "-" : ByteCountFormatter.string(fromByteCount: sizeInKB * kb, countStyle: .binary)
}

struct DetailedXFTPStatsView: View {
    var stats: AgentXFTPServerStatsData
    var statsStartedAt: Date

    var body: some View {
        List {
            Section("Uploaded files") {
                infoRow("Size", prettySize(stats._uploadsSize))
                infoRowTwoValues("Chunks uploaded", "attempts", stats._uploads, stats._uploadAttempts)
                infoRow("Upload errors", numOrDash(stats._uploadErrs))
                infoRowTwoValues("Chunks deleted", "attempts", stats._deletions, stats._deleteAttempts)
                infoRow("Deletion errors", numOrDash(stats._deleteErrs))
            }
            Section {
                infoRow("Size", prettySize(stats._downloadsSize))
                infoRowTwoValues("Chunks downloaded", "attempts", stats._downloads, stats._downloadAttempts)
                Text("Download errors")
                infoRow(Text(verbatim: "AUTH"), numOrDash(stats._downloadAuthErrs)).padding(.leading, 24)
                infoRow("other", numOrDash(stats._downloadErrs)).padding(.leading, 24)
            } header: {
                Text("Downloaded files")
            } footer: {
                Text("Starting from \(localTimestamp(statsStartedAt)).")
            }
        }
    }
}

#Preview {
    ServersSummaryView()
}
