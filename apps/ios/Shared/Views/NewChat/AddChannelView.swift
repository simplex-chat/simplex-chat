//
//  AddChannelView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 23.02.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct AddChannelView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @StateObject private var channelRelaysModel = ChannelRelaysModel.shared
    @StateObject private var ss = SaveableSettings()
    @State private var profile = GroupProfile(displayName: "", fullName: "")
    @FocusState private var focusDisplayName: Bool
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var hasRelays = true
    @State private var groupInfo: GroupInfo? = nil
    @State private var groupLink: GroupLink? = nil
    @State private var groupRelays: [GroupRelay] = []
    @State private var creationInProgress = false
    @State private var showLinkStep = false
    @State private var relayListExpanded = false

    var body: some View {
        Group {
            if showLinkStep, let gInfo = groupInfo {
                linkStepView(gInfo)
            } else if let gInfo = groupInfo {
                progressStepView(gInfo)
            } else {
                profileStepView()
            }
        }
    }

    // MARK: - Step 1: Profile

    private func profileStepView() -> some View {
        List {
            Group {
                ZStack(alignment: .center) {
                    ZStack(alignment: .topTrailing) {
                        ProfileImage(imageStr: profile.image, size: 128)
                        if profile.image != nil {
                            Button {
                                profile.image = nil
                            } label: {
                                Image(systemName: "multiply")
                                    .resizable()
                                    .aspectRatio(contentMode: .fit)
                                    .frame(width: 12)
                            }
                        }
                    }
                    editImageButton { showChooseSource = true }
                        .buttonStyle(BorderlessButtonStyle())
                }
                .frame(maxWidth: .infinity, alignment: .center)
            }
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
            .listRowInsets(EdgeInsets(top: 8, leading: 0, bottom: 8, trailing: 0))

            Section {
                channelNameTextField()
                NavigationLink {
                    NetworkAndServers()
                        .navigationTitle("Network & servers")
                        .modifier(ThemedBackground(grouped: true))
                        .environmentObject(ss)
                } label: {
                    let color: Color = hasRelays ? .accentColor : .orange
                    settingsRow("externaldrive.connected.to.line.below", color: color) {
                        Text("Configure relays").foregroundColor(color)
                    }
                }
                let canCreate = canCreateProfile() && hasRelays && !creationInProgress
                Button(action: createChannel) {
                    settingsRow("checkmark", color: canCreate ? theme.colors.primary : theme.colors.secondary) { Text("Create public channel") }
                }
                .disabled(!canCreate)
            } footer: {
                if !hasRelays {
                    ServersWarningView(warnStr: NSLocalizedString("Enable at least one chat relay in Network & Servers.", comment: "channel creation warning"))
                } else {
                    let name = ChatModel.shared.currentUser?.displayName ?? ""
                    Text("Your profile **\(name)** will be shared with channel relays and subscribers.\nRelays can access channel messages.")
                        .foregroundColor(theme.colors.secondary)
                }
            }
            .compactSectionSpacing()
        }
        .onAppear {
            Task { hasRelays = await checkHasRelays() }
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                focusDisplayName = true
            }
        }
        .confirmationDialog("Channel image", isPresented: $showChooseSource, titleVisibility: .visible) {
            Button("Take picture") { showTakePhoto = true }
            Button("Choose from library") { showImagePicker = true }
        }
        .fullScreenCover(isPresented: $showTakePhoto) {
            ZStack {
                Color.black.edgesIgnoringSafeArea(.all)
                CameraImagePicker(image: $chosenImage)
            }
        }
        .sheet(isPresented: $showImagePicker) {
            LibraryImagePicker(image: $chosenImage) { _ in
                await MainActor.run { showImagePicker = false }
            }
        }
        .onChange(of: chosenImage) { image in
            Task {
                let resized: String? = if let image {
                    await resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)
                } else {
                    nil
                }
                await MainActor.run { profile.image = resized }
            }
        }
        .modifier(ThemedBackground(grouped: true))
    }

    private func channelNameTextField() -> some View {
        ZStack(alignment: .leading) {
            let name = profile.displayName.trimmingCharacters(in: .whitespaces)
            if name != mkValidName(name) {
                Button {
                    showInvalidChannelNameAlert()
                } label: {
                    Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                }
            } else {
                Image(systemName: "pencil").foregroundColor(theme.colors.secondary)
            }
            TextField("Enter channel name…", text: $profile.displayName)
                .padding(.leading, 36)
                .focused($focusDisplayName)
                .submitLabel(.continue)
                .onSubmit {
                    if canCreateProfile() && hasRelays { createChannel() }
                }
        }
    }

    private func canCreateProfile() -> Bool {
        let name = profile.displayName.trimmingCharacters(in: .whitespaces)
        return name != "" && validDisplayName(name)
    }

    private func createChannel() {
        focusDisplayName = false
        profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
        profile.groupPreferences = GroupPreferences(history: GroupPreference(enable: .on))
        creationInProgress = true
        Task {
            do {
                let enabledRelays = try await chooseRandomRelays()
                let relayIds = enabledRelays.compactMap { $0.chatRelayId }
                guard !relayIds.isEmpty else {
                    await MainActor.run {
                        creationInProgress = false
                        hasRelays = false
                    }
                    return
                }
                guard let (gInfo, gLink, gRelays) = try await apiNewPublicGroup(
                    incognito: false, relayIds: relayIds, groupProfile: profile
                ) else {
                    await MainActor.run { creationInProgress = false }
                    return
                }
                await MainActor.run {
                    m.updateGroup(gInfo)
                    m.creatingChannelId = gInfo.id
                    groupInfo = gInfo
                    groupLink = gLink
                    groupRelays = gRelays.sorted { relayDisplayName($0) < relayDisplayName($1) }
                    channelRelaysModel.set(groupId: gInfo.groupId, groupRelays: gRelays)
                    creationInProgress = false
                }
            } catch {
                await MainActor.run {
                    creationInProgress = false
                    showAlert(
                        NSLocalizedString("Error creating channel", comment: "alert title"),
                        message: responseError(error)
                    )
                }
            }
        }
    }

    private let maxRelays = 3

    private func chooseRandomRelays() async throws -> [UserChatRelay] {
        let servers = try await getUserServers()
        // Operator relays are grouped per operator; custom relays (nil operator)
        // are treated independently to maximize trust distribution.
        var operatorGroups: [[UserChatRelay]] = []
        var customRelays: [UserChatRelay] = []
        for op in servers {
            let relays = op.chatRelays.filter { $0.enabled && !$0.deleted && $0.chatRelayId != nil }
            guard !relays.isEmpty else { continue }
            if op.operator != nil {
                operatorGroups.append(relays.shuffled())
            } else {
                customRelays = relays.shuffled()
            }
        }
        var selected: [UserChatRelay] = []
        // Prefer at least one custom relay when available -
        // user's own infrastructure for trust distribution.
        if let relay = customRelays.first {
            selected.append(relay)
            customRelays.removeFirst()
            if selected.count >= maxRelays { return selected }
        }
        // Round-robin across shuffled groups to distribute relays across operators.
        var groups = operatorGroups + customRelays.map { [$0] }
        groups.shuffle()
        let maxDepth = groups.map(\.count).max() ?? 0
        for depth in 0..<maxDepth {
            for group in groups {
                if depth < group.count {
                    selected.append(group[depth])
                    if selected.count >= maxRelays { return selected }
                }
            }
        }
        return selected
    }

    private func checkHasRelays() async -> Bool {
        guard let servers = try? await getUserServers() else { return false }
        return servers.contains { op in
            op.chatRelays.contains { $0.enabled && !$0.deleted && $0.chatRelayId != nil }
        }
    }

    // MARK: - Step 2: Progress

    private func progressStepView(_ gInfo: GroupInfo) -> some View {
        let failedCount = groupRelays.filter { relayMemberConnFailed($0) != nil }.count
        let activeCount = groupRelays.filter { $0.relayStatus == .rsActive && relayMemberConnFailed($0) == nil }.count
        let total = groupRelays.count
        return List {
            Group {
                ProfileImage(imageStr: gInfo.groupProfile.image, size: 128)
                    .frame(maxWidth: .infinity, alignment: .center)

                Text(gInfo.groupProfile.displayName)
                    .font(.headline)
                    .frame(maxWidth: .infinity, alignment: .center)
            }
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
            .listRowInsets(EdgeInsets(top: 8, leading: 0, bottom: 8, trailing: 0))

            Section {
                Button {
                    withAnimation { relayListExpanded.toggle() }
                } label: {
                    HStack(spacing: 8) {
                        if activeCount + failedCount < total {
                            RelayProgressIndicator(active: activeCount, total: total)
                        }
                        if failedCount > 0 {
                            Text(String.localizedStringWithFormat(NSLocalizedString("%d/%d relays active, %d failed", comment: "channel creation progress with errors"), activeCount, total, failedCount))
                        } else {
                            Text(String.localizedStringWithFormat(NSLocalizedString("%d/%d relays active", comment: "channel creation progress"), activeCount, total))
                        }
                        Spacer()
                        Image(systemName: relayListExpanded ? "chevron.up" : "chevron.down")
                            .foregroundColor(theme.colors.secondary)
                    }
                }
                .foregroundColor(theme.colors.onBackground)

                if relayListExpanded {
                    ForEach(groupRelays) { relay in
                        let failed = relayMemberConnFailed(relay)
                        if let err = failed {
                            Button {
                                showAlert(
                                    NSLocalizedString("Relay connection failed", comment: "alert title"),
                                    message: err
                                )
                            } label: {
                                relayRow(relay, connFailed: true)
                            }
                            .buttonStyle(.plain)
                        } else {
                            relayRow(relay, connFailed: false)
                        }
                    }
                }
            }
            .compactSectionSpacing()

            Section {
                Button("Channel link") {
                    if activeCount >= total {
                        showLinkStep = true
                    } else if activeCount > 0 {
                        let actions: [UIAlertAction] = if activeCount + failedCount < total {
                            [
                                UIAlertAction(title: NSLocalizedString("Proceed", comment: "alert action"), style: .default) { _ in showLinkStep = true },
                                UIAlertAction(title: NSLocalizedString("Wait", comment: "alert action"), style: .cancel) { _ in }
                            ]
                        } else {
                            [
                                UIAlertAction(title: NSLocalizedString("Proceed", comment: "alert action"), style: .default) { _ in showLinkStep = true },
                                cancelAlertAction
                            ]
                        }
                        showAlert(
                            NSLocalizedString("Not all relays connected", comment: "alert title"),
                            message: String.localizedStringWithFormat(NSLocalizedString("Channel will start working with %d of %d relays. Proceed?", comment: "alert message"), activeCount, total),
                            actions: { actions }
                        )
                    }
                }
                .disabled(activeCount == 0)
            }
        }
        .navigationTitle("Creating channel")
        .navigationBarBackButtonHidden(true)
        .toolbar {
            ToolbarItem(placement: .navigationBarTrailing) {
                Button("Cancel") { cancelChannelCreation(gInfo) }
            }
        }
        .onChange(of: channelRelaysModel.groupRelays) { relays in
            guard channelRelaysModel.groupId == gInfo.groupId else { return }
            groupRelays = relays.sorted { relayDisplayName($0) < relayDisplayName($1) }
            if relays.allSatisfy({ $0.relayStatus == .rsActive && relayMemberConnFailed($0) == nil }) {
                showLinkStep = true
                channelRelaysModel.reset()
            }
        }
    }

    private func relayMemberConnFailed(_ relay: GroupRelay) -> String? {
        m.groupMembers.first(where: { $0.wrapped.groupMemberId == relay.groupMemberId })?
            .wrapped.activeConn?.connFailedErr
    }

    private func relayRow(_ relay: GroupRelay, connFailed: Bool) -> some View {
        HStack {
            Text(relayDisplayName(relay))
            Spacer()
            relayStatusIndicator(relay.relayStatus, connFailed: connFailed)
        }
    }

    // MARK: - Step 3: Link

    private func linkStepView(_ gInfo: GroupInfo) -> some View {
        GroupLinkView(
            groupId: gInfo.groupId,
            groupLink: $groupLink,
            groupLinkMemberRole: Binding.constant(.observer), // TODO [relays] starting role should be communicated in protocol from owner to relays
            showTitle: false,
            creatingGroup: true,
            isChannel: true
        ) {
            m.creatingChannelId = nil
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                dismissAllSheets(animated: true) {
                    ItemsModel.shared.loadOpenChat(gInfo.id)
                }
            }
        }
        .navigationBarTitle("Channel link")
    }

    private func cancelChannelCreation(_ gInfo: GroupInfo) {
        m.creatingChannelId = nil
        channelRelaysModel.reset()
        dismissAllSheets(animated: true)
        Task {
            do {
                try await apiDeleteChat(type: .group, id: gInfo.apiId)
                await MainActor.run { m.removeChat(gInfo.id) }
            } catch {
                logger.error("cancelChannelCreation error: \(responseError(error))")
            }
        }
    }

    // MARK: - Helpers

    private func showInvalidChannelNameAlert() {
        let validName = mkValidName(profile.displayName)
        if validName == "" {
            showAlert(NSLocalizedString("Invalid name!", comment: "alert title"))
        } else {
            showAlert(
                NSLocalizedString("Invalid name!", comment: "alert title"),
                message: String.localizedStringWithFormat(NSLocalizedString("Correct name to %@?", comment: "alert message"), validName),
                actions: {[
                    UIAlertAction(title: NSLocalizedString("Ok", comment: "alert action"), style: .default) { _ in
                        profile.displayName = validName
                    },
                    cancelAlertAction
                ]}
            )
        }
    }

}

func relayDisplayName(_ relay: GroupRelay) -> String {
    if !relay.userChatRelay.displayName.isEmpty { return relay.userChatRelay.displayName }
    if let domain = relay.userChatRelay.domains.first { return domain }
    if let link = relay.relayLink { return hostFromRelayLink(link) }
    return "relay \(relay.groupRelayId)"
}

func relayStatusIndicator(_ status: RelayStatus, connFailed: Bool = false) -> some View {
    let color: Color = connFailed || status == .rsInactive ? .red : (status == .rsActive ? .green : .yellow)
    let text: LocalizedStringKey = connFailed ? "failed" : status.text
    return HStack(spacing: 4) {
        Circle()
            .fill(color)
            .frame(width: 8, height: 8)
        Text(text)
            .font(.caption)
            .foregroundStyle(.secondary)
        if connFailed {
            Image(systemName: "exclamationmark.circle")
                .foregroundColor(.accentColor)
                .font(.caption)
        }
    }
}

struct RelayProgressIndicator: View {
    var active: Int
    var total: Int

    var body: some View {
        if active == 0 {
            ProgressView()
                .frame(width: 20, height: 20)
        } else {
            ZStack {
                Circle()
                    .stroke(Color(uiColor: .tertiaryLabel), style: StrokeStyle(lineWidth: 2.5))
                Circle()
                    .trim(from: 0, to: Double(active) / Double(max(total, 1)))
                    .stroke(Color.accentColor, style: StrokeStyle(lineWidth: 2.5, lineCap: .round))
                    .rotationEffect(.degrees(-90))
            }
            .frame(width: 20, height: 20)
        }
    }
}

#Preview {
    AddChannelView()
}
