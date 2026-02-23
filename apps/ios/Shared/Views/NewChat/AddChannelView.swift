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
    @State private var showInvalidNameAlert = false
    @State private var hasRelays = true
    @State private var groupInfo: GroupInfo? = nil
    @State private var groupLink: GroupLink? = nil
    @State private var groupLinkMemberRole: GroupMemberRole = .member
    @State private var groupRelays: [GroupRelay] = []
    @State private var configuredRelays: [Int64: UserChatRelay] = [:]
    @State private var creationInProgress = false
    @State private var showLinkStep = false
    @State private var relayListExpanded = false
    @State private var alert: AddChannelAlert? = nil

    enum AddChannelAlert: Identifiable {
        case retryableError(title: String, message: String)
        case proceedWithPartialRelays(connectedCount: Int, totalCount: Int)

        var id: String {
            switch self {
            case .retryableError: "retryableError"
            case .proceedWithPartialRelays: "proceedPartial"
            }
        }
    }

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
        .alert(item: $alert) { alertItem in
            switch alertItem {
            case let .retryableError(title, message):
                Alert(
                    title: Text(title), message: Text(message),
                    primaryButton: .default(Text("Retry")) { createChannel() },
                    secondaryButton: .cancel()
                )
            case let .proceedWithPartialRelays(connected, total):
                Alert(
                    title: Text("Not all relays connected"),
                    message: Text("Channel will start working with \(connected) of \(total) relays. Proceed?"),
                    primaryButton: .default(Text("Proceed")) { showLinkStep = true },
                    secondaryButton: .cancel(Text("Wait"))
                )
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
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

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
                    settingsRow("checkmark", color: canCreate ? theme.colors.primary : theme.colors.secondary) { Text("Create channel") }
                }
                .disabled(!canCreate)
            } footer: {
                if !hasRelays {
                    ServersWarningView(warnStr: NSLocalizedString("Enable at least one chat relay in Network & Servers.", comment: "channel creation warning"))
                } else {
                    Text("Your profile will be shared with chat relays and subscribers.")
                        .foregroundColor(theme.colors.secondary)
                }
            }
        }
        .task {
            hasRelays = await checkHasRelays()
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
        .alert(isPresented: $showInvalidNameAlert) {
            createInvalidNameAlert(mkValidName(profile.displayName), $profile.displayName)
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
                    showInvalidNameAlert = true
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
                let enabledRelays = try await getEnabledRelays()
                let relayIds = enabledRelays.compactMap { $0.chatRelayId }
                let (gInfo, gLink, gRelays) = try await apiNewPublicGroup(
                    incognito: false, relayIds: relayIds, groupProfile: profile
                )
                await MainActor.run {
                    m.updateGroup(gInfo)
                    configuredRelays = Dictionary(uniqueKeysWithValues: enabledRelays.compactMap { r in
                        r.chatRelayId.map { ($0, r) }
                    })
                    groupInfo = gInfo
                    groupLink = gLink
                    groupRelays = gRelays
                    creationInProgress = false
                }
            } catch {
                await MainActor.run {
                    creationInProgress = false
                    alert = .retryableError(
                        title: NSLocalizedString("Error creating channel", comment: "alert title"),
                        message: responseError(error)
                    )
                }
            }
        }
    }

    private func getEnabledRelays() async throws -> [UserChatRelay] {
        let servers = try await getUserServers()
        return servers.flatMap { op in
            (op.chatRelays ?? []).filter { $0.enabled && !$0.deleted && $0.chatRelayId != nil }
        }
    }

    private func checkHasRelays() async -> Bool {
        guard let servers = try? await getUserServers() else { return false }
        return servers.contains { op in
            (op.chatRelays ?? []).contains { $0.enabled && !$0.deleted }
        }
    }

    // MARK: - Step 2: Progress

    private func progressStepView(_ gInfo: GroupInfo) -> some View {
        let activeCount = groupRelays.filter { $0.relayStatus == .rsActive }.count
        let total = groupRelays.count
        return List {
            Section {
                HStack {
                    Spacer()
                    ProfileImage(imageStr: gInfo.groupProfile.image, size: 96)
                    Spacer()
                }
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)

                Text(gInfo.groupProfile.displayName)
                    .font(.headline)
                    .frame(maxWidth: .infinity, alignment: .center)
                    .listRowBackground(Color.clear)
                    .listRowSeparator(.hidden)
            }

            Section {
                ProgressView(value: Double(activeCount), total: Double(max(total, 1)))

                Button {
                    withAnimation { relayListExpanded.toggle() }
                } label: {
                    HStack {
                        Text("\(activeCount) of \(total) relays connected")
                        Spacer()
                        Image(systemName: relayListExpanded ? "chevron.up" : "chevron.down")
                            .foregroundColor(theme.colors.secondary)
                    }
                }
                .foregroundColor(theme.colors.onBackground)

                if relayListExpanded {
                    ForEach(groupRelays) { relay in
                        HStack {
                            Text(relayDisplayName(relay))
                            Spacer()
                            relayStatusIndicator(relay.relayStatus)
                        }
                    }
                }
            }

            Section {
                Button("Channel link") {
                    if activeCount >= total {
                        showLinkStep = true
                    } else if activeCount > 0 {
                        alert = .proceedWithPartialRelays(connectedCount: activeCount, totalCount: total)
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
            groupRelays = relays
            if let link = channelRelaysModel.groupLink {
                groupLink = link
            }
            if relays.allSatisfy({ $0.relayStatus == .rsActive }) {
                showLinkStep = true
            }
        }
    }

    // MARK: - Step 3: Link

    private func linkStepView(_ gInfo: GroupInfo) -> some View {
        GroupLinkView(
            groupId: gInfo.groupId,
            groupLink: $groupLink,
            groupLinkMemberRole: $groupLinkMemberRole,
            showTitle: false,
            creatingGroup: true,
            isChannel: true
        ) {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                dismissAllSheets(animated: true) {
                    ItemsModel.shared.loadOpenChat(gInfo.id)
                }
            }
        }
        .navigationBarTitle("Channel link")
    }

    private func cancelChannelCreation(_ gInfo: GroupInfo) {
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

    private func relayDisplayName(_ relay: GroupRelay) -> String {
        if let cfg = configuredRelays[relay.userChatRelayId] {
            if !cfg.name.isEmpty { return cfg.name }
            if let domain = cfg.domains.first { return domain }
        }
        if let link = relay.relayLink { return hostFromRelayLink(link) }
        return "relay\(relay.groupRelayId)"
    }

    private func relayStatusIndicator(_ status: RelayStatus) -> some View {
        HStack(spacing: 4) {
            Circle()
                .fill(status == .rsActive ? .green : status == .rsNew ? .red : .orange)
                .frame(width: 8, height: 8)
            Text(status.text)
                .font(.caption)
                .foregroundStyle(.secondary)
        }
    }
}

#Preview {
    AddChannelView()
}
