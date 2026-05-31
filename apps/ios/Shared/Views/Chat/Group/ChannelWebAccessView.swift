//
//  ChannelWebAccessView.swift
//  SimpleX (iOS)
//
//  Created by simplex.chat on 31/05/2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChannelWebAccessView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var groupInfo: GroupInfo
    @State private var webPage: String
    @State private var allowEmbedding: Bool
    @State private var saving = false
    @State private var groupRelays: [GroupRelay] = []

    init(groupInfo: Binding<GroupInfo>) {
        _groupInfo = groupInfo
        let access = groupInfo.wrappedValue.groupProfile.publicGroup?.publicGroupAccess
        _webPage = State(initialValue: access?.groupWebPage ?? "")
        _allowEmbedding = State(initialValue: access?.allowEmbedding ?? false)
    }

    var body: some View {
        let isChannel = groupInfo.useRelays
        List {
            Section {
                Text("Create a webpage to show your channel preview to visitors before they subscribe. Host it yourself or use any static hosting.")
                    .foregroundColor(theme.colors.secondary)
            }
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)

            if let code = embedCode {
                Section {
                    Text(code)
                        .font(.system(.caption, design: .monospaced))
                        .textSelection(.enabled)
                    Button {
                        UIPasteboard.general.string = code
                    } label: {
                        Label("Copy page code", systemImage: "doc.on.doc")
                    }
                } header: {
                    Text("Page code")
                } footer: {
                    Text("Add this code to your HTML page. It will load and display the last messages from your channel.")
                }
            }

            Section {
                TextField("https://", text: $webPage)
                    .keyboardType(.URL)
                    .autocapitalization(.none)
                    .disableAutocorrection(true)
            } header: {
                Text("Page URL")
            } footer: {
                Text("Enter the URL where you host this page. It will be shown to subscribers and used to allow loading the preview.")
            }

            Section {
                Toggle("Allow any website to embed", isOn: $allowEmbedding)
            } footer: {
                Text("When off, only your page URL above can load the preview.")
            }

            Section {
                Button {
                    saveAccess()
                } label: {
                    HStack {
                        Text(isChannel ? "Save and notify subscribers" : "Save and notify members")
                        if saving { Spacer(); ProgressView() }
                    }
                }
                .disabled(!hasChanges || saving)
            }
        }
        .modifier(ThemedBackground(grouped: true))
        .onAppear {
            Task {
                let relays = await apiGetGroupRelays(groupInfo.groupId)
                await MainActor.run { groupRelays = relays }
            }
        }
        .onDisappear {
            if hasChanges {
                showAlert(
                    title: NSLocalizedString("Save webpage settings?", comment: "alert title"),
                    message: NSLocalizedString("Webpage settings were changed. If you save, the updated settings will be sent to subscribers.", comment: "alert message"),
                    buttonTitle: NSLocalizedString("Save", comment: "alert button"),
                    buttonAction: saveAccess,
                    cancelButton: true
                )
            }
        }
    }

    private var hasChanges: Bool {
        let access = groupInfo.groupProfile.publicGroup?.publicGroupAccess
        let currentWebPage = access?.groupWebPage ?? ""
        let currentEmbedding = access?.allowEmbedding ?? false
        return webPage != currentWebPage || allowEmbedding != currentEmbedding
    }

    private var relayUrls: [String] {
        groupRelays.compactMap { $0.relayCap.baseWebUrl }
    }

    private var embedCode: String? {
        guard let pg = groupInfo.groupProfile.publicGroup else { return nil }
        guard !relayUrls.isEmpty else { return nil }
        let publicGroupId = pg.publicGroupId
        let groupLink = pg.groupLink
        let urls = relayUrls.joined(separator: ",")
        return """
            <div data-simplex-group-preview
                 data-relay-urls="\(urls)"
                 data-public-group-id="\(publicGroupId)"
                 data-group-link="\(groupLink)"></div>
            <script src="https://simplex.chat/js/channel-preview.js"></script>
            """
    }

    private func saveAccess() {
        saving = true
        Task {
            do {
                var gp = groupInfo.groupProfile
                if var pg = gp.publicGroup {
                    let trimmedPage = webPage.trimmingCharacters(in: .whitespacesAndNewlines)
                    let existingAccess = pg.publicGroupAccess
                    pg.publicGroupAccess = PublicGroupAccess(
                        groupWebPage: trimmedPage.isEmpty ? nil : trimmedPage,
                        groupDomain: existingAccess?.groupDomain,
                        domainWebPage: existingAccess?.domainWebPage ?? false,
                        allowEmbedding: allowEmbedding
                    )
                    gp.publicGroup = pg
                }
                let gInfo = try await apiUpdateGroup(groupInfo.groupId, gp)
                await MainActor.run {
                    groupInfo = gInfo
                    ChatModel.shared.updateGroup(gInfo)
                    saving = false
                }
            } catch {
                logger.error("ChannelWebAccessView apiUpdateGroup error: \(responseError(error))")
                await MainActor.run { saving = false }
            }
        }
    }
}
