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
        List {
            if let code = embedCode {
                webpageInfo("Create a webpage to show your channel preview to visitors before they subscribe. Host it yourself or use any static hosting.")

                Section {
                    ScrollView {
                        Text(code)
                            .font(.system(.caption, design: .monospaced))
                            .textSelection(.enabled)
                    }
                    .frame(maxHeight: 88)
                    Button {
                        UIPasteboard.general.string = code
                    } label: {
                        Label("Copy code", systemImage: "doc.on.doc")
                    }
                } header: {
                    Text("Webpage code")
                } footer: {
                    Text("Add this code to your webpage. It will display the preview of your channel / group.")
                }
            } else {
                webpageInfo("Used chat relays do not support webpages.")
            }

            Section {
                TextField("https://", text: $webPage)
                    .keyboardType(.URL)
                    .autocapitalization(.none)
                    .disableAutocorrection(true)
            } header: {
                Text("Enter webpage URL")
            } footer: {
                Text("It will be shown to subscribers and used to allow loading the preview.")
            }

            Section {
                Toggle("Allow anyone to embed", isOn: $allowEmbedding)
            } footer: {
                Text(allowEmbedding ? "Any webpage can show the preview." : "Only your page above can show the preview.")
            }

            Section {
                Button {
                    saveAccess()
                } label: {
                    HStack {
                        Text(groupInfo.isChannel ? "Save and notify subscribers" : "Save and notify members")
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

    private func webpageInfo(_ text: LocalizedStringKey) -> some View {
        Section {
            Text(text).foregroundColor(theme.colors.secondary)
        }
        .listRowBackground(Color.clear)
        .listRowSeparator(.hidden)
        .listRowInsets(EdgeInsets(top: 8, leading: 16, bottom: 0, trailing: 16))
    }

    private var hasChanges: Bool {
        let access = groupInfo.groupProfile.publicGroup?.publicGroupAccess
        let currentWebPage = access?.groupWebPage ?? ""
        let currentEmbedding = access?.allowEmbedding ?? false
        return webPage != currentWebPage || allowEmbedding != currentEmbedding
    }

    private var relayDomains: [String] {
        groupRelays.compactMap { $0.relayCap.webDomain }
    }

    private var embedCode: String? {
        if let pg = groupInfo.groupProfile.publicGroup,
           !relayDomains.isEmpty {
            """
            <div data-simplex-channel-preview
              data-channel-link="\(pg.groupLink)"
              data-channel-id="\(pg.publicGroupId)"
              data-relay-domains="\(relayDomains.joined(separator: ","))"
              data-app-download-buttons="on"
              data-color-scheme="light"
            ></div>
            <script src="https://simplex.chat/js/channel-preview.js"></script>
            """
        } else {
            nil
        }
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
                        simplexName: existingAccess?.simplexName,
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
