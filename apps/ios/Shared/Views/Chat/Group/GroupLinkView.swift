//
//  GroupLinkView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 15.10.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupLinkView: View {
    var groupId: Int64
    @Binding var groupLink: String?
    @Binding var groupLinkMemberRole: GroupMemberRole
    var showTitle: Bool = false
    var creatingGroup: Bool = false
    var linkCreatedCb: (() -> Void)? = nil
    @State private var creatingLink = false
    @State private var alert: GroupLinkAlert?
    @State private var shouldCreate = true

    private enum GroupLinkAlert: Identifiable {
        case deleteLink
        case error(title: LocalizedStringKey, error: LocalizedStringKey?)

        var id: String {
            switch self {
            case .deleteLink: return "deleteLink"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        if creatingGroup {
            groupLinkView()
                .navigationBarBackButtonHidden()
                .toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        Button ("Continue") { linkCreatedCb?() }
                    }
                }
        } else {
            groupLinkView()
        }
    }

    private func groupLinkView() -> some View {
        List {
            Group {
                if showTitle {
                    Text("Group link")
                        .font(.largeTitle)
                        .bold()
                        .fixedSize(horizontal: false, vertical: true)
                }
                Text("You can share a link or a QR code - anybody will be able to join the group. You won't lose members of the group if you later delete it.")
            }
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

            Section {
                if let groupLink = groupLink {
                    Picker("Initial role", selection: $groupLinkMemberRole) {
                        ForEach([GroupMemberRole.member, GroupMemberRole.observer]) { role in
                            Text(role.text)
                        }
                    }
                    .frame(height: 36)
                    SimpleXLinkQRCode(uri: groupLink)
                        .id("simplex-qrcode-view-for-\(groupLink)")
                    Button {
                        showShareSheet(items: [simplexChatLink(groupLink)])
                    } label: {
                        Label("Share link", systemImage: "square.and.arrow.up")
                    }

                    if !creatingGroup {
                        Button(role: .destructive) { alert = .deleteLink } label: {
                            Label("Delete link", systemImage: "trash")
                        }
                    }
                } else {
                    Button(action: createGroupLink) {
                        Label("Create link", systemImage: "link.badge.plus")
                    }
                    .disabled(creatingLink)
                    if creatingLink {
                        ProgressView()
                            .scaleEffect(2)
                            .frame(maxWidth: .infinity)
                    }
                }
            }
            .alert(item: $alert) { alert in
                switch alert {
                case .deleteLink:
                    return Alert(
                        title: Text("Delete link?"),
                        message: Text("All group members will remain connected."),
                        primaryButton: .destructive(Text("Delete")) {
                            Task {
                                do {
                                    try await apiDeleteGroupLink(groupId)
                                    await MainActor.run { groupLink = nil }
                                } catch let error {
                                    logger.error("GroupLinkView apiDeleteGroupLink: \(responseError(error))")
                                }
                            }
                        }, secondaryButton: .cancel()
                    )
                case let .error(title, error):
                    return mkAlert(title: title, message: error)
                }
            }
            .onChange(of: groupLinkMemberRole) { _ in
                Task {
                    do {
                        _ = try await apiGroupLinkMemberRole(groupId, memberRole: groupLinkMemberRole)
                    } catch let error {
                        let a = getErrorAlert(error, "Error updating group link")
                        alert = .error(title: a.title, error: a.message)
                    }
                }
            }
            .onAppear {
                if groupLink == nil && !creatingLink && shouldCreate {
                    createGroupLink()
                }
                shouldCreate = false
            }
        }
        .modifier(ThemedBackground(grouped: true))
    }

    private func createGroupLink() {
        Task {
            do {
                creatingLink = true
                let link = try await apiCreateGroupLink(groupId)
                await MainActor.run {
                    creatingLink = false
                    (groupLink, groupLinkMemberRole) = link
                }
            } catch let error {
                logger.error("GroupLinkView apiCreateGroupLink: \(responseError(error))")
                await MainActor.run {
                    creatingLink = false
                    let a = getErrorAlert(error, "Error creating group link")
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }
}

struct GroupLinkView_Previews: PreviewProvider {
    static var previews: some View {
        @State var groupLink: String? = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"
        @State var noGroupLink: String? = nil

        return Group {
            GroupLinkView(groupId: 1, groupLink: $groupLink, groupLinkMemberRole: Binding.constant(.member))
            GroupLinkView(groupId: 1, groupLink: $noGroupLink, groupLinkMemberRole: Binding.constant(.member))
        }
    }
}

