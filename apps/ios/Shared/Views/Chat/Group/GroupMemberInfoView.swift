//
//  GroupMemberInfoView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 25.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupMemberInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    var groupInfo: GroupInfo
    var member: GroupMember
    var connectionStats: ConnectionStats?
    var mainProfile: Profile?
    @State private var alert: GroupMemberInfoViewAlert?
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    enum GroupMemberInfoViewAlert: Identifiable {
        case removeMemberAlert

        var id: GroupMemberInfoViewAlert { get { self } }
    }

    var body: some View {
        NavigationView {
            List {
                groupMemberInfoHeader()
                    .listRowBackground(Color.clear)

                Section("Member") {
                    infoRow("Group", groupInfo.displayName)
                    if let mainProfile = mainProfile {
                        mainProfileRow(mainProfile)
                    }
                    // TODO change role
                    // localizedInfoRow("Role", member.memberRole.text)
                    // TODO invited by - need to get contact by contact id
                    if let conn = member.activeConn {
                        let connLevelDesc = conn.connLevel == 0 ? NSLocalizedString("direct", comment: "connection level description") : String.localizedStringWithFormat(NSLocalizedString("indirect (%d)", comment: "connection level description"), conn.connLevel)
                        infoRow("Connection", connLevelDesc)
                    }
                }

                if let connStats = connectionStats {
                    Section("Servers") {
                        // TODO network connection status
                        smpServers("Receiving via", connStats.rcvServers)
                        smpServers("Sending via", connStats.sndServers)
                    }
                }

                if member.canBeRemoved(membership: groupInfo.membership) {
                    Section {
                        removeMemberButton()
                    }
                }

                if developerTools {
                    Section("For console") {
                        infoRow("Local name", member.localDisplayName)
                        infoRow("Database ID", "\(member.groupMemberId)")
                    }
                }
            }
            .navigationBarHidden(true)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .removeMemberAlert: return removeMemberAlert()
            }
        }
    }

    private func mainProfileRow(_ mainProfile: Profile) -> some View {
        HStack {
            Text("Known main profile")
            Spacer()
            if (mainProfile.image != nil) {
                ProfileImage(imageStr: member.image)
                    .frame(width: 38, height: 38)
                    .padding(.trailing, 2)
            }
            Text(mainProfile.chatViewName)
                .foregroundColor(.secondary)
        }
    }

    private func groupMemberInfoHeader() -> some View {
        VStack {
            ProfileImage(imageStr: member.image, color: Color(uiColor: .tertiarySystemFill))
                .frame(width: 192, height: 192)
                .padding(.top, 12)
                .padding()
            Text(member.displayName)
                .font(.largeTitle)
                .lineLimit(1)
                .padding(.bottom, 2)
            if member.fullName != "" && member.fullName != member.displayName {
                Text(member.fullName)
                    .font(.title2)
                    .lineLimit(2)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    func removeMemberButton() -> some View {
        Button(role: .destructive) {
            alert = .removeMemberAlert
        } label: {
            Label("Remove member", systemImage: "trash")
                .foregroundColor(Color.red)
        }
    }

    private func removeMemberAlert() -> Alert {
        Alert(
            title: Text("Remove member?"),
            message: Text("Member will be removed from group - this cannot be undone!"),
            primaryButton: .destructive(Text("Remove")) {
                Task {
                    do {
                        let member = try await apiRemoveMember(groupInfo.groupId, member.groupMemberId)
                        await MainActor.run {
                            _ = ChatModel.shared.upsertGroupMember(groupInfo, member)
                            dismiss()
                        }
                    } catch let error {
                        logger.error("apiRemoveMember error: \(responseError(error))")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }
}

struct GroupMemberInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupMemberInfoView(groupInfo: GroupInfo.sampleData, member: GroupMember.sampleData)
    }
}
