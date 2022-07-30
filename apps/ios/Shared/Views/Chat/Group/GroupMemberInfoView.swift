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
    @State private var alert: GroupMemberInfoViewAlert?
    @State private var connectionStats: ConnectionStats?

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
                    // TODO change role
                    // localizedInfoRow("Role", member.memberRole.text)
                    // TODO invited by - need to get contact by contact id
                    if let conn = member.activeConn {
                        let connLevelDesc = conn.connLevel == 0 ? "direct" : "indirect (\(conn.connLevel))"
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

                Section("For console") {
                    infoRow("Local name", member.localDisplayName)
                    infoRow("Database ID", "\(member.groupMemberId)")
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
        .task {
            do {
                let stats = try await apiGroupMemberInfo(groupInfo.apiId, member.groupMemberId)
                await MainActor.run { connectionStats = stats }
            } catch let error {
                logger.error("apiGroupMemberInfo error: \(responseError(error))")
            }
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
                        _ = try await apiRemoveMember(groupId: member.groupId, memberId: member.groupMemberId)
                        dismiss()
                    } catch let error {
                        logger.error("removeMemberAlert apiRemoveMember error: \(error.localizedDescription)")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }
}

struct GroupMemberInfoView_Previews: PreviewProvider {
    static var previews: some View {
        return GroupMemberInfoView(groupInfo: GroupInfo.sampleData, member: GroupMember.sampleData)
    }
}
