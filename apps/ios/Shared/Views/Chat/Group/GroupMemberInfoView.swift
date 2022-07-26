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
    var member: GroupMember
    @State private var alert: GroupMemberInfoViewAlert? = nil

    enum GroupMemberInfoViewAlert: Identifiable {
        case removeMemberAlert

        var id: GroupMemberInfoViewAlert { get { self } }
    }

    var body: some View {
        NavigationView {
            List {
                groupMemberInfoHeader()
                    .listRowBackground(Color.clear)

                // TODO server status

                Section(header: Text("Info")) {
                    Text("Role: ") + Text(member.memberRole.text)
                    // TODO invited by - need to get contact by contact id
                    Text("Status: ") + Text(member.memberStatus.text)
                    if let conn = member.activeConn {
                        let connLevelDesc = conn.connLevel == 0 ? "Direct" : "Indirect \(conn.connLevel)"
                        Text("Connection level: \(connLevelDesc)")
                    }
                }

                Section {
                    removeMemberButton()
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

    func groupMemberInfoHeader() -> some View {
        VStack(spacing: 0) {
            ProfileImage(imageStr: member.image, color: Color(uiColor: .tertiarySystemFill))
                .frame(width: 108, height: 108)
                .padding(.top, 12)
                .padding(.bottom, 6)
            Text(member.localDisplayName)
                .font(.title)
                .lineLimit(1)
                .padding(.bottom, 2)
            Text(member.fullName)
                .font(.title2)
                .lineLimit(2)
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
                        // TODO navigate back
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
        GroupMemberInfoView(member: GroupMember.sampleData)
    }
}
