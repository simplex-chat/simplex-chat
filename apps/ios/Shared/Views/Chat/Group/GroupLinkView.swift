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
    @State private var alert: GroupLinkAlert?

    private enum GroupLinkAlert: Identifiable {
        case deleteLink
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case .deleteLink: return "deleteLink"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        ScrollView {
            VStack (alignment: .leading) {
                Text("You can share a link or a QR code - anybody will be able to join the group. You won't lose members of the group if you later delete it.")
                    .padding(.bottom)
                if let groupLink = groupLink {
                    QRCode(uri: groupLink)
                    HStack {
                        Button {
                            showShareSheet(items: [groupLink])
                        } label: {
                            Label("Share link", systemImage: "square.and.arrow.up")
                        }
                        .padding()

                        Button(role: .destructive) { alert = .deleteLink } label: {
                            Label("Delete link", systemImage: "trash")
                        }
                        .padding()
                    }
                    .frame(maxWidth: .infinity)
                } else {
                    Button {
                        Task {
                            do {
                                groupLink = try await apiCreateGroupLink(groupId)
                            } catch let error {
                                logger.error("GroupLinkView apiCreateGroupLink: \(responseError(error))")
                                let a = getErrorAlert(error, "Error creating group link")
                                alert = .error(title: a.title, error: a.message)
                            }
                        }
                    } label: { Label("Create link", systemImage: "link.badge.plus") }
                    .frame(maxWidth: .infinity)
                }
            }
            .padding()
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
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
                                    await MainActor.run {
                                        groupLink = nil
                                    }
                                } catch let error {
                                    logger.error("GroupLinkView apiDeleteGroupLink: \(responseError(error))")
                                }
                            }
                        }, secondaryButton: .cancel()
                    )
                case let .error(title, error):
                    return Alert(title: Text(title), message: Text(error))
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
            GroupLinkView(groupId: 1, groupLink: $groupLink)
            GroupLinkView(groupId: 1, groupLink: $noGroupLink)
        }
    }
}

