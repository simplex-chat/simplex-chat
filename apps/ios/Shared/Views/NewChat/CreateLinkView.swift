//
//  CreateLinkView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum CreateLinkTab {
    case oneTime
    case longTerm

    var title: LocalizedStringKey {
        switch self {
        case .oneTime: return "One-time invitation link"
        case .longTerm: return "Your SimpleX address"
        }
    }
}

struct CreateLinkView: View {
    @EnvironmentObject var m: ChatModel
    @State var selection: CreateLinkTab
    @State var connReqInvitation: String = ""
    @State var contactConnection: PendingContactConnection? = nil
    @State private var creatingConnReq = false
    var viaNavLink = false

    var body: some View {
        if viaNavLink {
            createLinkView()
        } else {
            NavigationView {
                createLinkView()
            }
        }
    }

    private func createLinkView() -> some View {
        TabView(selection: $selection) {
            AddContactView(contactConnection: $contactConnection, connReqInvitation: connReqInvitation)
                .tabItem {
                    Label(
                        connReqInvitation == ""
                        ? "Create one-time invitation link"
                        : "One-time invitation link",
                        systemImage: "1.circle"
                    )
                }
                .tag(CreateLinkTab.oneTime)
            UserAddressView(viaCreateLinkView: true)
                .tabItem {
                    Label("Your SimpleX address", systemImage: "infinity.circle")
                }
                .tag(CreateLinkTab.longTerm)
        }
        .onChange(of: selection) { _ in
            if case .oneTime = selection, connReqInvitation == "", contactConnection == nil && !creatingConnReq {
                createInvitation()
            }
        }
        .onAppear { m.connReqInv = connReqInvitation }
        .onDisappear { m.connReqInv = nil }
        .navigationTitle(selection.title)
        .navigationBarTitleDisplayMode(.large)
    }

    private func createInvitation() {
        creatingConnReq = true
        Task {
            if let (connReq, pcc) = await apiAddContact(incognito: incognitoGroupDefault.get()) {
                await MainActor.run {
                    connReqInvitation = connReq
                    contactConnection = pcc
                    m.connReqInv = connReq
                }
            } else {
                await MainActor.run {
                    creatingConnReq = false
                }
            }
        }
    }
}

struct CreateLinkView_Previews: PreviewProvider {
    static var previews: some View {
        CreateLinkView(selection: CreateLinkTab.oneTime)
    }
}
