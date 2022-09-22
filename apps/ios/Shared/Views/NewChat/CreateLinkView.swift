//
//  CreateLinkView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

enum CreateLinkTab {
    case oneTime
    case longTerm
}

struct CreateLinkView: View {
    @State var selection: CreateLinkTab
    @State var connReqInvitation: String = ""
    @State private var creatingConnReq = false
    var viaSettings = false

    private let minDragTranslationForSwipe: CGFloat = 50

    var body: some View {
        TabView(selection: $selection) {
            AddContactView(connReqInvitation: connReqInvitation, viaSettings: viaSettings)
                .tabItem {
                    Label(
                        connReqInvitation == ""
                        ? "Create one-time invitation link"
                        : "One-time invitation link",
                        systemImage: "1.circle"
                    )
                }
                .tag(CreateLinkTab.oneTime)
                .highPriorityGesture(DragGesture().onEnded({
                    handleSwipe(translation: $0.translation.width)
                }))
            UserAddress(viaSettings: viaSettings)
                .tabItem {
                    Label("Your contact address", systemImage: "infinity.circle")
                }
                .tag(CreateLinkTab.longTerm)
                .highPriorityGesture(DragGesture().onEnded({
                    handleSwipe(translation: $0.translation.width)
                }))
        }
        .onChange(of: selection) { _ in
            if case .oneTime = selection, connReqInvitation == "" && !creatingConnReq {
                createInvitation()
            }
        }
    }

    private func createInvitation() {
        creatingConnReq = true
        Task {
            let connReq = await apiAddContact()
            await MainActor.run {
                if let connReq = connReq {
                    connReqInvitation = connReq
                } else {
                    creatingConnReq = false
                }
            }
        }
    }

    private func handleSwipe(translation: CGFloat) {
        if translation > minDragTranslationForSwipe && selection == .longTerm {
            selection = .oneTime
        } else if translation < -minDragTranslationForSwipe && selection == .oneTime {
            selection = .longTerm
        }
    }
}

struct CreateLinkView_Previews: PreviewProvider {
    static var previews: some View {
        CreateLinkView(selection: CreateLinkTab.oneTime)
    }
}
