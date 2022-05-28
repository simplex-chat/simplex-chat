//
//  ContentView.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI

struct ContentView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var alertManager = AlertManager.shared
    @ObservedObject var callController = CallController.shared
    @Binding var userAuthorized: Bool?

    var body: some View {
        ZStack {
            if let step = chatModel.onboardingStage {
                if userAuthorized == true,
                   case .onboardingComplete = step,
                   let user = chatModel.currentUser {
                    ZStack(alignment: .top) {
                        ChatListView(user: user)
                        if chatModel.showCallView, let call = chatModel.activeCall {
                            ActiveCallView(call: call)
                        }
                        IncomingCallView()
                    }
                } else {
                    OnboardingView(onboarding: step)
                }
            }
        }
        .alert(isPresented: $alertManager.presentAlert) { alertManager.alertView! }
    }
}

func connectViaUrl() {
    let m = ChatModel.shared
    if let url = m.appOpenUrl {
        m.appOpenUrl = nil
        AlertManager.shared.showAlert(connectViaUrlAlert(url))
    }
}

func connectViaUrlAlert(_ url: URL) -> Alert {
    var path = url.path
    logger.debug("ChatListView.connectViaUrlAlert path: \(path)")
    if (path == "/contact" || path == "/invitation") {
        path.removeFirst()
        let action: ConnReqType = path == "contact" ? .contact : .invitation
        let link = url.absoluteString.replacingOccurrences(of: "///\(path)", with: "/\(path)")
        let title: LocalizedStringKey
        if case .contact = action { title = "Connect via contact link?" }
        else { title = "Connect via one-time link?" }
        return Alert(
            title: Text(title),
            message: Text("Your profile will be sent to the contact that you received this link from"),
            primaryButton: .default(Text("Connect")) {
                connectViaLink(link)
            },
            secondaryButton: .cancel()
        )
    } else {
        return Alert(title: Text("Error: URL is invalid"))
    }
}

//struct ContentView_Previews: PreviewProvider {
//    static var previews: some View {
//        ContentView(text: "Hello!")
//    }
//}
