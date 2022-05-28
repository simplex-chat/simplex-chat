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
            if userAuthorized == true {
                if let step = chatModel.onboardingStage {
                    if case .onboardingComplete = step,
                       let user = chatModel.currentUser {
                        ZStack(alignment: .top) {
                            ChatListView(user: user)
                            .onAppear {
                                NtfManager.shared.requestAuthorization(onDeny: {
                                    alertManager.showAlert(notificationAlert())
                                })
                            }
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
        }
        .alert(isPresented: $alertManager.presentAlert) { alertManager.alertView! }
    }

    func notificationAlert() -> Alert {
        Alert(
            title: Text("Notifications are disabled!"),
             message: Text("The app can notify you when you receive messages or contact requests - please open settings to enable."),
             primaryButton: .default(Text("Open Settings")) {
                 DispatchQueue.main.async {
                     UIApplication.shared.open(URL(string: UIApplication.openSettingsURLString)!, options: [:], completionHandler: nil)
                 }
             },
             secondaryButton: .cancel()
         )
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

final class AlertManager: ObservableObject {
    static let shared = AlertManager()
    @Published var presentAlert = false
    @Published var alertView: Alert?

    func showAlert(_ alert: Alert) {
        logger.debug("AlertManager.showAlert")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
            self.alertView = alert
            self.presentAlert = true
        }
    }

    func showAlertMsg(title: LocalizedStringKey, message: LocalizedStringKey? = nil) {
        showAlert(mkAlert(title: title, message: message))
    }
}

func mkAlert(title: LocalizedStringKey, message: LocalizedStringKey? = nil) -> Alert {
    if let message = message {
        return Alert(title: Text(title), message: Text(message))
    } else {
        return Alert(title: Text(title))
    }
}

//struct ContentView_Previews: PreviewProvider {
//    static var previews: some View {
//        ContentView(text: "Hello!")
//    }
//}
