//
//  ContentView.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import LocalAuthentication
import SwiftUI

struct ContentView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var alertManager = AlertManager.shared
    @State private var showNotificationAlert = false
    @State private var showChats = false

    var body: some View {
        ZStack {
            if let step = chatModel.onboardingStage {
                if case .onboardingComplete = step,
                   let user = chatModel.currentUser,
                   showChats {
                    ChatListView(user: user)
                    .onAppear {
                        NtfManager.shared.requestAuthorization(onDeny: {
                            alertManager.showAlert(notificationAlert())
                        })
                    }
                } else {
                    OnboardingView(onboarding: step)
                }
            }
        }
        .alert(isPresented: $alertManager.presentAlert) { alertManager.alertView! }
        .onAppear {
            // TODO authenticate only if onboarding is complete and preference is set
            authenticate()
        }
    }

    func authenticate() {
        let laContext = LAContext()
        var authAvailabilityError: NSError?
        if laContext.canEvaluatePolicy(.deviceOwnerAuthentication, error: &authAvailabilityError) {
            let reason = "Access chats"
            laContext.evaluatePolicy(.deviceOwnerAuthentication, localizedReason: reason) { success, authError in
                DispatchQueue.main.async {
                    if success {
                        showChats = true
                    } else {
                        logger.error("authentication error: \(authError.debugDescription)")
                        AlertManager.shared.showAlertMsg(
                            title: "Authentication failed",
                            message: "You could not be verified; please try again."
                        )
                    }
                }
            }
        } else {
            logger.error("authentication availability error: \(authAvailabilityError.debugDescription)")
            AlertManager.shared.showAlertMsg(
                title: "Authentication unavailable",
                message: "You device is not configured for authentication."
            )
            // TODO turn off preference
            showChats = true
        }
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
        if let message = message {
            showAlert(Alert(title: Text(title), message: Text(message)))
        } else {
            showAlert(Alert(title: Text(title)))
        }
    }
}

//struct ContentView_Previews: PreviewProvider {
//    static var previews: some View {
//        ContentView(text: "Hello!")
//    }
//}
