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
    @State private var showNotificationAlert = false

    var body: some View {
        if let user = chatModel.currentUser {
            ChatListView(user: user)
                .onAppear {
                    do {
                        try apiStartChat()
                        chatModel.chats = try apiGetChats()
                    } catch {
                        fatalError("Failed to start or load chats: \(error)")
                    }
                    ChatReceiver.shared.start()
                    NtfManager.shared.requestAuthorization(onDeny: {
                        alertManager.showAlert(notificationAlert())
                    })
                }
                .alert(isPresented: $alertManager.presentAlert) { alertManager.alertView! }
        } else {
            WelcomeView()
        }
    }

    func notificationAlert() -> Alert {
        Alert(
            title: Text("Notification are disabled!"),
             message: Text("Please open settings to enable"),
             primaryButton: .default(Text("Open Settings")) {
                 DispatchQueue.main.async {
                     UIApplication.shared.open(URL(string: UIApplication.openSettingsURLString)!, options: [:], completionHandler: nil)
                 }
             },
             secondaryButton: .cancel()
         )
    }
}

final class AlertManager: ObservableObject {
    static let shared = AlertManager()
    @Published var presentAlert = false
    @Published var alertView: Alert?

    func showAlert(_ alert: Alert) {
        logger.debug("AlertManager.showAlert")
        DispatchQueue.main.async {
            self.alertView = alert
            self.presentAlert = true
        }
    }

    func showAlertMsg(title: String, message: String? = nil) {
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
