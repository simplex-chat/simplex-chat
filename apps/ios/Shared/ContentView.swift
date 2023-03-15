//
//  ContentView.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI
import Intents
import SimpleXChat

struct ContentView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var alertManager = AlertManager.shared
    @ObservedObject var callController = CallController.shared
    @Environment(\.colorScheme) var colorScheme
    @Binding var doAuthenticate: Bool
    @Binding var userAuthorized: Bool?
    @Binding var canConnectCall: Bool
    @Binding var lastSuccessfulUnlock: TimeInterval?
    @AppStorage(DEFAULT_SHOW_LA_NOTICE) private var prefShowLANotice = false
    @AppStorage(DEFAULT_LA_NOTICE_SHOWN) private var prefLANoticeShown = false
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @AppStorage(DEFAULT_PRIVACY_PROTECT_SCREEN) private var protectScreen = false
    @AppStorage(DEFAULT_NOTIFICATION_ALERT_SHOWN) private var notificationAlertShown = false
    @State private var showWhatsNew = false

    var body: some View {
        ZStack {
            if !CallController.useCallKit() && chatModel.showCallView, let call = chatModel.activeCall {
                ActiveCallView(call: call, canConnectCall: $canConnectCall)
            }
            if CallController.useCallKit() && chatModel.showCallView, let call = chatModel.activeCall {
                ActiveCallView(call: call, canConnectCall: Binding.constant(true))
                .onDisappear { if userAuthorized == false && doAuthenticate { runAuthenticate() } }
            } else if prefPerformLA && userAuthorized != true {
                if !CallController.useCallKit() {
                    Rectangle().fill(colorScheme == .dark ? .black : .white)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
                    .onTapGesture(perform: {})
                }
                Button(action: runAuthenticate) { Label("Unlock", systemImage: "lock") }
            } else if let status = chatModel.chatDbStatus, status != .ok {
                DatabaseErrorView(status: status)
            } else if !chatModel.v3DBMigration.startChat {
                MigrateToAppGroupView()
            } else if let step = chatModel.onboardingStage, (!CallController.useCallKit() && (!chatModel.showCallView || chatModel.activeCall == nil)) {
                if case .onboardingComplete = step,
                   chatModel.currentUser != nil {
                    mainView()
                } else {
                    OnboardingView(onboarding: step)
                }
            }
        }
        .onAppear {
            if prefPerformLA { requestNtfAuthorization() }
            if CallController.useCallKit() && chatModel.showCallView && chatModel.activeCall != nil {
                userAuthorized = false
            } else if doAuthenticate { runAuthenticate() }
        }
        .onChange(of: doAuthenticate) { _ in
            if CallController.useCallKit() && chatModel.showCallView && chatModel.activeCall != nil {
                userAuthorized = false
            } else if doAuthenticate { runAuthenticate() }
        }
        .alert(isPresented: $alertManager.presentAlert) { alertManager.alertView! }
    }

    private func mainView() -> some View {
        ZStack(alignment: .top) {
            ChatListView().privacySensitive(protectScreen)
            .onAppear {
                if !prefPerformLA { requestNtfAuthorization() }
                // Local Authentication notice is to be shown on next start after onboarding is complete
                if (!prefLANoticeShown && prefShowLANotice && !chatModel.chats.isEmpty) {
                    prefLANoticeShown = true
                    alertManager.showAlert(laNoticeAlert())
                } else if !chatModel.showCallView && CallController.shared.activeCallInvitation == nil {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                        if !showWhatsNew {
                            showWhatsNew = shouldShowWhatsNew()
                        }
                    }
                }
                prefShowLANotice = true
            }
            .sheet(isPresented: $showWhatsNew) {
                WhatsNewView()
            }
            IncomingCallView()
        }
//        .onContinueUserActivity("INStartCallIntent", perform: processUserActivity)
//        .onContinueUserActivity("INStartAudioCallIntent", perform: processUserActivity)
//        .onContinueUserActivity("INStartVideoCallIntent", perform: processUserActivity)
    }

//    private func processUserActivity(_ activity: NSUserActivity) {
//        let callToContact = { (contactId: ChatId?, mediaType: CallMediaType) in
//            if let chatInfo = chatModel.chats.first(where: { $0.id == contactId })?.chatInfo,
//                case let .direct(contact) = chatInfo {
//                CallController.shared.startCall(contact, mediaType)
//            }
//        }
//        if let intent = activity.interaction?.intent as? INStartCallIntent {
//            callToContact(intent.contacts?.first?.personHandle?.value, .audio)
//        } else if let intent = activity.interaction?.intent as? INStartAudioCallIntent {
//            callToContact(intent.contacts?.first?.personHandle?.value, .audio)
//        } else if let intent = activity.interaction?.intent as? INStartVideoCallIntent {
//            callToContact(intent.contacts?.first?.personHandle?.value, .video)
//        }
//    }

    private func runAuthenticate() {
        if !prefPerformLA {
            userAuthorized = true
        } else {
            dismissAllSheets(animated: false) {
                chatModel.chatId = nil
                justAuthenticate()
            }
        }
    }

    private func justAuthenticate() {
        userAuthorized = false
        authenticate(reason: NSLocalizedString("Unlock", comment: "authentication reason")) { laResult in
            switch (laResult) {
            case .success:
                userAuthorized = true
                canConnectCall = true
                lastSuccessfulUnlock = ProcessInfo.processInfo.systemUptime
            case .failed:
                break
            case .unavailable:
                userAuthorized = true
                prefPerformLA = false
                canConnectCall = true
                AlertManager.shared.showAlert(laUnavailableTurningOffAlert())
            }
        }
    }

    func requestNtfAuthorization() {
        NtfManager.shared.requestAuthorization(
            onDeny: {
                if (!notificationAlertShown) {
                    notificationAlertShown = true
                    alertManager.showAlert(notificationAlert())
                }
            },
            onAuthorized: { notificationAlertShown = false }
        )
    }

    func laNoticeAlert() -> Alert {
        Alert(
            title: Text("SimpleX Lock"),
            message: Text("To protect your information, turn on SimpleX Lock.\nYou will be prompted to complete authentication before this feature is enabled."),
            primaryButton: .default(Text("Turn on")) {
                authenticate(reason: NSLocalizedString("Enable SimpleX Lock", comment: "authentication reason")) { laResult in
                    switch laResult {
                    case .success:
                        prefPerformLA = true
                        alertManager.showAlert(laTurnedOnAlert())
                    case .failed:
                        prefPerformLA = false
                        alertManager.showAlert(laFailedAlert())
                    case .unavailable:
                        prefPerformLA = false
                        alertManager.showAlert(laUnavailableInstructionAlert())
                    }
                }
            },
            secondaryButton: .cancel()
         )
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
