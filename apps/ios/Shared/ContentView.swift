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

    var contentAccessAuthenticationExtended: Bool

    @Environment(\.scenePhase) var scenePhase
    @State private var automaticAuthenticationAttempted = false
    @State private var canConnectViewCall = false
    @State private var lastSuccessfulUnlock: TimeInterval? = nil

    @AppStorage(DEFAULT_SHOW_LA_NOTICE) private var prefShowLANotice = false
    @AppStorage(DEFAULT_LA_NOTICE_SHOWN) private var prefLANoticeShown = false
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @AppStorage(DEFAULT_PRIVACY_PROTECT_SCREEN) private var protectScreen = false
    @AppStorage(DEFAULT_NOTIFICATION_ALERT_SHOWN) private var notificationAlertShown = false
    @State private var showSettings = false
    @State private var showWhatsNew = false
    @State private var showChooseLAMode = false
    @State private var showSetPasscode = false
    @State private var chatListActionSheet: ChatListActionSheet? = nil

    private enum ChatListActionSheet: Identifiable {
        case planAndConnectSheet(sheet: PlanAndConnectActionSheet)

        var id: String {
            switch self {
            case let .planAndConnectSheet(sheet): return sheet.id
            }
        }
    }

    private var accessAuthenticated: Bool {
        chatModel.contentViewAccessAuthenticated || contentAccessAuthenticationExtended
    }

    var body: some View {
        ZStack {
            // contentView() has to be in a single branch, so that enabling authentication doesn't trigger re-rendering and close settings.
            // i.e. with separate branches like this settings are closed: `if prefPerformLA { ... contentView() ... } else { contentView() }
            if !prefPerformLA || accessAuthenticated {
                contentView()
            } else {
                lockButton()
            }
            if chatModel.showCallView, let call = chatModel.activeCall {
                callView(call)
            }
            if !showSettings, let la = chatModel.laRequest {
                LocalAuthView(authRequest: la)
            } else if showSetPasscode {
                SetAppPasscodeView {
                    chatModel.contentViewAccessAuthenticated = true
                    prefPerformLA = true
                    showSetPasscode = false
                    privacyLocalAuthModeDefault.set(.passcode)
                    alertManager.showAlert(laTurnedOnAlert())
                } cancel: {
                    prefPerformLA = false
                    showSetPasscode = false
                    alertManager.showAlert(laPasscodeNotSetAlert())
                }
            }
            if chatModel.chatDbStatus == nil {
                initializationView()
            }
        }
        .alert(isPresented: $alertManager.presentAlert) { alertManager.alertView! }
        .sheet(isPresented: $showSettings) {
            SettingsView(showSettings: $showSettings)
        }
        .confirmationDialog("SimpleX Lock mode", isPresented: $showChooseLAMode, titleVisibility: .visible) {
            Button("System authentication") { initialEnableLA() }
            Button("Passcode entry") { showSetPasscode = true }
        }
        .onChange(of: scenePhase) { phase in
            logger.debug("scenePhase was \(String(describing: scenePhase)), now \(String(describing: phase))")
            switch (phase) {
            case .background:
                // also see .onChange(of: scenePhase) in SimpleXApp: on entering background
                // it remembers enteredBackgroundAuthenticated and sets chatModel.contentViewAccessAuthenticated to false
                automaticAuthenticationAttempted = false
                canConnectViewCall = false
            case .active:
                canConnectViewCall = !prefPerformLA || contentAccessAuthenticationExtended || unlockedRecently()
                
                // condition `!chatModel.contentViewAccessAuthenticated` is required for when authentication is enabled in settings or on initial notice
                if prefPerformLA && !chatModel.contentViewAccessAuthenticated {
                    if AppChatState.shared.value != .stopped {
                        if contentAccessAuthenticationExtended {
                            chatModel.contentViewAccessAuthenticated = true
                        } else {
                            if !automaticAuthenticationAttempted {
                                automaticAuthenticationAttempted = true
                                // authenticate if call kit call is not in progress
                                if !(CallController.useCallKit() && chatModel.showCallView && chatModel.activeCall != nil) {
                                    authenticateContentViewAccess()
                                }
                            }
                        }
                    } else {
                        // when app is stopped automatic authentication is not attempted
                        chatModel.contentViewAccessAuthenticated = contentAccessAuthenticationExtended
                    }
                }
            default:
                break
            }
        }
    }

    @ViewBuilder private func contentView() -> some View {
        if let status = chatModel.chatDbStatus, status != .ok {
            DatabaseErrorView(status: status)
        } else if !chatModel.v3DBMigration.startChat {
            MigrateToAppGroupView()
        } else if let step = chatModel.onboardingStage {
            if case .onboardingComplete = step,
               chatModel.currentUser != nil {
                mainView()
                .actionSheet(item: $chatListActionSheet) { sheet in
                    switch sheet {
                    case let .planAndConnectSheet(sheet): return planAndConnectActionSheet(sheet, dismiss: false)
                    }
                }
            } else {
                OnboardingView(onboarding: step)
            }
        }
    }

    @ViewBuilder private func callView(_ call: Call) -> some View {
        if CallController.useCallKit() {
            ActiveCallView(call: call, canConnectCall: Binding.constant(true))
                .onDisappear {
                    if prefPerformLA && !accessAuthenticated { authenticateContentViewAccess() }
                }
        } else {
            ActiveCallView(call: call, canConnectCall: $canConnectViewCall)
            if prefPerformLA && !accessAuthenticated {
                Rectangle()
                    .fill(colorScheme == .dark ? .black : .white)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
                lockButton()
            }
        }
    }

    private func lockButton() -> some View {
        Button(action: authenticateContentViewAccess) { Label("Unlock", systemImage: "lock") }
    }

    private func initializationView() -> some View {
        VStack {
            ProgressView().scaleEffect(2)
            Text("Opening app…")
                .padding()
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity )
        .background(
            Rectangle()
                .fill(.background)
        ) 
    }

    private func mainView() -> some View {
        ZStack(alignment: .top) {
            ChatListView(showSettings: $showSettings).privacySensitive(protectScreen)
            .onAppear {
                requestNtfAuthorization()
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
                connectViaUrl()
            }
            .onChange(of: chatModel.appOpenUrl) { _ in connectViaUrl() }
            .sheet(isPresented: $showWhatsNew) {
                WhatsNewView()
            }
            if chatModel.setDeliveryReceipts {
                SetDeliveryReceiptsView()
            }
            IncomingCallView()
        }
        .onContinueUserActivity("INStartCallIntent", perform: processUserActivity)
        .onContinueUserActivity("INStartAudioCallIntent", perform: processUserActivity)
        .onContinueUserActivity("INStartVideoCallIntent", perform: processUserActivity)
    }

    private func processUserActivity(_ activity: NSUserActivity) {
        let intent = activity.interaction?.intent
        if let intent = intent as? INStartCallIntent {
            callToRecentContact(intent.contacts, intent.callCapability == .videoCall ? .video : .audio)
        } else if let intent = intent as? INStartAudioCallIntent {
            callToRecentContact(intent.contacts, .audio)
        } else if let intent = intent as? INStartVideoCallIntent {
            callToRecentContact(intent.contacts, .video)
        }
    }

    private func callToRecentContact(_ contacts: [INPerson]?, _ mediaType: CallMediaType) {
        logger.debug("callToRecentContact")
        if let contactId = contacts?.first?.personHandle?.value,
           let chat = chatModel.getChat(contactId),
           case let .direct(contact) = chat.chatInfo {
            logger.debug("callToRecentContact: schedule call")
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                CallController.shared.startCall(contact, mediaType)
            }
        }
    }

    private func unlockedRecently() -> Bool {
        if let lastSuccessfulUnlock = lastSuccessfulUnlock {
            return ProcessInfo.processInfo.systemUptime - lastSuccessfulUnlock < 2
        } else {
            return false
        }
    }

    private func authenticateContentViewAccess() {
        logger.debug("DEBUGGING: authenticateContentViewAccess")
        dismissAllSheets(animated: false) {
            logger.debug("DEBUGGING: authenticateContentViewAccess, in dismissAllSheets callback")
            chatModel.chatId = nil

            authenticate(reason: NSLocalizedString("Unlock app", comment: "authentication reason"), selfDestruct: true) { laResult in
                logger.debug("DEBUGGING: authenticate callback: \(String(describing: laResult))")
                switch (laResult) {
                case .success:
                    chatModel.contentViewAccessAuthenticated = true
                    canConnectViewCall = true
                    lastSuccessfulUnlock = ProcessInfo.processInfo.systemUptime
                case .failed:
                    chatModel.contentViewAccessAuthenticated = false
                    if privacyLocalAuthModeDefault.get() == .passcode {
                        AlertManager.shared.showAlert(laFailedAlert())
                    }
                case .unavailable:
                    prefPerformLA = false
                    canConnectViewCall = true
                    AlertManager.shared.showAlert(laUnavailableTurningOffAlert())
                }
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
            primaryButton: .default(Text("Turn on")) { showChooseLAMode = true },
            secondaryButton: .cancel()
         )
    }

    private func initialEnableLA () {
        privacyLocalAuthModeDefault.set(.system)
        authenticate(reason: NSLocalizedString("Enable SimpleX Lock", comment: "authentication reason")) { laResult in
            switch laResult {
            case .success:
                chatModel.contentViewAccessAuthenticated = true
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

    func connectViaUrl() {
        dismissAllSheets() {
            let m = ChatModel.shared
            if let url = m.appOpenUrl {
                m.appOpenUrl = nil
                var path = url.path
                if (path == "/contact" || path == "/invitation") {
                    path.removeFirst()
                    let link = url.absoluteString.replacingOccurrences(of: "///\(path)", with: "/\(path)")
                    planAndConnect(
                        link,
                        showAlert: showPlanAndConnectAlert,
                        showActionSheet: { chatListActionSheet = .planAndConnectSheet(sheet: $0) },
                        dismiss: false,
                        incognito: nil
                    )
                } else {
                    AlertManager.shared.showAlert(Alert(title: Text("Error: URL is invalid")))
                }
            }
        }
    }

    private func showPlanAndConnectAlert(_ alert: PlanAndConnectAlert) {
        AlertManager.shared.showAlert(planAndConnectAlert(alert, dismiss: false))
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
