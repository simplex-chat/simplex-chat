//
//  ContentView.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI
import Intents
import SimpleXChat

private enum NoticesSheet: Identifiable {
    case whatsNew(updatedConditions: Bool)

    var id: String {
        switch self {
        case .whatsNew: return "whatsNew"
        }
    }
}

struct ContentView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var alertManager = AlertManager.shared
    @ObservedObject var callController = CallController.shared
    @ObservedObject var appSheetState = AppSheetState.shared
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var sceneDelegate: SceneDelegate

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
    @State private var noticesShown = false
    @State private var noticesSheetItem: NoticesSheet? = nil
    @State private var showChooseLAMode = false
    @State private var showSetPasscode = false
    @State private var waitingForOrPassedAuth = true
    @State private var chatListActionSheet: ChatListActionSheet? = nil
    @State private var chatListUserPickerSheet: UserPickerSheet? = nil

    private let callTopPadding: CGFloat = 40

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
        if #available(iOS 16.0, *) {
            allViews()
                .scrollContentBackground(.hidden)
        } else {
            // on iOS 15 scroll view background disabled in SceneDelegate
            allViews()
        }
    }

    func allViews() -> some View {
        ZStack {
            let showCallArea = chatModel.activeCall != nil && chatModel.activeCall?.callState != .waitCapabilities && chatModel.activeCall?.callState != .invitationAccepted
            // contentView() has to be in a single branch, so that enabling authentication doesn't trigger re-rendering and close settings.
            // i.e. with separate branches like this settings are closed: `if prefPerformLA { ... contentView() ... } else { contentView() }
            if !prefPerformLA || accessAuthenticated {
                contentView()
                    .padding(.top, showCallArea ? callTopPadding : 0)
            } else {
                lockButton()
                    .padding(.top, showCallArea ? callTopPadding : 0)
            }

            if showCallArea, let call = chatModel.activeCall {
                VStack {
                    activeCallInteractiveArea(call)
                    Spacer()
                }
            }

            if chatModel.showCallView, let call = chatModel.activeCall {
                callView(call)
            }

            if chatListUserPickerSheet == nil, let la = chatModel.laRequest {
                LocalAuthView(authRequest: la)
                    .onDisappear {
                        // this flag is separate from accessAuthenticated to show initializationView while we wait for authentication
                        waitingForOrPassedAuth = accessAuthenticated
                    }
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
            } else if chatModel.chatDbStatus == nil && AppChatState.shared.value != .stopped && waitingForOrPassedAuth {
                initializationView()
            }
        }
        .alert(isPresented: $alertManager.presentAlert) { alertManager.alertView! }
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
        .onAppear {
            reactOnDarkThemeChanges(systemInDarkThemeCurrently)
        }
        .onChange(of: colorScheme) { scheme in
            // It's needed to update UI colors when iOS wants to make screenshot after going to background,
            // so when a user changes his global theme from dark to light or back, the app will adapt to it
            reactOnDarkThemeChanges(scheme == .dark)
        }
        .onChange(of: theme.name) { _ in
            ThemeManager.adjustWindowStyle()
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

    private func activeCallInteractiveArea(_ call: Call) -> some View {
        HStack {
            Text(call.contact.displayName).font(.body).foregroundColor(.white)
            Spacer()
            CallDuration(call: call)
        }
        .padding(.horizontal)
        .frame(height: callTopPadding)
        .background(Color(uiColor: UIColor(red: 47/255, green: 208/255, blue: 88/255, alpha: 1)))
        .onTapGesture {
            chatModel.activeCallViewIsCollapsed = false
        }
    }

    struct CallDuration: View {
        let call: Call
        @State var text: String = ""
        @State var timer: Timer? = nil

        var body: some View {
            Text(text).frame(minWidth: text.count <= 5 ? 52 : 77, alignment: .leading).offset(x: 4).font(.body).foregroundColor(.white)
            .onAppear {
                timer = Timer.scheduledTimer(withTimeInterval: 0.3, repeats: true) { timer in
                    if let connectedAt = call.connectedAt {
                        text = durationText(Int(Date.now.timeIntervalSince1970 - connectedAt.timeIntervalSince1970))
                    }
                }
            }
            .onDisappear {
                _ = timer?.invalidate()
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
                .fill(theme.colors.background)
        )
    }

    private func mainView() -> some View {
        ZStack(alignment: .top) {
            ChatListView(activeUserPickerSheet: $chatListUserPickerSheet)
                .redacted(reason: appSheetState.redactionReasons(protectScreen))
            .onAppear {
                requestNtfAuthorization()
                // Local Authentication notice is to be shown on next start after onboarding is complete
                if (!prefLANoticeShown && prefShowLANotice && chatModel.chats.count > 2) {
                    prefLANoticeShown = true
                    alertManager.showAlert(laNoticeAlert())
                } else if !chatModel.showCallView && CallController.shared.activeCallInvitation == nil {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                        if !noticesShown {
                            let showWhatsNew = shouldShowWhatsNew()
                            let showUpdatedConditions = chatModel.conditions.conditionsAction?.showNotice ?? false
                            noticesShown = showWhatsNew || showUpdatedConditions
                            if showWhatsNew || showUpdatedConditions {
                                noticesSheetItem = .whatsNew(updatedConditions: showUpdatedConditions)
                            }
                        }
                    }
                }
                prefShowLANotice = true
                connectViaUrl()
                showReRegisterTokenAlert()
            }
            .onChange(of: chatModel.appOpenUrl) { _ in connectViaUrl() }
            .onChange(of: chatModel.reRegisterTknStatus) { _ in showReRegisterTokenAlert() }
            .sheet(item: $noticesSheetItem) { item in
                switch item {
                case let .whatsNew(updatedConditions):
                    WhatsNewView(updatedConditions: updatedConditions)
                        .modifier(ThemedBackground())
                        .if(updatedConditions) { v in
                            v.task { await setConditionsNotified_() }
                        }
                }
            }
            if chatModel.setDeliveryReceipts {
                SetDeliveryReceiptsView()
            }
            IncomingCallView()
        }
        .onContinueUserActivity("INStartCallIntent", perform: processUserActivity)
        .onContinueUserActivity("INStartAudioCallIntent", perform: processUserActivity)
        .onContinueUserActivity("INStartVideoCallIntent", perform: processUserActivity)
        .onContinueUserActivity(NSUserActivityTypeBrowsingWeb) { userActivity in
            if let url = userActivity.webpageURL {
                logger.debug("onContinueUserActivity.NSUserActivityTypeBrowsingWeb: \(url)")
                chatModel.appOpenUrl = url
            }
        }
    }

    private func setConditionsNotified_() async {
        do {
            let conditionsId = ChatModel.shared.conditions.currentConditions.conditionsId
            try await setConditionsNotified(conditionsId: conditionsId)
        } catch let error {
            logger.error("setConditionsNotified error: \(responseError(error))")
        }
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
            let activeCall = chatModel.activeCall
            // This line works when a user clicks on a video button in CallKit UI while in call.
            // The app tries to make another call to the same contact and overwite activeCall instance making its state broken
            if let activeCall, contactId == activeCall.contact.id, mediaType == .video, !activeCall.hasVideo {
                Task {
                    await chatModel.callCommand.processCommand(.media(source: .camera, enable: true))
                }
            } else if activeCall == nil {
                logger.debug("callToRecentContact: schedule call")
                DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                    CallController.shared.startCall(contact, mediaType)
                }
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
        let m = ChatModel.shared
        if let url = m.appOpenUrl {
            m.appOpenUrl = nil
            dismissAllSheets() {
                var path = url.path
                if (path == "/contact" || path == "/invitation" || path == "/a" || path == "/c" || path == "/g" || path == "/i") {
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

    func showReRegisterTokenAlert() {
        dismissAllSheets() {
            let m = ChatModel.shared
            if let errorTknStatus = m.reRegisterTknStatus, let token = chatModel.deviceToken {
                chatModel.reRegisterTknStatus = nil
                AlertManager.shared.showAlert(Alert(
                    title: Text("Notifications error"),
                    message: Text(tokenStatusInfo(errorTknStatus, register: true)),
                    primaryButton: .default(Text("Register")) { reRegisterToken(token: token) },
                    secondaryButton: .cancel()
                ))
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
