//
//  SimpleXApp.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI
import OSLog
import SimpleXChat

let logger = Logger()

let machMessenger = MachMessenger(APP_MACH_PORT, callback: receivedNSEMachMessage)

func receivedNSEMachMessage(msgId: Int32, msg: String) -> String? {
    logger.debug("MachMessenger: receivedNSEMachMessage \"\(msg)\" from NSE, replying")
    return "reply from App to: \(msg)"
}

@main
struct SimpleXApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
    @StateObject private var chatModel = ChatModel.shared
    @ObservedObject var alertManager = AlertManager.shared
    @Environment(\.scenePhase) var scenePhase
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var userAuthorized: Bool? = nil
    @State private var doAuthenticate: Bool = false
    @State private var enteredBackground: Double? = nil

    init() {
        hs_init(0, nil)
        UserDefaults.standard.register(defaults: appDefaults)
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
        machMessenger.start()

        // test service comms
        DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
            testFPService()
        }
    }

    var body: some Scene {
        return WindowGroup {
            ContentView(doAuthenticate: $doAuthenticate, enteredBackground: $enteredBackground)
                .environmentObject(chatModel)
                .onOpenURL { url in
                    logger.debug("ContentView.onOpenURL: \(url)")
                    chatModel.appOpenUrl = url
                }
                .onAppear() {
                    initializeChat()
                }
                .onChange(of: scenePhase) { phase in
                    logger.debug("scenePhase \(String(describing: scenePhase))")
                    let res = machMessenger.sendMessageWithReply(NSE_MACH_PORT, msg: "App scenePhase changed to \(String(describing: scenePhase))")
                    logger.debug("MachMessenger \(String(describing: res), privacy: .public)")
                    setAppState(phase)
                    switch (phase) {
                    case .background:
                        BGManager.shared.schedule()
                        doAuthenticate = false
                        enteredBackground = ProcessInfo.processInfo.systemUptime
                        machMessenger.stop()
                    case .active:
                        doAuthenticate = true
                        machMessenger.start()
                    default:
                        break
                    }
                }
        }
    }
}
