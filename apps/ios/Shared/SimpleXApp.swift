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

//let machMessenger = MachMessenger(APP_MACH_PORT, callback: receivedMachMessage)
//
//func receivedMachMessage(msgId: Int32, msg: String) -> String? {
//    logger.debug("MachMessenger: receivedMachMessage from FPS")
////    return "reply from App to: \(msg)"
//
//    if let data = msg.data(using: .utf8) {
//        logger.debug("receivedMachMessage has data")
//        let endpoint = try! NSKeyedUnarchiver.unarchivedObject(ofClass: NSXPCListenerEndpoint.self, from: data)!
//        logger.debug("receivedMachMessage has endpoint")
//        let connection = NSXPCConnection(listenerEndpoint: endpoint)
//        logger.debug("receivedMachMessage has connection")
//        connection.remoteObjectInterface = NSXPCInterface(with: SimpleXFPServiceProtocol.self)
//
//        // Start the connection.
//        connection.resume()
//
//        // Get the proxy object.
//        let rawProxy = connection.remoteObjectProxyWithErrorHandler({ (errorAccessingRemoteObject) in
//            // Handle the error here...
//        })
//
//        // Cast the proxy object to the interface's protocol.
//        guard let proxy = rawProxy as? SimpleXFPServiceProtocol else {
//            // If the interface is set up properly, this should never fail.
//            fatalError("*** Unable to cast \(rawProxy) to a DesiredProtocol instance ***")
//        }
//
//        logger.debug("receivedMachMessage calling service")
//        proxy.upperCaseString("hello to service", withReply: { reply in
//            logger.debug("receivedMachMessage reply from service \(reply)")
//        })
//
//    }
//    return nil
//}

@main
struct SimpleXApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
    @StateObject private var chatModel = ChatModel.shared
    @ObservedObject var alertManager = AlertManager.shared
    @Environment(\.scenePhase) var scenePhase
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var userAuthorized: Bool?
    @State private var doAuthenticate = false
    @State private var enteredBackground: Double? = nil

    init() {
        hs_init(0, nil)
        UserDefaults.standard.register(defaults: appDefaults)
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
//        machMessenger.start()

        // test service comms
        DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
            testFPService()
        }
    }

    var body: some Scene {
        return WindowGroup {
            ContentView(doAuthenticate: $doAuthenticate, userAuthorized: $userAuthorized)
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
//                    let res = machMessenger.sendMessageWithReply(NSE_MACH_PORT, msg: "App scenePhase changed to \(String(describing: scenePhase))")
//                    logger.debug("MachMessenger \(String(describing: res), privacy: .public)")
                    setAppState(phase)
                    switch (phase) {
                    case .background:
                        BGManager.shared.schedule()
                        if userAuthorized == true {
                            enteredBackground = ProcessInfo.processInfo.systemUptime
                        }
                        doAuthenticate = false
//                        machMessenger.stop()
                    case .active:
                        doAuthenticate = authenticationExpired()
//                        machMessenger.start()
                    default:
                        break
                    }
                }
        }
    }

    private func authenticationExpired() -> Bool {
        if let enteredBackground = enteredBackground {
            return ProcessInfo.processInfo.systemUptime - enteredBackground >= 30
        } else {
            return true
        }
    }
}
