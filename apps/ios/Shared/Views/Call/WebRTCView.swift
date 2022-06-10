//
//  WebRTCView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 29/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebKit
import SimpleXChatSDK

class WebRTCCoordinator: NSObject, WKNavigationDelegate, WKScriptMessageHandler, WKUIDelegate {
    var rtcWebView: Binding<WKWebView?>
    var webViewMsg: Binding<WVAPIMessage?>

    internal init(rtcWebView: Binding<WKWebView?>, webViewMsg: Binding<WVAPIMessage?>) {
        self.rtcWebView = rtcWebView
        self.webViewMsg = webViewMsg
    }

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        webView.allowsBackForwardNavigationGestures = false
        self.rtcWebView.wrappedValue = webView
        ChatModel.shared.callWebView = webView
    }

    func webView(_ webView: WKWebView, decideMediaCapturePermissionsFor origin : WKSecurityOrigin, initiatedBy frame: WKFrameInfo, type: WKMediaCaptureType) async -> WKPermissionDecision {
        print("webView", #function)
        return .grant
    }

    // receive message from WKWebView
    func userContentController(
        _ userContentController: WKUserContentController,
        didReceive message: WKScriptMessage
    ) {
        logger.debug("WebRTCCoordinator.userContentController")
        switch message.name {
        case "webrtc":
            if let msgStr = message.body as? String,
               let msg: WVAPIMessage = decodeJSON(msgStr) {
                // this is the binding that communicates messages from webview to swift view
                webViewMsg.wrappedValue = msg
                if case .invalid = msg.resp {
                    logger.error("WebRTCCoordinator.userContentController: invalid message \(String(describing: message.body))")
                }
            } else {
                logger.error("WebRTCCoordinator.userContentController: message parsing error \(String(describing: message.body))")
            }
        case "logger":
            if let msgStr = message.body as? String {
                logger.error("WebRTCCoordinator console.log: \(msgStr)")
            } else {
                logger.error("WebRTCCoordinator console.log: \(String(describing: message.body))")
            }
        default:
            logger.error("WebRTCCoordinator.userContentController: invalid message.name \(message.name)")
        }
    }
}

struct WebRTCView: UIViewRepresentable {
    @State private var coordinator: WebRTCCoordinator?
    @Binding var rtcWebView: WKWebView?
    @Binding var webViewMsg: WVAPIMessage?

    func makeCoordinator() -> WebRTCCoordinator {
        WebRTCCoordinator(rtcWebView: $rtcWebView, webViewMsg: $webViewMsg)
    }

    func makeUIView(context: Context) -> WKWebView {
        let wkCoordinator = makeCoordinator()
        DispatchQueue.main.async { coordinator = wkCoordinator }

        let wkController = WKUserContentController()

        let cfg = WKWebViewConfiguration()
        cfg.userContentController = wkController
        cfg.mediaTypesRequiringUserActionForPlayback = []
        cfg.allowsInlineMediaPlayback = true

        let addScript = { (handler: String, source: String) in
            let script = WKUserScript(source: source, injectionTime: .atDocumentEnd, forMainFrameOnly: false)
            wkController.addUserScript(script)
            wkController.add(wkCoordinator, name: handler)
        }

        addScript("webrtc", "sendMessageToNative = (msg) => webkit.messageHandlers.webrtc.postMessage(JSON.stringify(msg))")
        addScript("logger", "console.log = (arg) => webkit.messageHandlers.logger.postMessage(JSON.stringify(arg))")

        let wkWebView = WKWebView(frame: .zero, configuration: cfg)
        wkWebView.navigationDelegate = wkCoordinator
        guard let path: String = Bundle.main.path(forResource: "call", ofType: "html", inDirectory: "www") else {
            logger.error("WebRTCView.makeUIView call.html not found")
            return wkWebView
        }
        let localHTMLUrl = URL(fileURLWithPath: path, isDirectory: false)
        wkWebView.loadFileURL(localHTMLUrl, allowingReadAccessTo: localHTMLUrl)
        return wkWebView
    }

    func updateUIView(_ webView: WKWebView, context: Context) {
        logger.debug("WebRTCView.updateUIView")
    }
}

func sendCallCommand(_ webView: WKWebView, _ command: WCallCommand) {
    logger.debug("sendCallCommand: \(command.cmdType)")
    let apiCmd = encodeJSON(WVAPICall(command: command))
    let js = "processCommand(\(apiCmd))"
    webView.evaluateJavaScript(js)
}

//struct CallViewDebug: View {
//    @State private var commandStr = ""
//    @State private var rtcWebView: WKWebView? = nil
//    @State private var webViewMsg: WVAPIMessage? = nil
//    @FocusState private var keyboardVisible: Bool
//
//    var body: some View {
//        VStack(spacing: 30) {
//            WebRTCView(rtcWebView: $rtcWebView, webViewMsg: $webViewMsg).frame(maxHeight: 260)
//                .onChange(of: webViewMsg) { _ in
//                    if let resp = webViewMsg {
//                        commandStr = encodeJSON(resp)
//                    }
//                }
//            TextEditor(text: $commandStr)
//                .focused($keyboardVisible)
//                .disableAutocorrection(true)
//                .textInputAutocapitalization(.never)
//                .padding(.horizontal, 5)
//                .padding(.top, 2)
//                .frame(height: 112)
//                .overlay(
//                    RoundedRectangle(cornerRadius: 10)
//                        .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
//                )
//            HStack(spacing: 20) {
//                Button("Copy") {
//                    UIPasteboard.general.string = commandStr
//                }
//                Button("Paste") {
//                    commandStr = UIPasteboard.general.string ?? ""
//                }
//                Button("Clear") {
//                    commandStr = ""
//                }
//                Button("Send") {
//                    if let wv = rtcWebView,
//                       let command: WCallCommand = decodeJSON(commandStr) {
//                        sendCallCommand(wv, command)
//                    }
//                }
//            }
//            HStack(spacing: 20) {
//                Button("Capabilities") {
//                    if let wv = rtcWebView {
//                        sendCallCommand(wv, .capabilities(useWorker: true))
//                    }
//                }
//                Button("Start") {
//                    if let wv = rtcWebView {
//                        sendCallCommand(wv, .start(media: .video))
//                    }
//                }
//                Button("Accept") {
//
//                }
//                Button("Answer") {
//
//                }
//                Button("ICE") {
//
//                }
//                Button("End") {
//
//                }
//            }
//        }
//    }
//}
