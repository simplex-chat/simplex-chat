//
//  WebRTCView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 29/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebKit

class WebRTCCoordinator: NSObject, WKNavigationDelegate, WKScriptMessageHandler {
    var webViewMsg: Binding<WCallResponse?>
    var webView: WKWebView?
//    var corrId = 0
//    var pendingCommands: Dictionary<Int, CheckedContinuation<WCallResponse, Never>> = [:]

    internal init(webViewMsg: Binding<WCallResponse?>) {
        self.webViewMsg = webViewMsg
    }

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        webView.allowsBackForwardNavigationGestures = false
        self.webView = webView
    }

    // receive message from WKWebView
    func userContentController(
        _ userContentController: WKUserContentController,
        didReceive message: WKScriptMessage
    ) {
        logger.debug("WebRTCCoordinator.userContentController")
        if let data = (message.body as? String)?.data(using: .utf8),
           let msg = try? jsonDecoder.decode(WVAPIMessage.self, from: data) {
//            if let corrId = msg.corrId, let cont = pendingCommands.removeValue(forKey: corrId) {
//                cont.resume(returning: msg.resp)
//            } else {
            webViewMsg.wrappedValue = msg.resp
                // TODO pass messages to call view via binding
                // print(msg.resp)
//            }
        } else {
            logger.error("WebRTCCoordinator.userContentController: invalid message \(String(describing: message.body))")
        }
    }

    func messageToWebview(msg: String) {
        if let webView = webView {
            logger.debug("WebRTCCoordinator.messageToWebview")
            webView.evaluateJavaScript("webkit.messageHandlers.logHandler.postMessage('\(msg)')")
        }
    }

    func sendCommand(command: WCallCommand) {
        if let webView = webView {
            logger.debug("WebRTCCoordinator.sendCommand")
            let apiCmd = encodeJSON(WVAPICall(command: command))
            let js = "processCommand(\(apiCmd))"
            webView.evaluateJavaScript(js)
        }
    }

//    func processCommand(command: WCallCommand) async -> WCallResponse {
//        await withCheckedContinuation { cont in
//            if let webView = webView {
//                logger.debug("WebRTCCoordinator.processCommand")
//                let corrId_ = corrId
//                corrId = corrId + 1
//                pendingCommands[corrId_] = cont
//                let apiCmd = encodeJSON(WVAPICall(corrId: corrId_, command: command))
//                DispatchQueue.main.async {
//                    logger.debug("WebRTCCoordinator.processCommand DispatchQueue.main.async")
//                    let js = "processCommand(\(apiCmd))"
//                    webView.evaluateJavaScript(js)
//                }
//            }
//        }
//    }
}

struct WebRTCView: UIViewRepresentable {
    @Binding var coordinator: WebRTCCoordinator?
    @Binding var webViewMsg: WCallResponse?

    func makeCoordinator() -> WebRTCCoordinator {
        WebRTCCoordinator(webViewMsg: $webViewMsg)
    }

    func makeUIView(context: Context) -> WKWebView {
        let _coordinator = makeCoordinator()
        DispatchQueue.main.async {
            coordinator = _coordinator
        }

        let userContentController = WKUserContentController()

        let cfg = WKWebViewConfiguration()
        cfg.userContentController = userContentController
        cfg.mediaTypesRequiringUserActionForPlayback = []
        cfg.allowsInlineMediaPlayback = true

        // Enable us to capture calls to console.log in the xcode logs
        let source = "console.log = (msg) => webkit.messageHandlers.logHandler.postMessage(msg)"
        let script = WKUserScript(source: source, injectionTime: .atDocumentStart, forMainFrameOnly: false)
        cfg.userContentController.addUserScript(script)
        cfg.userContentController.add(_coordinator, name: "logHandler")

        let _wkwebview = WKWebView(frame: .zero, configuration: cfg)
        _wkwebview.navigationDelegate = _coordinator
        guard let path: String = Bundle.main.path(forResource: "call", ofType: "html", inDirectory: "www") else {
            logger.error("WebRTCView.makeUIView call.html not found")
            return _wkwebview
        }
        let localHTMLUrl = URL(fileURLWithPath: path, isDirectory: false)
        _wkwebview.loadFileURL(localHTMLUrl, allowingReadAccessTo: localHTMLUrl)
        return _wkwebview
    }

    func updateUIView(_ webView: WKWebView, context: Context) {
        logger.debug("WebRTCView.updateUIView")
    }
}



struct CallViewDebug: View {
    @State var coordinator: WebRTCCoordinator? = nil
    @State var commandStr = ""
    @State private var webViewMsg: WCallResponse? = nil
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        VStack(spacing: 30) {
            WebRTCView(coordinator: $coordinator, webViewMsg: $webViewMsg).frame(maxHeight: 260)
                .onChange(of: webViewMsg) { _ in
                    if let resp = webViewMsg {
                        commandStr = encodeJSON(resp)
                    }
                }
            TextEditor(text: $commandStr)
                .focused($keyboardVisible)
                .disableAutocorrection(true)
                .textInputAutocapitalization(.never)
                .padding(.horizontal, 5)
                .padding(.top, 2)
                .frame(height: 112)
                .overlay(
                    RoundedRectangle(cornerRadius: 10)
                        .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                )
            HStack(spacing: 20) {
                Button("Copy") {
                    UIPasteboard.general.string = commandStr
                }
                Button("Paste") {
                    commandStr = UIPasteboard.general.string ?? ""
                }
                Button("Clear") {
                    commandStr = ""
                }
                Button("Send") {
                    do {
                        let command = try jsonDecoder.decode(WCallCommand.self, from: commandStr.data(using: .utf8)!)
                        if let c = coordinator {
                            c.sendCommand(command: command)
                        }
                    } catch {
                        print(error)
                    }
                }
            }
            HStack(spacing: 20) {
                Button("Capabilities") {

                }
                Button("Start") {
                    if let c = coordinator {
                        c.sendCommand(command: .start(media: .video))
                    }
                }
                Button("Accept") {

                }
                Button("Answer") {

                }
                Button("ICE") {

                }
                Button("End") {

                }
            }
        }
    }
}

struct CallViewDebug_Previews: PreviewProvider {
    static var previews: some View {
        CallViewDebug()
    }
}
