//
//  CallView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 29/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebKit

class WebViewCoordinator: NSObject, WKNavigationDelegate, WKScriptMessageHandler {
    var webView: WKWebView!

    var corrId = 0
    var pendingCommands: Dictionary<Int, CheckedContinuation<WError, Never>> = [:]

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        webView.allowsBackForwardNavigationGestures = false
        self.webView = webView
    }

    // receive message from wkwebview
    func userContentController(
        _ userContentController: WKUserContentController,
        didReceive message: WKScriptMessage
    ) {
        logger.debug("WebViewCoordinator.userContentController")
        print(message.body)
        // parse response
        let data = (message.body as! String).data(using: .utf8)!
        let msg = try! jsonDecoder.decode(WebViewMessage.self, from: data)
        if let corrId = msg.corrId, let cont = pendingCommands.removeValue(forKey: corrId) {
            cont.resume(returning: msg.resp)
        } else {
            print(msg.resp)
        }
        

//        cont.resume(returning: resp)
//            let date = Date()
//            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
//                self.messageToWebview(msg: "hello, I got your messsage: \(message.body) at \(date)")
//            }
    }

    func messageToWebview(msg: String) {
        logger.debug("WebViewCoordinator.messageToWebview")
        self.webView.evaluateJavaScript("webkit.messageHandlers.logHandler.postMessage('\(msg)')")
    }

    func processCommand(cmd: WCallCommand) async -> WError {
        await withCheckedContinuation { cont in
            logger.debug("WebViewCoordinator.processCommand")
            let corrId_ = corrId
            corrId = corrId + 1
            pendingCommands[corrId_] = cont
            let apiCmd = WebViewAPICall(corrId: corrId_, command: cmd)
            DispatchQueue.main.async {
                logger.debug("WebViewCoordinator.processCommand DispatchQueue.main.async")
                let apiData = try! jsonEncoder.encode(apiCmd)
                let apiStr = String(decoding: apiData, as: UTF8.self)
                let js = "processCommand(\(apiStr))"
                print(js)
                self.webView.evaluateJavaScript(js)
            }
        }
    }
}

struct WebView: UIViewRepresentable {
    @Binding var coordinator: WebViewCoordinator?

    func makeCoordinator() -> WebViewCoordinator {
        return Coordinator()
    }

    func makeUIView(context: Context) -> WKWebView {
        let _coordinator = makeCoordinator()
        DispatchQueue.main.async {
            coordinator = _coordinator
        }

        let userContentController = WKUserContentController()

        let configuration = WKWebViewConfiguration()
        configuration.userContentController = userContentController
        configuration.mediaTypesRequiringUserActionForPlayback = []
        configuration.allowsInlineMediaPlayback = true

        // Enable us to capture calls to console.log in the xcode logs
        let source = "console.log = (msg) => webkit.messageHandlers.logHandler.postMessage(msg)"
        let script = WKUserScript(source: source, injectionTime: .atDocumentStart, forMainFrameOnly: false)
        configuration.userContentController.addUserScript(script)
        configuration.userContentController.add(_coordinator, name: "logHandler")

        let _wkwebview = WKWebView(frame: .zero, configuration: configuration)
        _wkwebview.navigationDelegate = _coordinator
        guard let path: String = Bundle.main.path(forResource: "call", ofType: "html", inDirectory: "www") else {
            print("Page Not Found")
            return _wkwebview
        }
        let localHTMLUrl = URL(fileURLWithPath: path, isDirectory: false)
        _wkwebview.loadFileURL(localHTMLUrl, allowingReadAccessTo: localHTMLUrl)
        return _wkwebview
    }

    func updateUIView(_ webView: WKWebView, context: Context) {
        print("updating")
    }
}



struct CallView: View {
    @State var coordinator: WebViewCoordinator? = nil
    @State var commandStr = ""
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        VStack(spacing: 30) {
            WebView(coordinator: $coordinator).frame(maxHeight: 260)
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
                    print("in send")
                    if let c = coordinator {
                        print("has coordinator")
                        Task {
                            let resp = await c.processCommand(cmd: .startCall(media: .video))
                            print(resp)
                        }
                    }
                }
            }
            HStack(spacing: 20) {
                Button("Capabilities") {

                }
                Button("Start") {

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




struct CallView_Previews: PreviewProvider {
    static var previews: some View {
        CallView()
    }
}
