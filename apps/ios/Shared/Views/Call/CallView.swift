//
//  CallView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 29/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebKit

struct WebView: UIViewRepresentable {
    @Binding var commandStr: String

    class Coordinator: NSObject, WKNavigationDelegate, WKScriptMessageHandler {
        var webView: WKWebView!


        func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
            webView.allowsBackForwardNavigationGestures = false
            self.webView = webView
        }

        // receive message from wkwebview
        func userContentController(
            _ userContentController: WKUserContentController,
            didReceive message: WKScriptMessage
        ) {
            print(message.body)
//            let date = Date()
//            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
//                self.messageToWebview(msg: "hello, I got your messsage: \(message.body) at \(date)")
//            }
        }

        func messageToWebview(msg: String) {
            self.webView?.evaluateJavaScript("webkit.messageHandlers.bridge.onMessage('\(msg)')")
        }
    }

    func makeCoordinator() -> Coordinator {
        return Coordinator()
    }

    func makeUIView(context: Context) -> WKWebView {
        let coordinator = makeCoordinator()
        let userContentController = WKUserContentController()
        userContentController.add(coordinator, name: "bridge")

        let configuration = WKWebViewConfiguration()
        configuration.userContentController = userContentController
        configuration.mediaTypesRequiringUserActionForPlayback = []
        configuration.allowsInlineMediaPlayback = true

        // Enable us to capture calls to console.log in the xcode logs
        // Print actually happens on line 29
        let source = "console.log = (msg) => webkit.messageHandlers.logHandler.postMessage(msg)"
        let script = WKUserScript(source: source, injectionTime: .atDocumentStart, forMainFrameOnly: false)
        configuration.userContentController.addUserScript(script)
        configuration.userContentController.add(coordinator, name: "logHandler")

        let _wkwebview = WKWebView(frame: .zero, configuration: configuration)
        _wkwebview.navigationDelegate = coordinator
        guard let path: String = Bundle.main.path(forResource: "call", ofType: "html", inDirectory: "www") else {
            print("Page Not Found")
            return _wkwebview
        }
        let localHTMLUrl = URL(fileURLWithPath: path, isDirectory: false)
        _wkwebview.loadFileURL(localHTMLUrl, allowingReadAccessTo: localHTMLUrl)
        return _wkwebview
    }

    func updateUIView(_ webView: WKWebView, context: Context) {
        if commandStr.starts(with: "run ") {
            let cmd = dropPrefix(commandStr, "run ")
            print(cmd)
            let controller = makeCoordinator()
            controller.messageToWebview(msg: cmd)
        }
    }
}



struct CallView: View {
    @State var commandStr = ""
    @FocusState private var keyboardVisible: Bool
    var body: some View {
        VStack(spacing: 30) {
            WebView(commandStr: $commandStr).frame(maxHeight: 260)
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
            }
            HStack(spacing: 20) {
                Button("Capabilities") {
//                    initialize()
                }
                Button("Start") {
//                    initialize()
                }
                Button("Accept") {
//                    saveUserSMPServers()
                }
                Button("Answer") {
//                    saveUserSMPServers()
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
