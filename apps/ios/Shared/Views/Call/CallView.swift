//
//  CallView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 29/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

import SwiftUI
import WebKit

struct WebView: UIViewRepresentable {
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
        configuration.userContentController.add(makeCoordinator(), name: "logHandler")

        let _wkwebview = WKWebView(frame: .zero, configuration: configuration)
        _wkwebview.navigationDelegate = coordinator

        return _wkwebview
    }

    func updateUIView(_ webView: WKWebView, context: Context) {
        guard let path: String = Bundle.main.path(forResource: "call", ofType: "html", inDirectory: "www") else {
            print("page not found")
            return
        }
        let localHTMLUrl = URL(fileURLWithPath: path, isDirectory: false)
        webView.loadFileURL(localHTMLUrl, allowingReadAccessTo: localHTMLUrl)
    }
}



struct CallView: View {
    var body: some View {
        VStack {
            WebView()
        }
    }
}




struct CallView_Previews: PreviewProvider {
    static var previews: some View {
        CallView()
    }
}
