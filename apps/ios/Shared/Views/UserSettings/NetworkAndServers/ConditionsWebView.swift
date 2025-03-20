//
//  ConditionsWebView.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 26.11.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebKit

struct ConditionsWebView: UIViewRepresentable {
    @State var html: String
    @EnvironmentObject var theme: AppTheme
    @State var pageLoaded = false

    func makeUIView(context: Context) -> WKWebView {
        let view = WKWebView()
        view.backgroundColor = .clear
        view.isOpaque = false
        view.navigationDelegate = context.coordinator
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
            // just to make sure that even if updateUIView will not be called for any reason, the page
            // will be rendered anyway
            if !pageLoaded {
                loadPage(view)
            }
        }
        return view
    }

    func updateUIView(_ view: WKWebView, context: Context) {
        loadPage(view)
    }

    private func loadPage(_ webView: WKWebView) {
        let styles = """
        <style>
        body {
            color: \(theme.colors.onBackground.toHTMLHex());
            font-family: Helvetica;
        }
        a {
            color: \(theme.colors.primary.toHTMLHex());
        }
        code, pre {
            font-family: Menlo;
            background: \(theme.colors.secondary.opacity(theme.colors.isLight ? 0.2 : 0.3).toHTMLHex());
        }
        </style>
        """
        let head = "<head><meta name='viewport' content='width=device-width, initial-scale=1.0, minimum-scale=1.0, user-scalable=no'>\(styles)</head>"
        webView.loadHTMLString(head + html, baseURL: nil)
        DispatchQueue.main.async {
            pageLoaded = true
        }
    }

    func makeCoordinator() -> Cordinator {
        Cordinator()
    }

    class Cordinator: NSObject, WKNavigationDelegate {
        func webView(_ webView: WKWebView,
                     decidePolicyFor navigationAction: WKNavigationAction,
                     decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {

            guard let url = navigationAction.request.url else { return decisionHandler(.allow) }

            switch navigationAction.navigationType {
            case .linkActivated:
                decisionHandler(.cancel)
                if url.absoluteString.starts(with: "https://simplex.chat/contact#") {
                    ChatModel.shared.appOpenUrl = url
                } else {
                    UIApplication.shared.open(url)
                }
            default:
                decisionHandler(.allow)
            }
        }
    }
}
