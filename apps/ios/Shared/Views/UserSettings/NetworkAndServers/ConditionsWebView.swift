//
//  ConditionsWebView.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 26.11.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//
// Spec: spec/architecture.md

import SwiftUI
import WebKit

struct ConditionsWebView: UIViewRepresentable {
    @State var html: String
    var openLink: (URL) -> Void = openExternalLink
    @EnvironmentObject var theme: AppTheme
    @State var pageLoaded = false

    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.defaultWebpagePreferences.allowsContentJavaScript = false
        config.websiteDataStore = .nonPersistent()
        let view = WKWebView(frame: .zero, configuration: config)
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
        let nonce = UUID().uuidString
        let styles = """
        <style nonce="\(nonce)">
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
        pre {
            overflow-x: auto;
        }
        table {
            border-collapse: collapse;
        }
        th, td {
            border: 1px solid \(theme.colors.secondary.toHTMLHex());
            padding: 4px 8px;
        }
        img {
            max-width: 100%;
        }
        </style>
        """
        let csp = "<meta http-equiv='Content-Security-Policy' content=\"default-src 'none'; style-src 'nonce-\(nonce)'; img-src data:; base-uri 'none'; form-action 'none'\">"
        let head = "<head>\(csp)<meta name='viewport' content='width=device-width, initial-scale=1.0, minimum-scale=1.0, user-scalable=no'>\(styles)</head>"
        webView.loadHTMLString(head + html, baseURL: nil)
        DispatchQueue.main.async {
            pageLoaded = true
        }
    }

    func makeCoordinator() -> Cordinator {
        Cordinator(openLink: openLink)
    }

    class Cordinator: NSObject, WKNavigationDelegate {
        let openLink: (URL) -> Void

        init(openLink: @escaping (URL) -> Void) {
            self.openLink = openLink
        }

        func webView(_ webView: WKWebView,
                     decidePolicyFor navigationAction: WKNavigationAction,
                     decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {

            guard let url = navigationAction.request.url else { return decisionHandler(.allow) }

            switch navigationAction.navigationType {
            case .linkActivated:
                decisionHandler(.cancel)
                openLink(url)
            default:
                decisionHandler(url.scheme == "about" ? .allow : .cancel)
            }
        }
    }
}
