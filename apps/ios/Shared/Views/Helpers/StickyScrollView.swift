//
//  StickyScrollView.swift
//  SimpleX (iOS)
//
//  Created by user on 20/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebKit

struct StickyScrollView<Content: View>: UIViewRepresentable {
    @Binding var resetScroll: ResetScrollAction
    @ViewBuilder let content: () -> Content

    func makeUIView(context: Context) -> UIScrollView {
        let hc = context.coordinator.hostingController
        hc.view.backgroundColor = .clear
        let sv = UIScrollView()
        sv.showsHorizontalScrollIndicator = false
        sv.addSubview(hc.view)
        sv.delegate = context.coordinator
        DispatchQueue.main.async {
            resetScroll = ResetScrollAction { sv.setContentOffset(.zero, animated: false) }
        }
        return sv
    }

    func updateUIView(_ scrollView: UIScrollView, context: Context) {
        let hc = context.coordinator.hostingController
        hc.rootView = content()
        hc.view.frame.size = hc.view.intrinsicContentSize
        scrollView.contentSize = hc.view.intrinsicContentSize
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(content: content())
    }

    class Coordinator: NSObject, UIScrollViewDelegate {
        let hostingController: UIHostingController<Content>

        init(content: Content) {
            self.hostingController = UIHostingController(rootView: content)
        }

        func scrollViewWillEndDragging(
            _ scrollView: UIScrollView,
            withVelocity velocity: CGPoint,
            targetContentOffset: UnsafeMutablePointer<CGPoint>
        ) {
            if targetContentOffset.pointee.x < 32 {
                targetContentOffset.pointee.x = 0
            }
        }
    }
}

struct ResetScrollAction {
    var action = { }
    func callAsFunction() { action() }
}



struct InkMarkdown: UIViewRepresentable {
    @State var html: String
    @EnvironmentObject var theme: AppTheme

    func makeUIView(context: Context) -> WKWebView {
        let view = WKWebView()
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
        logger.debug("LALAL HTML \(head + html)")
        view.loadHTMLString(head + html, baseURL: nil)
        view.backgroundColor = .clear
        view.isOpaque = false
        view.navigationDelegate = context.coordinator
        return view
    }

    func updateUIView(_ downView: WKWebView, context: Context) {}

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


//import Down

//struct MarkdownView: UIViewRepresentable {
//    @State var markdown: String
//
//    func makeUIView(context: Context) -> DownView {
//        let view = try! DownView(frame: .zero, markdownString: markdown)
//        view.backgroundColor = .clear
//        view.isOpaque = false
//        view.pageZoom = 1.5
//        return view
//    }
//
//    func updateUIView(_ downView: DownView, context: Context) {}
//}


//class MarkdownObservable: ObservableObject {
//    @Published public var textView = UITextView()
//    public let text: String
//
//    init(text: String) {
//        self.text = text
//    }
//}
//
//struct MarkdownRepresentable: UIViewRepresentable {
//    @Environment(\.colorScheme) var colorScheme
//    //@Binding var dynamicHeight: CGFloat
//    @EnvironmentObject var markdownObject: MarkdownObservable
//
//    @State var test: Int = 0
//
////    init(height: Binding<CGFloat>) {
////        self._dynamicHeight = height
////    }
//
//
//    // TODO: As soon as PR: 258 is accepted - you need to uncomment
//    //    func makeCoordinator() -> Cordinator {
//    //        Cordinator(text: markdownObject.textView)
//    //    }
//
//    func makeUIView(context: Context) -> UITextView {
//
//        let down = Down(markdownString: markdownObject.text)
//
//        // TODO: As soon as PR: 258 is accepted - you need to uncomment
//        //        let attributedText = try? down.toAttributedString(styler: DownStyler(delegate: context.coordinator))
//
//        let attributedText = try? down.toAttributedString(styler: DownStyler())
//        markdownObject.textView.attributedText = attributedText
//        markdownObject.textView.textAlignment = .left
//        markdownObject.textView.isScrollEnabled = false
//        markdownObject.textView.isUserInteractionEnabled = true
//        markdownObject.textView.showsVerticalScrollIndicator = false
//        markdownObject.textView.showsHorizontalScrollIndicator = false
//        markdownObject.textView.isEditable = false
//        markdownObject.textView.backgroundColor = .clear
//
//        markdownObject.textView.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
//        markdownObject.textView.setContentCompressionResistancePriority(.defaultLow, for: .vertical)
//
//        return markdownObject.textView
//    }
//
//    func updateUIView(_ uiView: UITextView, context: Context) {
//        DispatchQueue.main.async {
//            /// Allows you to change the color of the text when switching the device theme.
//            /// I advise you to do it in the future through the configuration when setting up your own Styler class
//            uiView.textColor = colorScheme == .dark ? UIColor.white : UIColor.black
//
////            dynamicHeight = uiView.sizeThatFits(CGSize(width: uiView.bounds.width,
////                                                       height: CGFloat.greatestFiniteMagnitude))
////            .height
//        }
//    }
//
//    // TODO: As soon as PR: 258 is accepted - you need to uncomment
//    //    class Cordinator: NSObject, AsyncImageLoadDelegate {
//    //
//    //        public var textView: UITextView
//    //
//    //        init(text: UITextView) {
//    //            textView = text
//    //        }
//    //
//    //        func textAttachmentDidLoadImage(textAttachment: AsyncImageLoad, displaySizeChanged: Bool)
//    //            {
//    //                if displaySizeChanged
//    //                {
//    //                    textView.layoutManager.setNeedsLayout(forAttachment: textAttachment)
//    //                }
//    //
//    //                // always re-display, the image might have changed
//    //                textView.layoutManager.setNeedsDisplay(forAttachment: textAttachment)
//    //            }
//    //    }
//}

