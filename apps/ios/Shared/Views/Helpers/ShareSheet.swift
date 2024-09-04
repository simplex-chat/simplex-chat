//
//  ShareSheet.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

func showShareSheet(items: [Any], completed: (() -> Void)? = nil) {
    let keyWindowScene = UIApplication.shared.connectedScenes.first { $0.activationState == .foregroundActive } as? UIWindowScene
    if let keyWindow = keyWindowScene?.windows.filter(\.isKeyWindow).first,
       let rootViewController = keyWindow.rootViewController {        
        // Find the top-most presented view controller
        var topController = rootViewController
        while let presentedViewController = topController.presentedViewController {
            topController = presentedViewController
        }
        let activityViewController = UIActivityViewController(activityItems: items, applicationActivities: nil)
        if let completed = completed {
            activityViewController.completionWithItemsHandler = { _, _, _, _ in
                completed()
            }
        }        
        topController.present(activityViewController, animated: true)
    }
}

extension View {
    func shareSheet(item: Binding<ShareItem?>) -> some View {
        sheet(item: item) { item in
            Group {
                if #available(iOS 16.0, *) {
                    ActivityView(item: item)
                        .presentationDetents([.medium, .large])
                } else {
                    ActivityView(item: item)
                }
            }.ignoresSafeArea()
        }
    }
}

struct ShareItem: Identifiable {
    let content: any Hashable
    var id: Int { content.hashValue }
}

private struct ActivityView: UIViewControllerRepresentable {
    let item: ShareItem

    func makeUIViewController(
        context: UIViewControllerRepresentableContext<ActivityView>
    ) -> UIActivityViewController {
        UIActivityViewController(
            activityItems: [item.content],
            applicationActivities: nil
        )
    }

    func updateUIViewController(
        _ uiViewController: UIActivityViewController,
        context: UIViewControllerRepresentableContext<ActivityView>
    ) { }
}
