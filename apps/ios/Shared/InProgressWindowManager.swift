//
//  InProgressWindowManager.swift
//  SimpleX
//
//  Created by spaced4ndy on 07.07.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI

final class InProgressWindowManager {
    static let shared = InProgressWindowManager()

    private var window: UIWindow?

    func show(with text: String? = nil) {
        guard window == nil else { return }

        let hostingController = UIHostingController(rootView: InProgressOverlayView(text: text))
        hostingController.view.backgroundColor = .clear

        let newWindow = UIWindow(frame: UIScreen.main.bounds)
        newWindow.rootViewController = hostingController
        newWindow.windowLevel = .alert + 1
        newWindow.makeKeyAndVisible()

        window = newWindow

        print("Showing InProgressOverlayView")
    }

    func hide() {
        window?.isHidden = true
        window = nil
    }
}

struct InProgressOverlayView: View {
    let text: String?

    var body: some View {
        print("Rendering InProgressOverlayView")
        return viewBody()
    }

    @ViewBuilder func viewBody() -> some View {
        ZStack {
            Color.red.ignoresSafeArea()
            Text("Loading…")
                .font(.largeTitle)
                .foregroundColor(.white)
        }
    }

//    var body: some View {
//        ZStack {
//            Color.black.opacity(0.4)
//                .ignoresSafeArea()
//
//            Group {
//                if let text = text {
//                    ProgressView(text)
//                } else {
//                    ProgressView()
//                }
//            }
//            .padding()
//            .background(Color(.systemBackground))
//            .cornerRadius(12)
//        }
//    }
}
