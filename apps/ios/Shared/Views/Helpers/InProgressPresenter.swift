//
//  InProgressPresenter.swift
//  SimpleX
//
//  Created by spaced4ndy on 07.07.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct InProgressState {
    let text: String?
}

final class InProgressPresenter: ObservableObject {
    static let shared = InProgressPresenter()

    @Published private(set) var inProgressState: InProgressState? = nil

    func show(_ text: String?) {
        DispatchQueue.main.async {
            self.inProgressState = InProgressState(text: text)
            InProgressPresenter.presentOverlay(with: text)
        }
    }

    func hide() async {
        self.inProgressState = nil
        await InProgressPresenter.dismissOverlay()
    }

    private static var overlayVC: UIViewController?

    private static func presentOverlay(with text: String?) {
        guard overlayVC == nil, let topVC = getTopViewController() else { return }

        let overlayView = InProgressOverlayView(text: text)
        let hosting = UIHostingController(rootView: overlayView)
        hosting.view.backgroundColor = .clear
        hosting.modalPresentationStyle = .overFullScreen
        hosting.modalTransitionStyle = .crossDissolve
        hosting.view.isUserInteractionEnabled = true

        topVC.present(hosting, animated: false)
        overlayVC = hosting
    }

    private static func dismissOverlay() async {
        guard let overlayVC = overlayVC else { return }

        await withCheckedContinuation { continuation in
            Task { @MainActor in
                overlayVC.dismiss(animated: false) {
                    self.overlayVC = nil
                    continuation.resume()
                }
            }
        }
    }
}

struct InProgressOverlayView: View {
    let text: String?

    var body: some View {
        ZStack {
            Color.black.opacity(0.4)
                .ignoresSafeArea()

            VStack(spacing: 16) {
                ProgressView()
                    .scaleEffect(2)
                if let text = text {
                    Text(text)
                        .multilineTextAlignment(.center)
                }
            }
            .padding()
            .padding(.top)
            .background(Color(.systemBackground))
            .cornerRadius(12)
        }
    }
}
