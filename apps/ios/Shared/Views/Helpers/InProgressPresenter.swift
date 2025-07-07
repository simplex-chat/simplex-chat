//
//  InProgressPresenter.swift
//  SimpleX
//
//  Created by spaced4ndy on 07.07.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI

@MainActor
final class InProgressPresenter: ObservableObject {
    static let shared = InProgressPresenter()
    private var overlayVC: UIViewController?

    func show(_ text: String?) async {
        guard overlayVC == nil else { return }
        
        if let topVC = getTopViewController() {
            let overlay = UIHostingController(rootView: InProgressOverlayView(text: text))
            overlay.view.backgroundColor = .clear
            overlay.modalPresentationStyle = .overFullScreen
            overlay.modalTransitionStyle = .crossDissolve
            overlay.view.isUserInteractionEnabled = true
            
            topVC.present(overlay, animated: false) {
                self.overlayVC = overlay
            }
        }
    }

    func hide() async {
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
