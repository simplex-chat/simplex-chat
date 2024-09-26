//
//  SwiftUISheet.swift
//  SimpleX (iOS)
//
//  Created by user on 23/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let sheetAnimationDuration: Double = 0.3

struct Sheet<SheetContent: View>: ViewModifier {
    @Binding var isPresented: Bool
    @ViewBuilder let sheetContent: () -> SheetContent

    func body(content: Content) -> some View {
        ZStack {
            content
            SheetRepresentable(isPresented: $isPresented, content: sheetContent())
                .allowsHitTesting(isPresented)
                .ignoresSafeArea()
        }
    }
}

struct SheetRepresentable<Content: View>: UIViewControllerRepresentable {
    @Binding var isPresented: Bool
    let content:  Content

    func makeUIViewController(context: Context) -> Controller<Content> {
        Controller(content: content, representer: self)
    }

    func updateUIViewController(_ sheetController: Controller<Content>, context: Context) {
        sheetController.animate(isPresented: isPresented)
        sheetController.hostingController.rootView = content
    }

    class Controller<C: View>: UIViewController {
        let hostingController: UIHostingController<C>
        private let animator = UIViewPropertyAnimator(duration: sheetAnimationDuration, curve: .easeInOut)
        private let representer: SheetRepresentable<C>
        private var retainedFraction: CGFloat = 0
        private var sheetHeight: Double { hostingController.view.frame.height }

        init(content: C, representer: SheetRepresentable<C>) {
            self.representer = representer
            self.hostingController = UIHostingController(rootView: content)
            super.init(nibName: nil, bundle: nil)
        }

        @available(*, unavailable)
        required init?(coder: NSCoder) { fatalError("init(coder:) missing") }

        func animate(isPresented: Bool) {
            let sheetDismissed = animator.fractionComplete == (animator.isReversed ? 1 : 0)
            if isPresented || !sheetDismissed {
                animator.pauseAnimation()
                animator.isReversed = !isPresented
                animator.continueAnimation(withTimingParameters: nil, durationFactor: 1)
            }
        }

        override func viewDidLoad() {
            view.backgroundColor = .clear
            view.addGestureRecognizer(
                UITapGestureRecognizer(target: self, action: #selector(tap(gesture:)))
            )
            addChild(hostingController)
            hostingController.didMove(toParent: self)
            if let sheet = hostingController.view {
                sheet.isHidden = true
                sheet.clipsToBounds = true
                sheet.layer.cornerRadius = 10
                sheet.layer.maskedCorners = [.layerMaxXMinYCorner, .layerMinXMinYCorner]
                sheet.addGestureRecognizer(UIPanGestureRecognizer(target: self, action: #selector(pan(gesture:))))
                sheet.translatesAutoresizingMaskIntoConstraints = false
                view.addSubview(sheet)
                NSLayoutConstraint.activate([
                    hostingController.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
                    hostingController.view.trailingAnchor.constraint(equalTo: view.trailingAnchor),
                    hostingController.view.bottomAnchor.constraint(equalTo: view.bottomAnchor),
                ])
                animator.pausesOnCompletion = true
                animator.scrubsLinearly = true
            }
        }

        override func viewDidAppear(_ animated: Bool) {
            hostingController.view.transform = CGAffineTransform(translationX: 0, y: self.sheetHeight)
            hostingController.view.isHidden = false
            self.animator.addAnimations {
                self.hostingController.view.transform = .identity
                self.view.backgroundColor = .black.withAlphaComponent(0.3)
            }
        }

        @objc
        func pan(gesture: UIPanGestureRecognizer) {
            switch gesture.state {
            case .began:
                animator.isReversed = false
                animator.pauseAnimation()
                retainedFraction = animator.fractionComplete
            case .changed:
                animator.fractionComplete = retainedFraction - gesture.translation(in: view).y / sheetHeight
            case .ended, .cancelled:
                let velocity = gesture.velocity(in: view).y
                animator.isReversed = (velocity - (animator.fractionComplete - 0.5) * 100).sign == .plus
                let defaultVelocity = sheetHeight / sheetAnimationDuration
                let fractionRemaining = 1 - animator.fractionComplete
                let durationFactor = min(max(fractionRemaining / (abs(velocity) / defaultVelocity), 0.2), 1)
                animator.continueAnimation(withTimingParameters: nil, durationFactor: durationFactor)
                DispatchQueue.main.asyncAfter(deadline: .now() + sheetAnimationDuration) {
                    self.representer.isPresented = !self.animator.isReversed
                }
            default: break
            }
        }

        @objc
        func tap(gesture: UITapGestureRecognizer) {
            switch gesture.state {
            case .ended:
                if gesture.location(in: view).y < view.frame.height - sheetHeight {
                    representer.isPresented = false
                }
            default: break
            }
        }
    }
}
