//
//  SwiftUISheet.swift
//  SimpleX (iOS)
//
//  Created by user on 23/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let sheetAnimationDuration: Double = 0.35

// Refrence: https://easings.net/
private let easeOutCubic = UICubicTimingParameters(
    controlPoint1: CGPoint(x: 0.215, y: 0.61),
    controlPoint2: CGPoint(x: 0.355, y: 1)
)

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
    }

    class Controller<C: View>: UIViewController {
        let hostingController: UIHostingController<C>
        private let animator = UIViewPropertyAnimator(
            duration: sheetAnimationDuration,
            timingParameters: easeOutCubic
        )
        private let representer: SheetRepresentable<C>
        private var retainedFraction: CGFloat = 0
        private var sheetHeight: Double { hostingController.view.frame.height }
        private var task: Task<Void, Never>?

        init(content: C, representer: SheetRepresentable<C>) {
            self.representer = representer
            self.hostingController = UIHostingController(rootView: content)
            super.init(nibName: nil, bundle: nil)
        }

        @available(*, unavailable)
        required init?(coder: NSCoder) { fatalError("init(coder:) missing") }

        deinit {
            animator.stopAnimation(true)
            animator.finishAnimation(at: .current)
        }

        func animate(isPresented: Bool) {
            let alreadyAnimating = animator.isRunning && isPresented != animator.isReversed
            let sheetFullyDismissed = animator.fractionComplete == (animator.isReversed ? 1 : 0)
            let sheetFullyPresented = animator.fractionComplete == (animator.isReversed ? 0 : 1)

            if !isPresented && sheetFullyDismissed ||
                isPresented && sheetFullyPresented ||
                alreadyAnimating {
                return
            }

            animator.pauseAnimation()
            animator.isReversed = !isPresented
            animator.continueAnimation(
                withTimingParameters: isPresented
                ? easeOutCubic
                : UICubicTimingParameters(animationCurve: .easeIn),
                durationFactor: 1 - animator.fractionComplete
            )
            handleVisibility()
        }

        func handleVisibility() {
            if animator.isReversed {
                task = Task {
                    do {
                        let sleepDuration = UInt64(sheetAnimationDuration * Double(NSEC_PER_SEC))
                        try await Task.sleep(nanoseconds: sleepDuration)
                        view.isHidden = true
                    } catch { }
                }
            } else {
                task?.cancel()
                task = nil
                view.isHidden = false
            }
        }

        override func viewDidLoad() {
            view.isHidden = true
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
            }
        }

        override func viewDidAppear(_ animated: Bool) {
            // Ensures animations are only setup once
            // on some iOS version `viewDidAppear` can get called on each state change.
            if hostingController.view.isHidden {
                hostingController.view.transform = CGAffineTransform(translationX: 0, y: self.sheetHeight)
                hostingController.view.isHidden = false
                animator.pausesOnCompletion = true
                animator.addAnimations {
                    self.hostingController.view.transform = .identity
                    self.view.backgroundColor = UIColor {
                        switch $0.userInterfaceStyle {
                        case .dark: .black.withAlphaComponent(0.290)
                        default:    .black.withAlphaComponent(0.121)
                        }
                    }
                }
                animator.startAnimation()
                animator.pauseAnimation()
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
                let durationFactor = min(max(fractionRemaining / (abs(velocity) / defaultVelocity), 0.5), 1)
                animator.continueAnimation(withTimingParameters: nil, durationFactor: durationFactor * fractionRemaining)
                handleVisibility()
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
