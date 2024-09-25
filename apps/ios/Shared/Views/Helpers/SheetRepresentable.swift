//
//  SwiftUISheet.swift
//  SimpleX (iOS)
//
//  Created by user on 23/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let sheetAnimationDuration: Double = 0.3

struct SheetRepresentable<Content: View>: UIViewControllerRepresentable {
    @Binding var isPresented: Bool
    @ViewBuilder let content: () -> Content

    func makeUIViewController(context: Context) -> Controller<Content> {
        Controller(content: content(), representer: self)
    }

    func updateUIViewController(_ sheetController: Controller<Content>, context: Context) {
        sheetController.animate(isPresented: isPresented)
        sheetController.hostingController.rootView = content()
    }

    class Controller<C: View>: UIViewController {
        let hostingController: UIHostingController<C>
        private let animator = UIViewPropertyAnimator(duration: sheetAnimationDuration, curve: .easeInOut)
        private let representer: SheetRepresentable<C>
        private var retainedFraction: CGFloat = 0

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
                sheet.backgroundColor = UIColor { traits in
                    let elevated = switch traits.userInterfaceStyle {
                    case .dark: elevatedSystemGroupedBackground(.dark)
                    default: elevatedSystemGroupedBackground(.light)
                    }
                    return elevated.cgColor.map { UIColor(cgColor: $0) }
                    ?? .secondarySystemBackground
                }
                sheet.layer.cornerRadius = 10
                sheet.layer.maskedCorners = [.layerMaxXMinYCorner, .layerMinXMinYCorner]
                sheet.addGestureRecognizer(UIPanGestureRecognizer(target: self, action: #selector(pan(gesture:))))
                sheet.translatesAutoresizingMaskIntoConstraints = false
                view.addSubview(sheet)
                NSLayoutConstraint.activate([
                    hostingController.view.topAnchor.constraint(equalTo: view.bottomAnchor),
                    hostingController.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
                    hostingController.view.trailingAnchor.constraint(equalTo: view.trailingAnchor)
                ])
                animator.pausesOnCompletion = true
                animator.scrubsLinearly = true
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) { [weak self] in
                    self?.animator.addAnimations {
                        sheet.transform = CGAffineTransform(translationX: 0, y: -sheet.frame.height)
                        self?.view.backgroundColor = .black.withAlphaComponent(0.3)
                    }
                }
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
                animator.fractionComplete = retainedFraction - gesture.translation(in: view).y / hostingController.view.frame.height
            case .ended, .cancelled:
                let velocity = gesture.velocity(in: view).y
                animator.isReversed = velocity.sign == .plus
                let defaultVelocity = hostingController.view.frame.height / sheetAnimationDuration
                let fractionRemaining = 1 - animator.fractionComplete
                let durationFactor = min(fractionRemaining / (abs(velocity) / defaultVelocity), 1)
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
                representer.isPresented = false
            default: break
            }
        }
    }
}

// MARK: Sheet Colors

func elevatedSystemGroupedBackground(_ colorScheme: ColorScheme) -> Color {
    switch colorScheme {
    case .dark: Color(0xFF1C1C1E)
    default:    Color(0xFFF2F2F7)
    }
}

func elevatedSecondarySystemGroupedBackground(_ colorScheme: ColorScheme) -> Color {
    switch colorScheme {
    case .dark: Color(0xFF2C2C2E)
    default:    Color(0xFFFFFFFF)
    }
}

/// # Extracting Sheet Colors Programatically
///
/// System colors are returned dynamically, depending on the context:
///
///     struct ColorResolverView: View {
///         @Environment(\.self) var environment
///         let colors: [Color]
///
///         var body: some View {
///             HStack {
///                 column.environment(\.colorScheme, .dark)
///                 column.environment(\.colorScheme, .light)
///             }
///         }
///
///         var column: some View {
///             VStack {
///                 ForEach(colors, id: \.self) {
///                     Text("\($0.resolve(in: environment))")
///                         .frame(maxWidth: .infinity, maxHeight: .infinity)
///                         .background($0)
///                 }
///             }
///           }
///     }
///
/// Place `ColorResolverView` inside a sheet to acquire elevated color versions:
///
///     struct ContentView: View {
///         var body: some View {
///             EmptyView()
///             .sheet(isPresented: .constant(true)) {
///                 ColorResolverView(
///                     colors: [
///                         Color(.systemGroupedBackground),
///                         Color(.secondarySystemGroupedBackground)
///                     ]
///                 )
///             }
///         }
///     }


