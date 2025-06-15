//
//  ShareSheet.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

func getTopViewController() -> UIViewController? {
    let keyWindowScene = UIApplication.shared.connectedScenes.first { $0.activationState == .foregroundActive } as? UIWindowScene
    if let keyWindow = keyWindowScene?.windows.filter(\.isKeyWindow).first,
       let rootViewController = keyWindow.rootViewController {
        // Find the top-most presented view controller
        var topController = rootViewController
        while let presentedViewController = topController.presentedViewController {
            topController = presentedViewController
        }
        return topController
    }
    return nil
}

func showShareSheet(items: [Any], completed: (() -> Void)? = nil) {
    if let topController = getTopViewController() {
        let activityViewController = UIActivityViewController(activityItems: items, applicationActivities: nil)
        if let completed = completed {
            activityViewController.completionWithItemsHandler = { _, _, _, _ in
                completed()
            }
        }        
        topController.present(activityViewController, animated: true)
    }
}

func showAlert(
    title: String,
    message: String? = nil,
    buttonTitle: String,
    buttonAction: @escaping () -> Void,
    cancelButton: Bool
) -> Void {
    if let topController = getTopViewController() {
        let alert = UIAlertController(title: title, message: message, preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: buttonTitle, style: .default) { _ in
            buttonAction()
        })
        if cancelButton {
            alert.addAction(cancelAlertAction)
        }
        topController.present(alert, animated: true)
    }
}

func showAlert(
    _ title: String,
    message: String? = nil,
    actions: () -> [UIAlertAction] = { [okAlertAction] }
) {
    if let topController = getTopViewController() {
        let alert = UIAlertController(title: title, message: message, preferredStyle: .alert)
        for action in actions() { alert.addAction(action) }
        topController.present(alert, animated: true)
    }
}

func showSheet(
    _ title: String?,
    message: String? = nil,
    actions: () -> [UIAlertAction] = { [okAlertAction] },
    sourceView: UIView? = nil  // For iPad support
) {
    if let topController = getTopViewController() {
        let sheet = UIAlertController(title: title, message: message, preferredStyle: .actionSheet)
        for action in actions() { sheet.addAction(action) }

        // Required for iPad: Configure popover presentation
        if let popover = sheet.popoverPresentationController {
            popover.sourceView = sourceView ?? topController.view
            popover.sourceRect = sourceView?.bounds ?? CGRect(x: topController.view.bounds.midX, y: topController.view.bounds.midY, width: 0, height: 0)
            popover.permittedArrowDirections = []
        }

        topController.present(sheet, animated: true)
    }
}

let okAlertAction = UIAlertAction(title: NSLocalizedString("Ok", comment: "alert button"), style: .default)

let cancelAlertAction = UIAlertAction(title: NSLocalizedString("Cancel", comment: "alert button"), style: .cancel)

class OpenChatAlertViewController: UIViewController {
    private let profileName: String
    private let profileImage: UIView
    private let cancelTitle: String
    private let confirmTitle: String
    private let onCancel: () -> Void
    private let onConfirm: () -> Void

    init(
        profileName: String,
        profileImage: UIView,
        cancelTitle: String = "Cancel",
        confirmTitle: String = "Open",
        onCancel: @escaping () -> Void,
        onConfirm: @escaping () -> Void
    ) {
        self.profileName = profileName
        self.profileImage = profileImage
        self.cancelTitle = cancelTitle
        self.confirmTitle = confirmTitle
        self.onCancel = onCancel
        self.onConfirm = onConfirm
        super.init(nibName: nil, bundle: nil)

        modalPresentationStyle = .overFullScreen
        modalTransitionStyle = .crossDissolve
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

    override func viewDidLoad() {
        super.viewDidLoad()

        view.backgroundColor = UIColor.black.withAlphaComponent(0.3)

        // Container view
        let containerView = UIView()
        containerView.backgroundColor = .systemBackground
        containerView.layer.cornerRadius = 12
        containerView.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(containerView)

        // Profile image sizing
        profileImage.translatesAutoresizingMaskIntoConstraints = false
        NSLayoutConstraint.activate([
            profileImage.widthAnchor.constraint(equalToConstant: 60),
            profileImage.heightAnchor.constraint(equalToConstant: 60)
        ])

        // Name label
        let nameLabel = UILabel()
        nameLabel.text = profileName
        nameLabel.font = UIFont.systemFont(ofSize: 18, weight: .semibold)
        nameLabel.textColor = .label
        nameLabel.numberOfLines = 2
        nameLabel.translatesAutoresizingMaskIntoConstraints = false

        // Horizontal stack for image + name
        let hStack = UIStackView(arrangedSubviews: [profileImage, nameLabel])
        hStack.axis = .horizontal
        hStack.spacing = 12
        hStack.alignment = .center
        hStack.translatesAutoresizingMaskIntoConstraints = false

        let topRowContainer = UIView()
        topRowContainer.translatesAutoresizingMaskIntoConstraints = false
        topRowContainer.addSubview(hStack)

        NSLayoutConstraint.activate([
            hStack.topAnchor.constraint(equalTo: topRowContainer.topAnchor),
            hStack.bottomAnchor.constraint(equalTo: topRowContainer.bottomAnchor),
            hStack.leadingAnchor.constraint(equalTo: topRowContainer.leadingAnchor, constant: 20),
            hStack.trailingAnchor.constraint(equalTo: topRowContainer.trailingAnchor, constant: -20)
        ])

        // Buttons
        let cancelButton = UIButton(type: .system)
        cancelButton.setTitle(cancelTitle, for: .normal)
        cancelButton.titleLabel?.font = UIFont.systemFont(ofSize: 15)
        cancelButton.addTarget(self, action: #selector(cancelTapped), for: .touchUpInside)

        let confirmButton = UIButton(type: .system)
        confirmButton.setTitle(confirmTitle, for: .normal)
        confirmButton.titleLabel?.font = UIFont.systemFont(ofSize: 15, weight: .semibold)
        confirmButton.addTarget(self, action: #selector(confirmTapped), for: .touchUpInside)

        // Button stack with equal width buttons
        let buttonStack = UIStackView(arrangedSubviews: [cancelButton, confirmButton])
        buttonStack.axis = .horizontal
        buttonStack.distribution = .fillEqually
        buttonStack.spacing = 0 // no spacing, use divider instead
        buttonStack.translatesAutoresizingMaskIntoConstraints = false
        buttonStack.heightAnchor.constraint(greaterThanOrEqualToConstant: 50).isActive = true

        // Vertical stack containing hStack and buttonStack
        let vStack = UIStackView(arrangedSubviews: [topRowContainer, buttonStack])
        vStack.axis = .vertical
        vStack.spacing = 16
        vStack.alignment = .fill // important: buttons stretch full width
        vStack.translatesAutoresizingMaskIntoConstraints = false

        containerView.addSubview(vStack)

        // Add horizontal divider above buttons
        let horizontalDivider = UIView()
        horizontalDivider.backgroundColor = UIColor(white: 0.85, alpha: 1)
        horizontalDivider.translatesAutoresizingMaskIntoConstraints = false
        containerView.addSubview(horizontalDivider)

        // Add vertical divider between buttons
        let verticalDivider = UIView()
        verticalDivider.backgroundColor = UIColor(white: 0.85, alpha: 1)
        verticalDivider.translatesAutoresizingMaskIntoConstraints = false
        buttonStack.addSubview(verticalDivider)

        // Constraints

        NSLayoutConstraint.activate([
            // Container view centering and fixed width
            containerView.centerYAnchor.constraint(equalTo: view.centerYAnchor),
            containerView.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            containerView.widthAnchor.constraint(equalToConstant: 280),

            // Vertical stack padding inside containerView
            vStack.topAnchor.constraint(equalTo: containerView.topAnchor, constant: 20),
            vStack.leadingAnchor.constraint(equalTo: containerView.leadingAnchor, constant: 0),
            vStack.trailingAnchor.constraint(equalTo: containerView.trailingAnchor, constant: 0),
            vStack.bottomAnchor.constraint(equalTo: containerView.bottomAnchor, constant: 0),

            // Center hStack horizontally inside vStack's padded width
            hStack.centerXAnchor.constraint(equalTo: vStack.centerXAnchor),

            // Horizontal divider above buttons
            horizontalDivider.leadingAnchor.constraint(equalTo: containerView.leadingAnchor),
            horizontalDivider.trailingAnchor.constraint(equalTo: containerView.trailingAnchor),
            horizontalDivider.bottomAnchor.constraint(equalTo: buttonStack.topAnchor),
            horizontalDivider.heightAnchor.constraint(equalToConstant: 1 / UIScreen.main.scale),

            // Vertical divider between buttons
            verticalDivider.widthAnchor.constraint(equalToConstant: 1 / UIScreen.main.scale),
            verticalDivider.topAnchor.constraint(equalTo: buttonStack.topAnchor),
            verticalDivider.bottomAnchor.constraint(equalTo: containerView.bottomAnchor),
            verticalDivider.centerXAnchor.constraint(equalTo: buttonStack.centerXAnchor)
        ])
    }

    @objc private func cancelTapped() {
        dismiss(animated: true) {
            self.onCancel()
        }
    }

    @objc private func confirmTapped() {
        dismiss(animated: true) {
            self.onConfirm()
        }
    }
}


func showOpenChatAlert<Content: View>(
    profileName: String,
    profileImage: Content,
    theme: AppTheme,
    cancelTitle: String = "Cancel",
    confirmTitle: String = "Open",
    onCancel: @escaping () -> Void = {},
    onConfirm: @escaping () -> Void
) {
    let themedView = profileImage.environmentObject(theme)
    let hostingController = UIHostingController(rootView: themedView)
    let hostedView = hostingController.view!
    hostedView.backgroundColor = .clear

    if let topVC = getTopViewController() {
        let alertVC = OpenChatAlertViewController(
            profileName: profileName,
            profileImage: hostedView,
            cancelTitle: cancelTitle,
            confirmTitle: confirmTitle,
            onCancel: onCancel,
            onConfirm: onConfirm
        )
        topVC.present(alertVC, animated: true)
    }
}
