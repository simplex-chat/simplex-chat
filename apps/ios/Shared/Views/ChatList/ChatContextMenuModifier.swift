//
//  ChatContextMenuModifier.swift
//  SimpleX (iOS)
//
//  Created on 14/11/2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import UIKit
import SimpleXChat

// Spec: spec/client/chat-list.md#ChatContextMenuModifier
struct ChatContextMenuModifier: ViewModifier {
    let chat: Chat
    let theme: AppTheme
    let onCommit: () -> Void
    var onMarkRead: (() -> Void)? = nil
    var onToggleMute: (() -> Void)? = nil
    var onToggleFavorite: (() -> Void)? = nil
    var onClearChat: (() -> Void)? = nil
    var onDelete: (() -> Void)? = nil
    var deleteTitle: String = NSLocalizedString("Delete", comment: "menu action")
    var deleteIsLeave: Bool = false
    var disabled: Bool = false

    func body(content: Content) -> some View {
        if disabled {
            content
        } else {
            content.overlay(
                ChatContextMenuRepresentable(
                    chat: chat,
                    theme: theme,
                    onCommit: onCommit,
                    onMarkRead: onMarkRead,
                    onToggleMute: onToggleMute,
                    onToggleFavorite: onToggleFavorite,
                    onClearChat: onClearChat,
                    onDelete: onDelete,
                    deleteTitle: deleteTitle,
                    deleteIsLeave: deleteIsLeave
                )
            )
        }
    }
}

// Spec: spec/client/chat-list.md#ChatContextMenuRepresentable
struct ChatContextMenuRepresentable: UIViewRepresentable {
    let chat: Chat
    let theme: AppTheme
    let onCommit: () -> Void
    let onMarkRead: (() -> Void)?
    let onToggleMute: (() -> Void)?
    let onToggleFavorite: (() -> Void)?
    let onClearChat: (() -> Void)?
    let onDelete: (() -> Void)?
    let deleteTitle: String
    let deleteIsLeave: Bool

    func makeUIView(context: Context) -> ChatContextMenuUIView {
        let view = ChatContextMenuUIView()
        view.configure(
            chat: chat,
            theme: theme,
            onCommit: onCommit,
            onMarkRead: onMarkRead,
            onToggleMute: onToggleMute,
            onToggleFavorite: onToggleFavorite,
            onClearChat: onClearChat,
            onDelete: onDelete,
            deleteTitle: deleteTitle,
            deleteIsLeave: deleteIsLeave
        )
        return view
    }

    func updateUIView(_ uiView: ChatContextMenuUIView, context: Context) {
        uiView.configure(
            chat: chat,
            theme: theme,
            onCommit: onCommit,
            onMarkRead: onMarkRead,
            onToggleMute: onToggleMute,
            onToggleFavorite: onToggleFavorite,
            onClearChat: onClearChat,
            onDelete: onDelete,
            deleteTitle: deleteTitle,
            deleteIsLeave: deleteIsLeave
        )
    }
}

// Spec: spec/client/chat-list.md#ChatContextMenuUIView
final class ChatContextMenuUIView: UIView, UIContextMenuInteractionDelegate, UIGestureRecognizerDelegate {
    private var chat: Chat?
    private var theme: AppTheme?
    private var onCommit: (() -> Void)?
    private var onMarkRead: (() -> Void)?
    private var onToggleMute: (() -> Void)?
    private var onToggleFavorite: (() -> Void)?
    private var onClearChat: (() -> Void)?
    private var onDelete: (() -> Void)?
    private var deleteTitle: String = ""
    private var deleteIsLeave: Bool = false

    private var contextMenuInteraction: UIContextMenuInteraction?
    private var tapGesture: UITapGestureRecognizer?

    override init(frame: CGRect) {
        super.init(frame: frame)
        backgroundColor = .clear

        let interaction = UIContextMenuInteraction(delegate: self)
        addInteraction(interaction)
        self.contextMenuInteraction = interaction

        let tap = UITapGestureRecognizer(target: self, action: #selector(handleTap(_:)))
        tap.cancelsTouchesInView = false
        tap.delegate = self
        addGestureRecognizer(tap)
        self.tapGesture = tap
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func configure(
        chat: Chat,
        theme: AppTheme,
        onCommit: @escaping () -> Void,
        onMarkRead: (() -> Void)?,
        onToggleMute: (() -> Void)?,
        onToggleFavorite: (() -> Void)?,
        onClearChat: (() -> Void)?,
        onDelete: (() -> Void)?,
        deleteTitle: String,
        deleteIsLeave: Bool
    ) {
        self.chat = chat
        self.theme = theme
        self.onCommit = onCommit
        self.onMarkRead = onMarkRead
        self.onToggleMute = onToggleMute
        self.onToggleFavorite = onToggleFavorite
        self.onClearChat = onClearChat
        self.onDelete = onDelete
        self.deleteTitle = deleteTitle
        self.deleteIsLeave = deleteIsLeave
    }

    @objc private func handleTap(_ gesture: UITapGestureRecognizer) {
        guard gesture.state == .ended else { return }
        onCommit?()
    }

    // MARK: - UIGestureRecognizerDelegate

    func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer) -> Bool {
        // Allow simultaneous gestures with table view scrolling / swiping
        true
    }

    // MARK: - UIContextMenuInteractionDelegate

    func contextMenuInteraction(
        _ interaction: UIContextMenuInteraction,
        configurationForMenuAtLocation location: CGPoint
    ) -> UIContextMenuConfiguration? {
        guard let chat = self.chat, let theme = self.theme else { return nil }

        return UIContextMenuConfiguration(
            identifier: nil,
            previewProvider: { [weak self] in
                self?.makePreviewController(chat: chat, theme: theme)
            },
            actionProvider: { [weak self] _ in
                self?.makeMenu(chat: chat)
            }
        )
    }

    func contextMenuInteraction(
        _ interaction: UIContextMenuInteraction,
        willPerformPreviewActionForMenuWith configuration: UIContextMenuConfiguration,
        animator: UIContextMenuInteractionCommitAnimating
    ) {
        animator.addAnimations { [weak self] in
            self?.onCommit?()
        }
    }

    func contextMenuInteraction(
        _ interaction: UIContextMenuInteraction,
        configuration: UIContextMenuConfiguration,
        highlightPreviewForItemWithIdentifier identifier: Any
    ) -> UITargetedPreview? {
        guard let targetView = self.superview ?? self.window else { return nil }
        let parameters = UIPreviewParameters()
        parameters.backgroundColor = .clear
        return UITargetedPreview(view: targetView, parameters: parameters)
    }

    func contextMenuInteraction(
        _ interaction: UIContextMenuInteraction,
        configuration: UIContextMenuConfiguration,
        dismissalPreviewForItemWithIdentifier identifier: Any
    ) -> UITargetedPreview? {
        guard let targetView = self.superview ?? self.window else { return nil }
        let parameters = UIPreviewParameters()
        parameters.backgroundColor = .clear
        return UITargetedPreview(view: targetView, parameters: parameters)
    }

    // MARK: - Private Helpers

    private func makePreviewController(chat: Chat, theme: AppTheme) -> UIViewController {
        let previewView = ChatHistoryPreviewView(chat: chat)
            .environmentObject(theme)
        let hostingController = UIHostingController(rootView: previewView)
        let screenWidth = UIScreen.main.bounds.width
        let targetWidth = min(screenWidth - 32, 380)
        hostingController.preferredContentSize = CGSize(width: targetWidth, height: 440)
        hostingController.view.backgroundColor = .clear
        return hostingController
    }

    private func makeMenu(chat: Chat) -> UIMenu {
        var actions: [UIMenuElement] = []

        // 1. Mark as Read / Unread
        if let onMarkRead = self.onMarkRead {
            let isUnread = chat.chatStats.unreadCount > 0 || chat.chatStats.unreadChat
            let title = isUnread ? NSLocalizedString("Mark as Read", comment: "menu action") : NSLocalizedString("Mark as Unread", comment: "menu action")
            let iconName = isUnread ? "checkmark.circle" : "circlebadge.fill"
            let action = UIAction(title: title, image: UIImage(systemName: iconName)) { _ in
                onMarkRead()
            }
            actions.append(action)
        }

        // 2. Mute / Notifications
        if let onToggleMute = self.onToggleMute, let nextMode = chat.chatInfo.nextNtfMode {
            let title = nextMode.text(mentions: chat.chatInfo.hasMentions)
            let iconName = nextMode.iconFilled
            let action = UIAction(title: title, image: UIImage(systemName: iconName)) { _ in
                onToggleMute()
            }
            actions.append(action)
        }

        // 3. Favorite / Unfavorite
        if let onToggleFavorite = self.onToggleFavorite {
            let isFav = chat.chatInfo.chatSettings?.favorite == true
            let title = isFav ? NSLocalizedString("Unfavorite", comment: "menu action") : NSLocalizedString("Favorite", comment: "menu action")
            let iconName = isFav ? "star.slash.fill" : "star.fill"
            let action = UIAction(title: title, image: UIImage(systemName: iconName)) { _ in
                onToggleFavorite()
            }
            actions.append(action)
        }

        // 4. Clear Chat
        if let onClearChat = self.onClearChat, !chat.chatItems.isEmpty {
            let title = NSLocalizedString("Clear Chat", comment: "menu action")
            let action = UIAction(title: title, image: UIImage(systemName: "gobackward")) { _ in
                onClearChat()
            }
            actions.append(action)
        }

        // 5. Delete / Leave
        if let onDelete = self.onDelete, !deleteTitle.isEmpty {
            let iconName = deleteIsLeave ? "rectangle.portrait.and.arrow.right.fill" : "trash"
            let action = UIAction(title: deleteTitle, image: UIImage(systemName: iconName), attributes: .destructive) { _ in
                onDelete()
            }
            actions.append(action)
        }

        return UIMenu(title: "", children: actions)
    }
}
