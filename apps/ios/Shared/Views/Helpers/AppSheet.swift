//
//  AppSheet.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private struct PrivacySensitive: ViewModifier {
    @AppStorage(DEFAULT_PRIVACY_PROTECT_SCREEN) private var protectScreen = false
    @Environment(\.scenePhase) var scenePhase

    func body(content: Content) -> some View {
        if !protectScreen {
            content
        } else {
            content.privacySensitive(scenePhase != .active).redacted(reason: .privacy)
        }
    }
}

extension View {
    func appSheet<Content>(
        isPresented: Binding<Bool>,
        onDismiss: (() -> Void)? = nil,
        content: @escaping () -> Content
    ) -> some View where Content: View {
        sheet(isPresented: isPresented, onDismiss: onDismiss) {
            content().modifier(PrivacySensitive())
        }
    }

    func appSheet<T, Content>(
        item: Binding<T?>,
        onDismiss: (() -> Void)? = nil,
        content: @escaping (T) -> Content
    ) -> some View where T: Identifiable, Content: View {
        sheet(item: item, onDismiss: onDismiss) { it in
            content(it).modifier(PrivacySensitive())
        }
    }
}
