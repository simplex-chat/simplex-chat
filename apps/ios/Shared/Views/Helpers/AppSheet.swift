//
//  AppSheet.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private struct SheetIsPresented<C>: ViewModifier where C: View {
    var isPresented: Binding<Bool>
    var onDismiss: (() -> Void)?
    var sheetContent: () -> C
    @Environment(\.scenePhase) var scenePhase

    func body(content: Content) -> some View {
        content.sheet(isPresented: isPresented, onDismiss: onDismiss) {
            sheetContent().modifier(PrivacySensitive())
        }
    }
}

private struct SheetForItem<T, C>: ViewModifier where T: Identifiable, C: View {
    var item: Binding<T?>
    var onDismiss: (() -> Void)?
    var sheetContent: (T) -> C
    @Environment(\.scenePhase) var scenePhase

    func body(content: Content) -> some View {
        content.sheet(item: item, onDismiss: onDismiss) { it in
            sheetContent(it).modifier(PrivacySensitive())
        }
    }
}

private struct PrivacySensitive: ViewModifier {
    @AppStorage(DEFAULT_PRIVACY_PROTECT_SCREEN) private var protectScreen = true
    @Environment(\.scenePhase) var scenePhase

    func body(content: Content) -> some View {
        if case .active = scenePhase {
            content
        } else {
            content.privacySensitive(protectScreen).redacted(reason: .privacy)
        }
    }
}

extension View {
    func appSheet<Content>(
        isPresented: Binding<Bool>,
        onDismiss: (() -> Void)? = nil,
        content: @escaping () -> Content
    ) -> some View where Content: View {
        modifier(SheetIsPresented(isPresented: isPresented, onDismiss: onDismiss, sheetContent: content))
    }

    func appSheet<T, Content>(
        item: Binding<T?>,
        onDismiss: (() -> Void)? = nil,
        content: @escaping (T) -> Content
    ) -> some View where T: Identifiable, Content: View {
        modifier(SheetForItem(item: item, onDismiss: onDismiss, sheetContent: content))
    }
}
