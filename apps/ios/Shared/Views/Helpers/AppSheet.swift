//
//  AppSheet.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

class AppSheetState: ObservableObject {
    static let shared = AppSheetState()
    @Published var scenePhaseActive: Bool = false

    func redactionReasons(_ protectScreen: Bool) -> RedactionReasons {
        !protectScreen || scenePhaseActive
        ? RedactionReasons()
        : RedactionReasons.placeholder
    }
}

private struct PrivacySensitive: ViewModifier {
    @AppStorage(DEFAULT_PRIVACY_PROTECT_SCREEN) private var protectScreen = false
    // Screen protection doesn't work for appSheet on iOS 16 if @Environment(\.scenePhase) is used instead of global state
    @ObservedObject var appSheetState: AppSheetState = AppSheetState.shared

    func body(content: Content) -> some View {
        content.redacted(reason: appSheetState.redactionReasons(protectScreen))
    }
}

// Presented from the top view controller instead of a .sheet on a parent view, so an alert button or
// a view that is itself in a sheet can open it.
func showAppSheet<Content: View>(@ViewBuilder content: () -> Content) {
    if let topController = getTopViewController() {
        let v = content()
            .modifier(PrivacySensitive())
            .environmentObject(ChatModel.shared)
            .environmentObject(AppTheme.shared)
        topController.present(UIHostingController(rootView: v), animated: true)
    }
}

extension View {
    func appSheet<Content>(
        isPresented: Binding<Bool>,
        onDismiss: (() -> Void)? = nil,
        @ViewBuilder content: @escaping () -> Content
    ) -> some View where Content: View {
        sheet(isPresented: isPresented, onDismiss: onDismiss) {
            content().modifier(PrivacySensitive())
        }
    }

    func appSheet<T, Content>(
        item: Binding<T?>,
        onDismiss: (() -> Void)? = nil,
        @ViewBuilder content: @escaping (T) -> Content
    ) -> some View where T: Identifiable, Content: View {
        sheet(item: item, onDismiss: onDismiss) { it in
            content(it).modifier(PrivacySensitive())
        }
    }
}
