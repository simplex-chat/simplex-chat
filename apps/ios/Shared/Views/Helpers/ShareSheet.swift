//
//  ShareSheet.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

extension UIApplication {
    static let keyWindow = keyWindowScene?.windows.filter(\.isKeyWindow).first
    static let keyWindowScene = shared.connectedScenes.first { $0.activationState == .foregroundActive } as? UIWindowScene
}

extension View {
    func shareSheet(isPresented: Binding<Bool>, items: [Any]) -> some View {
        guard isPresented.wrappedValue else { return self }
        let activityViewController = UIActivityViewController(activityItems: items, applicationActivities: nil)
        let presentedViewController = UIApplication.keyWindow?.rootViewController?.presentedViewController ?? UIApplication.keyWindow?.rootViewController
        activityViewController.completionWithItemsHandler = { _, _, _, _ in isPresented.wrappedValue = false }
        presentedViewController?.present(activityViewController, animated: true)
        return self
    }
}

struct ShareSheetTest: View {
    @State private var isPresentingShareSheet = false

    var body: some View {
        Button("Show Share Sheet") { isPresentingShareSheet = true }
            .shareSheet(isPresented: $isPresentingShareSheet, items: ["Share me!"])
    }
}

struct ShareSheetTest_Previews: PreviewProvider {
    static var previews: some View {
        ShareSheetTest()
    }
}
