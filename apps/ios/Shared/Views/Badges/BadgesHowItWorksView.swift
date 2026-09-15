//
//  BadgesHowItWorksView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct BadgesHowItWorksView: View {
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        VStack(alignment: .leading) {
            Text("How private badges work")
                .font(.largeTitle)
                .bold()
                .foregroundColor(theme.colors.primary)
                .padding(.top, 8)
                .padding(.bottom, 16)
            ScrollView {
                VStack(alignment: .leading, spacing: 12) {
                    Text("A badge is not an account. It is a signed credential stored on your device. It does not identify you, and no one keeps a record of who holds which badge.")
                    Text("Your contacts see the badge and its expiry date, and nothing else. The badge carries no identifier, so it cannot be used to find out who you are or to match you across chats.")
                    Text("Payment and badge are kept apart. Paying is one step; the badge is issued in another, under a key that exists only for that badge. Whoever handles the payment cannot see where the badge ends up.")
                }
                .lineLimit(nil)
                .fixedSize(horizontal: false, vertical: true)
            }
            Spacer()
        }
        .padding(.horizontal, 25)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }
}

struct BadgesHowItWorksView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesHowItWorksView()
        }
    }
}
