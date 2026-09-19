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
                    Text("A badge is an anonymous credential stored in your profile on your device. It is not an account, and it does not identify you.")
                    Text("The credential itself is never sent to anyone. To show the badge to a contact or to present it to a server, the app generates a new proof that reveals only the badge type and the expiry date.")
                    Text("No two proofs can be linked to each other or to the purchase – by the badge service, your contacts or servers.")
                    ExternalLink("Read more in our blog.", destination: URL(string: "https://simplex.chat/blog/20260919-simplex-supporter-badges.html")!)
                }
                .lineLimit(nil)
                .fixedSize(horizontal: false, vertical: true)
            }
            Spacer()
        }
        .padding(.horizontal, 25)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .navigationBarTitleDisplayMode(.inline)
    }
}

struct BadgesHowItWorksView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesHowItWorksView()
        }
    }
}
