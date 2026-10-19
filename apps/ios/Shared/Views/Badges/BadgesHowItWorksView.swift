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
            Text("How badges protect your privacy")
                .font(.largeTitle)
                .bold()
                .foregroundColor(theme.colors.primary)
                .padding(.top, 8)
                .padding(.bottom, 16)
            ScrollView {
                VStack(alignment: .leading, spacing: 12) {
                    Text("A badge is an anonymous credential stored in your profile on your device. This credential is never sent to anyone.")
                    Text("To prove to a contact or a server that you have a badge, the app generates a new proof that reveals only the badge type and the expiry date, rounded to a week.")
                    Text("Nobody can link two different proofs to each other or to the purchase.")
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
