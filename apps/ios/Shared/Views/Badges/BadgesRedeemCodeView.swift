//
//  BadgesRedeemCodeView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Draft entry-point for redeeming an investor badge code. TODO [badges]: implement input field,
// server verification and success/failure states when the redeem API is defined.
struct BadgesRedeemCodeView: View {
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        VStack(alignment: .leading) {
            Text("Redeem badge code")
                .font(.largeTitle)
                .bold()
                .foregroundColor(theme.colors.primary)
                .padding(.top, 8)
            Spacer()
        }
        .padding(.horizontal, 25)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }
}

struct BadgesRedeemCodeView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesRedeemCodeView()
        }
    }
}
