//
//  BadgesHowItWorksView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// TODO [badges]: replace lorem ipsum with the real copy once the badge protocol and privacy properties are documented.
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
                    Text("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
                    Text("Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
                    Text("Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
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
