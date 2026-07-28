//
//  SupportSimpleXBanner.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// A dismissible chat-list card promoting the badges purchase flow. Tapping the card opens the
// badges entry view (BadgesSupportSimplexView) — the same first screen the settings row opens.
// The dismiss X hides the card permanently until DEFAULT_SUPPORTER_BANNER_SHOWN is reset.
struct SupportSimpleXBanner: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @AppStorage(DEFAULT_SUPPORTER_BANNER_SHOWN) private var supporterBannerShown = false
    let onTap: () -> Void

    var body: some View {
        if !supporterBannerShown {
            ZStack(alignment: .topTrailing) {
                Button(action: onTap) {
                    HStack(alignment: .center, spacing: 8) {
                        VStack(alignment: .leading, spacing: 4) {
                            Text("Support SimpleX")
                                .font(.headline)
                                .foregroundColor(theme.colors.primary)
                            Text("Get badge + files up to 5GB")
                                .font(.subheadline)
                                .foregroundColor(theme.colors.onBackground)
                        }
                        Spacer()
                        heroThumbnail()
                    }
                    .padding(EdgeInsets(top: 12, leading: 16, bottom: 12, trailing: 16))
                    .background(gradientBackground())
                    .clipShape(RoundedRectangle(cornerRadius: 20))
                }
                .buttonStyle(.plain)

                Button {
                    withAnimation { supporterBannerShown = true }
                } label: {
                    Image(systemName: "xmark")
                        .font(.system(size: 12, weight: .semibold))
                        .foregroundColor(theme.colors.secondary)
                        .frame(width: 24, height: 24)
                        .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .padding(6)
            }
        }
    }

    @ViewBuilder
    private func heroThumbnail() -> some View {
        #if SIMPLEX_ASSETS
        Image(colorScheme == .light ? "phone-supporter" : "phone-supporter-light")
            .resizable()
            .scaledToFit()
            .frame(width: 90, height: 90)
        #else
        Image("badge-supporter")
            .resizable()
            .scaledToFit()
            .frame(width: 56, height: 56)
        #endif
    }

    // Matches the onboarding card gradient so the banner reads as part of the same visual family.
    private func gradientBackground() -> some View {
        let gp = OnboardingCardView.gradientPoints(aspectRatio: 4.0, scale: colorScheme == .light ? 1.2 : 1.5)
        return LinearGradient(
            stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
            startPoint: gp.start,
            endPoint: gp.end
        )
    }
}

struct SupportSimpleXBanner_Previews: PreviewProvider {
    static var previews: some View {
        SupportSimpleXBanner(onTap: {})
            .padding()
    }
}
