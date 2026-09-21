//
//  GetStakeBanner.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 21.09.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Spec: spec/client/chat-list.md#GetStakeBanner
struct GetStakeBanner: View {
    @EnvironmentObject var theme: AppTheme
    var showDismiss: Bool
    let onTap: () -> Void
    let onDismiss: () -> Void

    var body: some View {
        ZStack(alignment: .topTrailing) {
            Button(action: onTap) {
                HStack(spacing: 0) {
                    VStack(alignment: .leading, spacing: 4) {
                        Text("Get a stake in SimpleX Chat!")
                            .font(.headline)
                            .foregroundColor(theme.colors.primary)
                            .lineLimit(2)
                        Text("Invest on Wefunder from $100")
                            .font(.subheadline)
                            .foregroundColor(theme.colors.onBackground)
                            .lineLimit(2)
                    }
                    Spacer(minLength: 6)
                    Image("wefunder_logo")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 48, height: 48)
                        // roughly where the phone of the badge banner's hero is: 40pt from the card edge, less its 8pt inset
                        .padding(.trailing, 32)
                }
                .modifier(BannerCard())
            }
            .buttonStyle(.plain)

            if showDismiss {
                BannerDismissButton(onDismiss: onDismiss)
            }
        }
    }
}

struct BannerCard: ViewModifier {
    @Environment(\.colorScheme) var colorScheme: ColorScheme

    // grows with Dynamic Type but never shrinks below the default, so small-font users see the same card
    @ScaledMetric(relativeTo: .body) private var scaledCardHeight: CGFloat = 72
    private var cardHeight: CGFloat { max(72, scaledCardHeight) }

    func body(content: Content) -> some View {
        content
            // the leading padding matches OneHandUICard's segment icon, so the text aligns with it in the list
            .padding(.leading, 16)
            .padding(.trailing, 8)
            .padding(.vertical, 12)
            .frame(minHeight: cardHeight)
            .background(gradientBackground())
            .clipShape(RoundedRectangle(cornerRadius: 16))
    }

    private func gradientBackground() -> some View {
        // Asymmetric scale: start (dark end) pushed further below the card than the end (warm) is
        // above, so the card's middle lands at the bright/mid-transition stop instead of the dark
        // navy region. Keeps the small warm accent at top-right.
        GeometryReader { geo in
            let aspect = max(geo.size.height, 1) / max(geo.size.width, 1)
            let startScale: CGFloat = colorScheme == .light ? 2.5 : 3.0
            let endScale: CGFloat = colorScheme == .light ? 1.7 : 2.1
            let gp = OnboardingCardView.gradientPoints(aspectRatio: aspect, scale: 1.0)
            let start = UnitPoint(x: 0.5 + (gp.start.x - 0.5) * startScale, y: 0.5 + (gp.start.y - 0.5) * startScale)
            let end = UnitPoint(x: 0.5 + (gp.end.x - 0.5) * endScale, y: 0.5 + (gp.end.y - 0.5) * endScale)
            return LinearGradient(
                stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
                startPoint: start,
                endPoint: end
            )
        }
    }
}

struct BannerDismissButton: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    let onDismiss: () -> Void

    var body: some View {
        Image(systemName: "multiply")
            .foregroundColor(colorScheme == .dark ? theme.colors.onBackground : theme.colors.secondary)
            .frame(width: 12, height: 12)
            .padding(.top, 12)
            .padding(.bottom, 4)
            .padding(.trailing, 16)
            .padding(.leading, 4)
            .contentShape(Rectangle())
            .onTapGesture(perform: onDismiss)
    }
}

#Preview {
    GetStakeBanner(showDismiss: true, onTap: {}, onDismiss: {})
        .padding()
}
