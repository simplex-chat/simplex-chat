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
// The dismiss X shows a confirmation alert (matching the Reachable-toolbar banner), and once
// dismissed the banner stays hidden until DEFAULT_SUPPORTER_BANNER_SHOWN is reset. The row-level
// show gate (chat count > 2 counting only visible chats) lives in ChatListView.
struct SupportSimpleXBanner: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @AppStorage(DEFAULT_SUPPORTER_BANNER_SHOWN) private var supporterBannerShown = false
    @State private var showDismissAlert = false
    let onTap: () -> Void

    private let cardCornerRadius: CGFloat = 24
    private let cardHeight: CGFloat = 72
    private let cardHorizontalPadding: CGFloat = 16
    private let heroWidth: CGFloat = 90
    private let heroHeight: CGFloat = 110
    // Trailing padding keeps the hero's right edge to the LEFT of the dismiss X's left edge
    // (X's claim from the card right is trailing 16 + width 12 = 28pt, matching OneHandUICard,
    // so hero trailing 32 gives a ~4pt gap between hero right and X left).
    private let heroTrailingPadding: CGFloat = 32
    private let textToHeroGap: CGFloat = 8

    var body: some View {
        if !supporterBannerShown {
            // The card Button is the base (its natural size is the card rectangle, cardHeight tall)
            // and the hero image is added as an .overlay so it can visually extend past the card top
            // without changing the layout size. The dismiss X lives as a sibling in the outer
            // ZStack, aligned .topTrailing so it sits at the card's top-right; the X label uses
            // asymmetric vertical padding (12 top / 4 bottom) so the icon has a visible gap from
            // the card top edge without being pinned to the very top.
            ZStack(alignment: .topTrailing) {
                Button(action: onTap) {
                    HStack(spacing: 0) {
                        VStack(alignment: .leading, spacing: 4) {
                            Text("Support SimpleX")
                                .font(.headline)
                                .foregroundColor(theme.colors.primary)
                            Text("Get badge + files up to 5GB")
                                .font(.subheadline)
                                .foregroundColor(theme.colors.onBackground)
                        }
                        Spacer(minLength: heroWidth + heroTrailingPadding + textToHeroGap)
                    }
                    .padding(.horizontal, cardHorizontalPadding)
                    .frame(height: cardHeight)
                    .background(gradientBackground())
                    .clipShape(RoundedRectangle(cornerRadius: cardCornerRadius))
                }
                .buttonStyle(.plain)
                .overlay(alignment: .bottomTrailing) {
                    heroThumbnail()
                        .padding(.trailing, heroTrailingPadding)
                        .allowsHitTesting(false)
                }

                // X, sibling of the Button in ZStack, aligned to CARD top-right.
                Image(systemName: "multiply")
                    .foregroundColor(theme.colors.secondary)
                    .frame(width: 12, height: 12)
                    .padding(.top, 12)
                    .padding(.bottom, 4)
                    .padding(.trailing, 16)
                    .padding(.leading, 4)
                    .contentShape(Rectangle())
                    .onTapGesture { showDismissAlert = true }
            }
            .zIndex(1)
            .alert(isPresented: $showDismissAlert) {
                Alert(
                    title: Text("Support SimpleX"),
                    message: Text("You can support SimpleX later in Settings."),
                    dismissButton: .default(Text("Ok")) {
                        withAnimation { supporterBannerShown = true }
                    }
                )
            }
        }
    }

    @ViewBuilder
    private func heroThumbnail() -> some View {
        #if SIMPLEX_ASSETS
        // .resizable + .frame (no aspect modifier) stretches the asset to fill the frame exactly.
        // The asset aspect (0.795) is close enough to the frame aspect (heroWidth/heroHeight ≈ 0.82)
        // that horizontal stretching is barely perceptible (~3%), and this guarantees the phone
        // illustration's bottom lines up with the frame bottom = card bottom (no whitespace gap
        // from .scaledToFit centering, no overhang from .aspectRatio(.fill) leaking past the clip).
        Image(colorScheme == .light ? "phone-supporter" : "phone-supporter-light")
            .resizable()
            .frame(width: heroWidth, height: heroHeight)
            .clipShape(HeroBottomRightRoundedShape(cornerRadius: cardCornerRadius))
        #else
        Image("badge-supporter")
            .resizable()
            .scaledToFit()
            .frame(width: 48, height: 48)
            .padding(.top, (cardHeight - 48) / 2)
        #endif
    }

    private func gradientBackground() -> some View {
        let gp = OnboardingCardView.gradientPoints(aspectRatio: 4.0, scale: colorScheme == .light ? 1.2 : 1.5)
        return LinearGradient(
            stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
            startPoint: gp.start,
            endPoint: gp.end
        )
    }
}

// Rounds only the bottom-right corner of the hero to match the card's rounded bottom-right —
// leaves the top, left and bottom-left edges straight so the hero can extend above the card
// (top) and reach into the card without extra rounding on the sides.
private struct HeroBottomRightRoundedShape: Shape {
    let cornerRadius: CGFloat

    func path(in rect: CGRect) -> Path {
        var p = Path()
        p.move(to: CGPoint(x: rect.minX, y: rect.minY))
        p.addLine(to: CGPoint(x: rect.maxX, y: rect.minY))
        p.addLine(to: CGPoint(x: rect.maxX, y: rect.maxY - cornerRadius))
        p.addQuadCurve(
            to: CGPoint(x: rect.maxX - cornerRadius, y: rect.maxY),
            control: CGPoint(x: rect.maxX, y: rect.maxY)
        )
        p.addLine(to: CGPoint(x: rect.minX, y: rect.maxY))
        p.addLine(to: CGPoint(x: rect.minX, y: rect.minY))
        p.closeSubpath()
        return p
    }
}

struct SupportSimpleXBanner_Previews: PreviewProvider {
    static var previews: some View {
        SupportSimpleXBanner(onTap: {})
            .padding()
    }
}
