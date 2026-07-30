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
    private let cardHeight: CGFloat = 78
    private let cardHorizontalPadding: CGFloat = 16
    private let heroWidth: CGFloat = 90
    // Illustration is DRAWN at heroDrawnHeight (taller than the visible slot on purpose) — this is
    // what sets how far into the phone body the crop line falls. The wrapping slot is
    // heroVisibleHeight; the bottom (heroDrawnHeight - heroVisibleHeight) points get cut by
    // .clipped(), producing the "phone continues below but the banner slices it off" look. The
    // visible slot is bottom-anchored to the card via .overlay(.bottomTrailing), so the crop line
    // coincides with the card bottom. Aspect 90/130 = 0.69 is ~13% off the natural 0.795 (subtle
    // vertical stretch, much less than the earlier 25%).
    private let heroDrawnHeight: CGFloat = 130
    private let heroVisibleHeight: CGFloat = 100
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
                    // Hero bottom-anchored to card bottom via .overlay(.bottomTrailing); the
                    // illustration extends UP above the card top only. No .offset — nothing must
                    // draw below the card. .clipped() inside heroThumbnail is a hard rectangular
                    // clip at the hero's own frame boundary to guarantee that.
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
        // Two nested frames: the inner frame draws the image at heroDrawnHeight (taller than the
        // outer), and the outer frame constrains the visible slot to heroVisibleHeight with .top
        // alignment (so the drawn image's TOP shows and its BOTTOM sticks out below the slot).
        // .clipped() cuts that bottom overhang, producing the mid-illustration cut. Aspect is
        // slightly stretched (heroWidth/heroDrawnHeight = 0.59 vs the asset's natural 0.795, about
        // 25% more portrait) — this is what puts the crop line deep in the phone body.
        Image(colorScheme == .light ? "phone-supporter" : "phone-supporter-light")
            .resizable()
            .frame(width: heroWidth, height: heroDrawnHeight)
            .frame(width: heroWidth, height: heroVisibleHeight, alignment: .top)
            .clipped()
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

struct SupportSimpleXBanner_Previews: PreviewProvider {
    static var previews: some View {
        SupportSimpleXBanner(onTap: {})
            .padding()
    }
}
