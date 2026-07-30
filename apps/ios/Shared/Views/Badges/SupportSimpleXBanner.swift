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

    private let cardCornerRadius: CGFloat = 16
    private let cardHeight: CGFloat = 72
    // 16pt matches OneHandUICard's segment icon .padding(.leading, 16) — so the banner's text
    // aligns with the icon column of the Reachable-toolbar card above it in the list.
    private let cardLeadingPadding: CGFloat = 16
    private let cardTrailingPadding: CGFloat = 8
    private let heroWidth: CGFloat = 110
    // Illustration is DRAWN at NATURAL aspect (0.795) via .aspectRatio(.fill) — for heroWidth 110
    // that gives a 110×138 image. The visible slot is heroWidth × heroVisibleHeight (110×108), top-
    // aligned; the ~30pt of natural image below the slot is cut by .clipped(). Crop line lands
    // ~78% into the phone body.
    private let heroVisibleHeight: CGFloat = 108
    // 28 (was 32) — hero right sits exactly at X's left edge (X's own trailing 16 + width 12 = 28).
    // No overlap, no gap. Required so the wider 110pt hero still leaves subtitle room.
    private let heroTrailingPadding: CGFloat = 28
    // 6 (was 8) — small reduction to reclaim text width with the bigger hero.
    private let textToHeroGap: CGFloat = 6

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
                    .padding(.leading, cardLeadingPadding)
                    .padding(.trailing, cardTrailingPadding)
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
        // .aspectRatio(.fill) draws at NATURAL aspect (0.795) filling the frame — image ends up
        // ~110×138. .frame(width, heroVisibleHeight, alignment: .top) makes the visible slot
        // shorter than the drawn image, top-aligned; .clipped() cuts the ~30pt of natural image
        // that would otherwise overflow the slot. The crop line lands ~78% into the phone body.
        Image(colorScheme == .light ? "phone-supporter" : "phone-supporter-light")
            .resizable()
            .aspectRatio(contentMode: .fill)
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
