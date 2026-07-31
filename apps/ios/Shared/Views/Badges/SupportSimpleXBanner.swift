//
//  SupportSimpleXBanner.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Dumb chat-list card promoting the badges purchase flow. Both the show gate and the dismissal
// persistence live in ChatListView; this view just renders and reports taps.
struct SupportSimpleXBanner: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @State private var showDismissAlert = false
    let onTap: () -> Void
    let onDismiss: () -> Void

    private let cardCornerRadius: CGFloat = 16
    // grows with Dynamic Type but never shrinks below the default so small-font users see the same
    // banner as today; hero stays fixed so its above-card overhang shrinks at very large fonts
    @ScaledMetric(relativeTo: .body) private var scaledCardHeight: CGFloat = 72
    private var cardHeight: CGFloat { max(72, scaledCardHeight) }
    // matches OneHandUICard's segment icon leading so the text aligns with it in the list
    private let cardLeadingPadding: CGFloat = 16
    private let cardTrailingPadding: CGFloat = 8
    private let heroWidth: CGFloat = 110
    // shorter than the natural drawn height so .clipped() slices the phone body at card bottom
    private let heroVisibleHeight: CGFloat = 108
    // hero right edge sits exactly at the dismiss X's left edge (X trailing 16 + width 12)
    private let heroTrailingPadding: CGFloat = 28
    private let textToHeroGap: CGFloat = 6

    var body: some View {
        // Card is the Button; hero is an overlay so it can extend above the card top without
        // affecting layout size. Dismiss X is a ZStack sibling anchored to the card's top-right.
        ZStack(alignment: .topTrailing) {
            Button(action: onTap) {
                HStack(spacing: 0) {
                    VStack(alignment: .leading, spacing: 4) {
                        Text("Support SimpleX")
                            .font(.headline)
                            .foregroundColor(theme.colors.primary)
                            .lineLimit(2)
                        Text("Get badge + files up to 5GB")
                            .font(.subheadline)
                            .foregroundColor(theme.colors.onBackground)
                            .lineLimit(2)
                    }
                    Spacer(minLength: heroWidth + heroTrailingPadding + textToHeroGap)
                }
                .padding(.leading, cardLeadingPadding)
                .padding(.trailing, cardTrailingPadding)
                .padding(.vertical, 12)
                .frame(minHeight: cardHeight)
                .background(gradientBackground())
                .clipShape(RoundedRectangle(cornerRadius: cardCornerRadius))
            }
            .buttonStyle(.plain)
            .overlay(alignment: .bottomTrailing) {
                heroThumbnail()
                    .padding(.trailing, heroTrailingPadding)
                    .allowsHitTesting(false)
            }

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
        .alert(isPresented: $showDismissAlert) {
            Alert(
                title: Text("Support SimpleX"),
                message: Text("You can support SimpleX later in Settings."),
                dismissButton: .default(Text("Ok"), action: onDismiss)
            )
        }
    }

    @ViewBuilder
    private func heroThumbnail() -> some View {
        #if SIMPLEX_ASSETS
        // draws at natural aspect, top-aligned in a shorter slot; .clipped() cuts the overflow at card bottom
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
            .padding(.vertical, (cardHeight - 48) / 2)
            .padding(.trailing, 12)
        #endif
    }

    private func gradientBackground() -> some View {
        // Same geometry-aware gradient as ConnectBannerCard: aspect ratio drives the start/end points
        // along a fixed 80° axis (OnboardingCardView.gradientAngle) so the full 5-stop palette lands
        // correctly regardless of the card being very wide (aspect ≈ 0.2) or growing at large fonts.
        GeometryReader { geo in
            let aspect = max(geo.size.height, 1) / max(geo.size.width, 1)
            let gp = OnboardingCardView.gradientPoints(
                aspectRatio: aspect,
                scale: colorScheme == .light ? 1.2 : 1.5
            )
            LinearGradient(
                stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
                startPoint: gp.start,
                endPoint: gp.end
            )
        }
    }
}

struct SupportSimpleXBanner_Previews: PreviewProvider {
    static var previews: some View {
        SupportSimpleXBanner(onTap: {}, onDismiss: {})
            .padding()
    }
}
