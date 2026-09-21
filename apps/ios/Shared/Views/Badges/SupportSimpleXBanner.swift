//
//  SupportSimpleXBanner.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SupportSimpleXBanner: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    var title: LocalizedStringKey = "Support SimpleX"
    var subtitle: LocalizedStringKey = "Get badge + better files"
    let onTap: () -> Void
    let onDismiss: () -> Void

    // the card's own height, for centring the fallback hero; hero stays fixed so its above-card
    // overhang shrinks at very large fonts
    @ScaledMetric(relativeTo: .body) private var scaledCardHeight: CGFloat = 72
    private var cardHeight: CGFloat { max(72, scaledCardHeight) }
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
                        Text(title)
                            .font(.headline)
                            .foregroundColor(theme.colors.primary)
                            .lineLimit(2)
                        Text(subtitle)
                            .font(.subheadline)
                            .foregroundColor(theme.colors.onBackground)
                            .lineLimit(2)
                    }
                    Spacer(minLength: heroWidth + heroTrailingPadding + textToHeroGap)
                }
                .modifier(BannerCard())
            }
            .buttonStyle(.plain)
            .overlay(alignment: .bottomTrailing) {
                heroThumbnail()
                    .padding(.trailing, heroTrailingPadding)
                    .allowsHitTesting(false)
            }

            BannerDismissButton(onDismiss: onDismiss)
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
}

struct SupportSimpleXBanner_Previews: PreviewProvider {
    static var previews: some View {
        SupportSimpleXBanner(onTap: {}, onDismiss: {})
            .padding()
    }
}
