//
//  ConnectBannerCard.swift
//  SimpleX (iOS)
//
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let bannerImageRatio: CGFloat = 800 / 505

struct ConnectBannerCard: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
    @AppStorage(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial
    @AppStorage(DEFAULT_ADDRESS_CREATION_CARD_SHOWN) private var addressCreationCardShown = false
    @State private var showNewLink = false
    @State private var showPasteLink = false

    var body: some View {
        VStack(alignment: .trailing, spacing: 3) {
            Button {
                withAnimation { addressCreationCardShown = true }
            } label: {
                Image(systemName: "multiply")
                    .font(.system(size: 14, weight: .semibold))
                    .foregroundColor(theme.colors.secondary)
                    .frame(width: 30, height: 30)
                    .background(theme.colors.onBackground.opacity(0.08), in: Circle())
            }
            HStack(spacing: 2) {
                bannerHalf(
                    imageName: "banner-create-link",
                    icon: "link.badge.plus",
                    title: "New 1-time link",
                    action: { showNewLink = true }
                )
                bannerHalf(
                    imageName: "banner-paste-link",
                    icon: "qrcode.viewfinder",
                    title: "Paste link / Scan",
                    action: { showPasteLink = true }
                )
            }
            .clipShape(RoundedRectangle(cornerRadius: 18))
        }
        .sheet(isPresented: $showNewLink) {
            NavigationView {
                NewChatView(selection: .invite)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
        .sheet(isPresented: $showPasteLink) {
            NavigationView {
                NewChatView(selection: .connect, showQRCodeScanner: true)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
    }

    @ViewBuilder
    private func bannerHalf(imageName: String, icon: String, title: LocalizedStringKey, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            VStack(spacing: 0) {
                #if SIMPLEX_ASSETS
                Image(colorScheme == .light ? imageName : "\(imageName)-light")
                    .resizable()
                    .scaledToFit()
                    .frame(maxWidth: .infinity)
                #else
                gradientFallback(icon: icon)
                #endif
                HStack(spacing: 8) {
                    #if SIMPLEX_ASSETS
                    Image(systemName: icon)
                        .font(.system(size: 18))
                        .foregroundColor(theme.colors.primary)
                    #endif
                    Text(title)
                        .font(.footnote)
                        .foregroundColor(theme.colors.onBackground)
                        .lineLimit(1)
                        .minimumScaleFactor(0.75)
                }
                .frame(maxWidth: .infinity)
                .padding(.vertical, 8)
                .background(ToolbarMaterial.material(toolbarMaterial))
            }
        }
        .buttonStyle(.plain)
    }

    @ViewBuilder
    private func gradientFallback(icon: String) -> some View {
        let gp = OnboardingCardView.gradientPoints(
            aspectRatio: 1 / bannerImageRatio,
            scale: colorScheme == .light ? 1.2 : 1.5
        )
        ZStack {
            LinearGradient(
                stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
                startPoint: gp.start,
                endPoint: gp.end
            )
            Image(systemName: icon)
                .font(.system(size: 40))
                .foregroundColor(theme.colors.primary)
        }
        .aspectRatio(bannerImageRatio, contentMode: .fit)
        .frame(maxWidth: .infinity)
    }
}
