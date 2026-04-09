//
//  OnboardingCards.swift
//  SimpleX (iOS)
//
//  Created by simplex-chat on 06.04.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// MARK: - Card component

struct OnboardingCardView: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
    @AppStorage(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial
    let imageName: String
    let icon: String
    let title: LocalizedStringKey
    var subtitle: LocalizedStringKey? = nil
    let labelHeightRatio: CGFloat
    let action: () -> Void

    static let lightStops: [Gradient.Stop] = [
        .init(color: Color(red: 0.824, green: 0.910, blue: 1.0), location: 0.0),
        .init(color: Color(red: 0.800, green: 0.914, blue: 1.0), location: 0.5),
        .init(color: Color(red: 0.875, green: 1.0, blue: 1.0), location: 0.9),
        .init(color: Color(red: 1.0, green: 0.988, blue: 0.918), location: 1.0)
    ]

    static let darkStops: [Gradient.Stop] = [
        .init(color: Color(red: 0.016, green: 0.039, blue: 0.141), location: 0.4),
        .init(color: Color(red: 0.220, green: 0.329, blue: 0.671), location: 0.72),
        .init(color: Color(red: 0.659, green: 0.929, blue: 0.953), location: 0.9),
        .init(color: Color(red: 1.0, green: 0.965, blue: 0.878), location: 1.0)
    ]

    static let gradientAngle: Double = 80.0 * .pi / 180.0

    static func gradientPoints(aspectRatio: CGFloat, scale: CGFloat) -> (start: UnitPoint, end: UnitPoint) {
        let r = Double(aspectRatio)
        let s = Double(scale)
        let dx = cos(gradientAngle)
        let dy = -sin(gradientAngle) / r
        let dLenSq = dx * dx + dy * dy
        let projections = [
            -0.5 * dx + (-0.5) * dy,
             0.5 * dx + (-0.5) * dy,
            -0.5 * dx + 0.5 * dy,
             0.5 * dx + 0.5 * dy
        ]
        let tMin = projections.min()!
        let tMax = projections.max()!
        let startX = 0.5 + tMin * dx / dLenSq
        let startY = 0.5 + tMin * dy / dLenSq
        let endX = 0.5 + tMax * dx / dLenSq
        let endY = 0.5 + tMax * dy / dLenSq
        return (
            start: .init(x: 0.5 + (startX - 0.5) * s, y: 0.5 + (startY - 0.5) * s),
            end: .init(x: 0.5 + (endX - 0.5) * s, y: 0.5 + (endY - 0.5) * s)
        )
    }

    var body: some View {
        Button(action: action) {
            GeometryReader { geo in
                let labelHeight = geo.size.width * labelHeightRatio
                let imageHeight = max(geo.size.height - labelHeight, 1)
                let imageAspect = imageHeight / geo.size.width
                let gp = Self.gradientPoints(aspectRatio: imageAspect, scale: colorScheme == .light ? 1.2 : 1.5)
                VStack(spacing: 0) {
                    ZStack {
                        LinearGradient(
                            stops: colorScheme == .light ? Self.lightStops : Self.darkStops,
                            startPoint: gp.start,
                            endPoint: gp.end
                        )
                        #if SIMPLEX_ASSETS
                        Image(colorScheme == .light ? imageName : "\(imageName)-light")
                            .resizable()
                            .scaledToFit()
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                            .clipped()
                        #else
                        Image(systemName: icon)
                            .font(.system(size: imageHeight * 0.4))
                            .foregroundColor(theme.colors.primary)
                        #endif
                    }
                    .frame(height: imageHeight)

                    labelRow(height: labelHeight)
                }
            }
            .clipShape(RoundedRectangle(cornerRadius: 24))
        }
        .buttonStyle(.plain)
    }

    private func labelRow(height: CGFloat) -> some View {
        VStack {
            HStack {
                Image(systemName: icon)
                    .font(.system(size: 24))
                    .foregroundColor(theme.colors.primary)
                Text(title)
                    .font(.body)
                    .fontWeight(.medium)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)
                    .minimumScaleFactor(0.75)
            }
            if let subtitle {
                Text(subtitle)
                    .font(.footnote)
                    .foregroundColor(theme.colors.onBackground.opacity(0.7))
            }
        }
        .frame(height: height)
        .frame(maxWidth: .infinity, alignment: .center)
        .padding(.horizontal, 16)
        .background(ToolbarMaterial.material(toolbarMaterial))
    }
}

// MARK: - Onboarding pager

private let backButtonHeight: CGFloat = 44

struct ConnectOnboardingView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.verticalSizeClass) private var verticalSizeClass
    @State private var currentPage = 0
    @State private var showConnectViaLink = false
    @State private var showInviteSomeone = false
    @State private var showCreateAddress = false

    var body: some View {
        TabView(selection: $currentPage) {
            talkToSomeonePage.tag(0)
            connectWithSomeonePage.tag(1)
        }
        .tabViewStyle(.page(indexDisplayMode: .never))
        .sheet(isPresented: $showConnectViaLink) {
            NavigationView {
                NewChatView(selection: .connect, showQRCodeScanner: true, onboarding: true)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
        .sheet(isPresented: $showInviteSomeone) {
            NavigationView {
                NewChatView(selection: .invite, onboarding: true)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
        .sheet(isPresented: $showCreateAddress) {
            NavigationView {
                UserAddressView(autoCreate: true, onboarding: true)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
    }

    @ViewBuilder
    private func cardPair<C1: View, C2: View>(
        _ geo: GeometryProxy,
        @ViewBuilder card1: () -> C1,
        @ViewBuilder card2: () -> C2
    ) -> some View {
        let padding: CGFloat = 16
        let spacing: CGFloat = 16
        let isLandscape = verticalSizeClass == .compact
        let cardWidth = isLandscape
            ? (geo.size.width - padding * 2 - spacing) / 2
            : geo.size.width - padding * 2
        let maxCardHeight = cardWidth * 0.75

        if isLandscape {
            HStack(spacing: spacing) {
                card1().frame(maxHeight: maxCardHeight)
                card2().frame(maxHeight: maxCardHeight)
            }
            .padding(.horizontal, padding)
        } else {
            VStack(spacing: spacing) {
                card1().frame(maxHeight: maxCardHeight)
                card2().frame(maxHeight: maxCardHeight)
            }
            .padding(.horizontal, padding)
        }
    }

    // MARK: Screen 1

    @ViewBuilder
    private func pageHeader(_ title: LocalizedStringKey, showBack: Bool) -> some View {
        let isLandscape = verticalSizeClass == .compact
        let titleView = Text(title)
            .font(.largeTitle)
            .bold()
            .lineLimit(1)
            .minimumScaleFactor(0.75)
            .frame(maxWidth: .infinity, alignment: .center)
        if isLandscape {
            ZStack(alignment: .leading) {
                if showBack { backButton }
                titleView
            }
            .padding(.horizontal, 16)
        } else {
            VStack(spacing: 0) {
                if showBack {
                    backButton.frame(maxWidth: .infinity, alignment: .leading)
                } else {
                    Color.clear.frame(height: backButtonHeight)
                }
                titleView
            }
            .padding(.horizontal, 16)
        }
    }

    private var backButton: some View {
        Button {
            withAnimation { currentPage = 0 }
        } label: {
            HStack(spacing: 4) {
                Image(systemName: "chevron.left")
                Text("Back")
            }
        }
        .frame(height: backButtonHeight)
    }

    // MARK: Screen 1

    private var talkToSomeonePage: some View {
        GeometryReader { geo in
            VStack(spacing: 0) {
                pageHeader("Talk to someone", showBack: false)

                Spacer(minLength: 16)

                cardPair(geo) {
                    OnboardingCardView(
                        imageName: "card-let-someone-connect-to-you-alpha",
                        icon: "link.badge.plus",
                        title: "Let someone connect to you",
                        labelHeightRatio: 0.132,
                        action: { withAnimation { currentPage = 1 } }
                    )
                } card2: {
                    OnboardingCardView(
                        imageName: "card-connect-via-link-alpha",
                        icon: "qrcode.viewfinder",
                        title: "Connect via link or QR code",
                        labelHeightRatio: 0.132,
                        action: { showConnectViaLink = true }
                    )
                }

                Spacer(minLength: 16)
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
    }

    // MARK: Screen 2

    private var connectWithSomeonePage: some View {
        GeometryReader { geo in
            VStack(spacing: 0) {
                pageHeader("Connect with someone", showBack: true)

                Spacer(minLength: 16)

                cardPair(geo) {
                    OnboardingCardView(
                        imageName: "card-invite-someone-privately-alpha",
                        icon: "link.badge.plus",
                        title: "Invite someone privately",
                        subtitle: "A link for one person to connect",
                        labelHeightRatio: 0.195,
                        action: { showInviteSomeone = true }
                    )
                } card2: {
                    OnboardingCardView(
                        imageName: "card-create-your-public-address-alpha",
                        icon: "qrcode",
                        title: "Create your public address",
                        subtitle: "For anyone to reach you",
                        labelHeightRatio: 0.195,
                        action: { showCreateAddress = true }
                    )
                }

                Spacer(minLength: 16)
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
    }
}
