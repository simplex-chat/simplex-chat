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

    private static let lightStops: [Gradient.Stop] = [
        .init(color: Color(red: 0.824, green: 0.910, blue: 1.0), location: 0.0),
        .init(color: Color(red: 0.800, green: 0.914, blue: 1.0), location: 0.5),
        .init(color: Color(red: 0.875, green: 1.0, blue: 1.0), location: 0.9),
        .init(color: Color(red: 1.0, green: 0.988, blue: 0.918), location: 1.0)
    ]

    private static let darkStops: [Gradient.Stop] = [
        .init(color: Color(red: 0.016, green: 0.039, blue: 0.141), location: 0.4),
        .init(color: Color(red: 0.220, green: 0.329, blue: 0.671), location: 0.72),
        .init(color: Color(red: 0.659, green: 0.929, blue: 0.953), location: 0.9),
        .init(color: Color(red: 1.0, green: 0.965, blue: 0.878), location: 1.0)
    ]

    private static let gradientAngle: Double = 80.0 * .pi / 180.0

    private static func gradientPoints(aspectRatio: CGFloat, scale: CGFloat) -> (start: UnitPoint, end: UnitPoint) {
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
                let imageHeight = geo.size.height - labelHeight
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
        HStack(alignment: subtitle != nil ? .firstTextBaseline : .center, spacing: 8) {
            Image(systemName: icon)
                .font(.system(size: 16))
                .foregroundColor(theme.colors.primary)
            VStack(alignment: .leading, spacing: 2) {
                Text(title)
                    .font(.body)
                    .foregroundColor(labelColor)
                if let subtitle {
                    Text(subtitle)
                        .font(.caption)
                        .foregroundColor(colorScheme == .dark ? .white.opacity(0.7) : .secondary)
                }
            }
        }
        .frame(height: height)
        .frame(maxWidth: .infinity, alignment: .center)
        .padding(.horizontal, 16)
        .background(ToolbarMaterial.material(toolbarMaterial))
    }

    private var labelColor: Color {
        colorScheme == .dark ? .white : .primary
    }
}

// MARK: - Onboarding pager

private let backButtonHeight: CGFloat = 44

struct ConnectOnboardingView: View {
    @EnvironmentObject var theme: AppTheme
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
                NewChatView(selection: .connect, showQRCodeScanner: true)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
        .sheet(isPresented: $showInviteSomeone) {
            NavigationView {
                NewChatView(selection: .invite)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
        .sheet(isPresented: $showCreateAddress) {
            NavigationView {
                UserAddressView(autoCreate: true)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
    }

    // MARK: Screen 1

    private var talkToSomeonePage: some View {
        GeometryReader { geo in
            let cardWidth = geo.size.width - 32
            let maxCardHeight = cardWidth * 0.75
            VStack(spacing: 0) {
                Color.clear.frame(height: backButtonHeight)

                Text("Talk to someone")
                    .font(.largeTitle)
                    .lineLimit(1)
                    .minimumScaleFactor(0.8)
                    .frame(maxWidth: .infinity, alignment: .center)
                    .padding(.horizontal, 16)

                Spacer()

                OnboardingCardView(
                    imageName: "card-let-someone-connect-to-you-alpha",
                    icon: "link",
                    title: "Let someone connect to you",
                    labelHeightRatio: 0.132,
                    action: { withAnimation { currentPage = 1 } }
                )
                .frame(maxHeight: maxCardHeight)
                .padding(.horizontal, 16)

                Spacer().frame(maxHeight: 16)

                OnboardingCardView(
                    imageName: "card-connect-via-link-alpha",
                    icon: "qrcode",
                    title: "Connect via link or QR code",
                    labelHeightRatio: 0.132,
                    action: { showConnectViaLink = true }
                )
                .frame(maxHeight: maxCardHeight)
                .padding(.horizontal, 16)

                Spacer()
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
    }

    // MARK: Screen 2

    private var connectWithSomeonePage: some View {
        GeometryReader { geo in
            let cardWidth = geo.size.width - 32
            let maxCardHeight = cardWidth * 0.75
            VStack(spacing: 0) {
                HStack {
                    Button {
                        withAnimation { currentPage = 0 }
                    } label: {
                        HStack(spacing: 4) {
                            Image(systemName: "chevron.left")
                            Text("Back")
                        }
                    }
                    Spacer()
                }
                .frame(height: backButtonHeight)
                .padding(.horizontal, 16)

                Text("Connect with someone")
                    .font(.largeTitle)
                    .lineLimit(1)
                    .minimumScaleFactor(0.8)
                    .frame(maxWidth: .infinity, alignment: .center)
                    .padding(.horizontal, 16)

                Spacer()

                OnboardingCardView(
                    imageName: "card-invite-someone-privately-alpha",
                    icon: "link",
                    title: "Invite someone privately",
                    subtitle: "A link for one person to connect",
                    labelHeightRatio: 0.195,
                    action: { showInviteSomeone = true }
                )
                .frame(maxHeight: maxCardHeight)
                .padding(.horizontal, 16)

                Spacer().frame(maxHeight: 16)

                OnboardingCardView(
                    imageName: "card-create-your-public-address-alpha",
                    icon: "qrcode",
                    title: "Create your public address",
                    subtitle: "For anyone to reach you",
                    labelHeightRatio: 0.195,
                    action: { showCreateAddress = true }
                )
                .frame(maxHeight: maxCardHeight)
                .padding(.horizontal, 16)

                Spacer()
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
    }
}
