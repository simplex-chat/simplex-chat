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
    let imageName: String
    let icon: String
    let title: LocalizedStringKey
    var subtitle: LocalizedStringKey? = nil
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

    var body: some View {
        Button(action: action) {
            ZStack(alignment: .bottom) {
                LinearGradient(
                    stops: colorScheme == .light ? Self.lightStops : Self.darkStops,
                    startPoint: .init(x: 0.0, y: 0.6),
                    endPoint: .init(x: 1.0, y: 0.4)
                )

                VStack(spacing: 0) {
                    #if SIMPLEX_ASSETS
                    Image(colorScheme == .light ? imageName : "\(imageName)-light")
                        .resizable()
                        .scaledToFit()
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                        .clipped()
                    #else
                    Spacer()
                    #endif

                    labelRow
                }
            }
            .clipShape(RoundedRectangle(cornerRadius: 18))
        }
        .buttonStyle(.plain)
    }

    private var labelRow: some View {
        HStack(spacing: 8) {
            Image(systemName: icon)
                .font(.system(size: 20))
                .foregroundColor(labelColor)
            VStack(alignment: .leading, spacing: 2) {
                Text(title)
                    .font(.body.weight(.semibold))
                    .foregroundColor(labelColor)
                if let subtitle {
                    Text(subtitle)
                        .font(.footnote)
                        .foregroundColor(.secondary)
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(.horizontal, 16)
        .padding(.vertical, 12)
    }

    private var labelColor: Color {
        colorScheme == .dark ? .white : .primary
    }
}

// MARK: - Screen 1

struct TalkToSomeoneView: View {
    @EnvironmentObject var theme: AppTheme
    @State private var showConnectWithSomeone = false
    @State private var showConnectViaLink = false

    var body: some View {
        VStack(spacing: 16) {
            Text("Talk to someone")
                .font(.largeTitle)
                .fontWeight(.bold)
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(.horizontal, 16)

            OnboardingCardView(
                imageName: "card-let-someone-connect-to-you-alpha",
                icon: "link",
                title: "Let someone connect to you",
                action: { showConnectWithSomeone = true }
            )
            .frame(maxHeight: .infinity)
            .padding(.horizontal, 16)

            OnboardingCardView(
                imageName: "card-connect-via-link-alpha",
                icon: "qrcode",
                title: "Connect via link or QR code",
                action: { showConnectViaLink = true }
            )
            .frame(maxHeight: .infinity)
            .padding(.horizontal, 16)
        }
        .padding(.vertical, 16)
        .background(
            NavigationLink(isActive: $showConnectWithSomeone) {
                ConnectWithSomeoneView()
                    .modifier(ThemedBackground(grouped: true))
                    .navigationBarTitleDisplayMode(.inline)
            } label: { EmptyView() }
        )
        .background(
            NavigationLink(isActive: $showConnectViaLink) {
                NewChatView(selection: .connect, showQRCodeScanner: true)
                    .modifier(ThemedBackground(grouped: true))
                    .navigationBarTitleDisplayMode(.inline)
            } label: { EmptyView() }
        )
    }
}

// MARK: - Screen 2

struct ConnectWithSomeoneView: View {
    @EnvironmentObject var theme: AppTheme
    @State private var showInviteSomeone = false
    @State private var showCreateAddress = false

    var body: some View {
        VStack(spacing: 16) {
            Text("Connect with someone")
                .font(.largeTitle)
                .fontWeight(.bold)
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(.horizontal, 16)

            OnboardingCardView(
                imageName: "card-invite-someone-privately-alpha",
                icon: "link",
                title: "Invite someone privately",
                subtitle: "A link for one person to connect",
                action: { showInviteSomeone = true }
            )
            .frame(maxHeight: .infinity)
            .padding(.horizontal, 16)

            OnboardingCardView(
                imageName: "card-create-your-public-address-alpha",
                icon: "qrcode",
                title: "Create your public address",
                subtitle: "For anyone to reach you",
                action: { showCreateAddress = true }
            )
            .frame(maxHeight: .infinity)
            .padding(.horizontal, 16)
        }
        .padding(.vertical, 16)
        .background(
            NavigationLink(isActive: $showInviteSomeone) {
                NewChatView(selection: .invite)
                    .modifier(ThemedBackground(grouped: true))
                    .navigationBarTitleDisplayMode(.inline)
            } label: { EmptyView() }
        )
        .background(
            NavigationLink(isActive: $showCreateAddress) {
                UserAddressView(autoCreate: true)
                    .modifier(ThemedBackground(grouped: true))
                    .navigationBarTitleDisplayMode(.inline)
            } label: { EmptyView() }
        )
        .modifier(ThemedBackground(grouped: true))
    }
}
