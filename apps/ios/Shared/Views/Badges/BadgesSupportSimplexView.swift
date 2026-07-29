//
//  BadgesSupportSimplexView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Entry point for badges management. The subsequent screens (level selection, pay, redeem code,
// how it works) are pushed via NavigationLink from this view, so the enclosing NavigationView —
// either the settings NavigationView or the sheet NavigationView presented from the chat list
// banner — provides the sliding animation.
struct BadgesSupportSimplexView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @State private var showWhySimpleX = false
    @State private var chooseLevelActive = false
    @State private var redeemCodeActive = false

    var body: some View {
        // TODO [badges] when the state machine lands, gate on user badge status:
        // - no badge → this view (support prompt)
        // - active badge → a "Manage your badge" view
        //
        // No ScrollView here — matches SimpleXInfo/YourNetworkView: on tight screens the
        // .scaledToFit hero shrinks to fit the vertical space left by the fixed-height title,
        // body, info button and CTAs, instead of pushing content off-screen.
        GeometryReader { g in
            VStack(alignment: .center, spacing: 16) {
                Text("Support SimpleX")
                    .font(.largeTitle)
                    .bold()
                    .foregroundColor(theme.colors.primary)
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)

                Text("SimpleX doesn't sell ads or data. It's funded by its users and by investors who share the mission. You can support the project and show a badge on your profile.")
                    .font(.body)
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)

                Button { showWhySimpleX = true } label: {
                    Label("Why SimpleX is built.", systemImage: "info.circle")
                        .font(.headline)
                }

                PhoneSupporterHero()
                    .frame(maxWidth: g.size.width * 0.55)
                    .layoutPriority(-1)

                Spacer(minLength: 0)

                chooseLevelButton()

                redeemCodeButton()
                    .padding(.top, 4)
                    .padding(.bottom, g.safeAreaInsets.bottom == 0 ? 20 : 0)
            }
            .padding(.horizontal, 25)
            .padding(.top, 8)
            .padding(.bottom, 20)
            // .frame(height:) instead of .frame(minHeight:) — locks the VStack to the
            // proposed geometry. In the onboarding flow (full-screen presentation) minHeight is
            // enough because the parent caps at the screen; inside the banner sheet's NavigationView
            // the parent DOESN'T cap, so a minHeight-only VStack expands past the visible area,
            // which both makes the hero gigantic and lets the sheet interpret the overflow as
            // scroll content. Locking to the geometry keeps the layout inside the visible bounds.
            .frame(height: g.size.height)
        }
        .frame(maxHeight: .infinity)
        .modifier(ThemedBackground())
        .navigationBarHidden(true)
        .sheet(isPresented: $showWhySimpleX) {
            WhySimpleX(onboarding: false, createProfileNavLinkActive: .constant(false))
        }
    }

    private func chooseLevelButton() -> some View {
        ZStack {
            Button {
                chooseLevelActive = true
            } label: {
                Text("Choose your level")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: false))

            NavigationLink(isActive: $chooseLevelActive) {
                BadgesYourLevelView()
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func redeemCodeButton() -> some View {
        ZStack {
            Button {
                redeemCodeActive = true
            } label: {
                Text("Redeem badge code")
                    .font(.body)
                    .fontWeight(.medium)
                    .foregroundColor(theme.colors.primary)
            }

            NavigationLink(isActive: $redeemCodeActive) {
                BadgesRedeemCodeView()
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }
}

// Hero image reused across badges views and the WhatsNewView v7.1 entry. Falls back to a gradient
// card carrying the small supporter badge glyph when SIMPLEX_ASSETS is not defined — matching the
// onboarding placeholder convention.
struct PhoneSupporterHero: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme

    var body: some View {
        #if SIMPLEX_ASSETS
        Image(colorScheme == .light ? "phone-supporter" : "phone-supporter-light")
            .resizable()
            .scaledToFit()
            .frame(maxWidth: .infinity)
        #else
        ZStack {
            let gp = OnboardingCardView.gradientPoints(aspectRatio: 1.0, scale: colorScheme == .light ? 1.2 : 1.5)
            LinearGradient(
                stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
                startPoint: gp.start,
                endPoint: gp.end
            )
            Image("badge-supporter")
                .resizable()
                .scaledToFit()
                .frame(width: 96)
        }
        .aspectRatio(1.0, contentMode: .fit)
        .clipShape(RoundedRectangle(cornerRadius: 24))
        .frame(maxWidth: .infinity)
        #endif
    }
}

struct BadgesSupportSimplexView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesSupportSimplexView()
        }
    }
}
