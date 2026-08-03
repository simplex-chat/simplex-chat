//
//  BadgesSupportSimplexView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct BadgesSupportSimplexView: View {
    @EnvironmentObject var theme: AppTheme
    // set true when presented as a sheet root (from the chat-list banner) — that path doesn't
    // reserve nav-bar space like a NavigationLink push does, so the title lands too close to the top
    var showsAsSheet: Bool = false
    @State private var whyBuiltActive = false
    @State private var chooseLevelActive = false
    @State private var redeemCodeActive = false

    var body: some View {
        // TODO [badges] gate on user badge status (no badge → this view, active → "Manage your badge")
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

                whyBuiltButton()

                Spacer(minLength: 0)

                PhoneSupporterHero()
                    .frame(maxWidth: g.size.width * 0.55)
                    .layoutPriority(-1)

                Spacer(minLength: 0)

                // Onboarding pattern: nested VStack(spacing: 10) + action button vertical padding 10.
                VStack(spacing: 10) {
                    chooseLevelButton()
                        .padding(.vertical, 10)
                    redeemCodeButton()
                        .frame(height: 22)
                }
                .padding(.bottom, g.safeAreaInsets.bottom == 0 ? 20 : 0)
            }
            .padding(.horizontal, 25)
            .padding(.top, showsAsSheet ? 48 : 0)
            .padding(.bottom, 20)
            // .frame(height:) not minHeight — inside the banner sheet's NavigationView minHeight
            // would let the VStack expand past the visible area and inflate the hero.
            .frame(height: g.size.height)
        }
        .frame(maxHeight: .infinity)
        .navigationBarTitleDisplayMode(.inline)
    }

    private func whyBuiltButton() -> some View {
        ZStack {
            Button { whyBuiltActive = true } label: {
                HStack(spacing: 4) {
                    Image(systemName: "info.circle")
                    Text("Why SimpleX is built.").fontWeight(.medium)
                }
                .font(.body)
            }
            NavigationLink(isActive: $whyBuiltActive) {
                WhySimpleX(onboarding: false, titleColor: theme.colors.primary, createProfileNavLinkActive: .constant(false))
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
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
