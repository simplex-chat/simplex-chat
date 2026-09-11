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
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject private var badgeModel = BadgeModel.shared
    // set true when presented as a sheet root (from the chat-list banner) — that path doesn't
    // reserve nav-bar space like a NavigationLink push does, so the title lands too close to the top
    var showsAsSheet: Bool = false
    @State private var whyBuiltActive = false
    @State private var howItWorksActive = false
    @State private var redeemCodeActive = false
    @State private var badgeStateUnavailable = false

    private enum Screen {
        case loading
        case support
        case badge(BadgeState)
    }

    private var screen: Screen {
        guard let userId = chatModel.currentUser?.userId else { return .loading }
        // a read that failed must not reach the model, where it would be indistinguishable from
        // this profile having no badge, so it is held here and falls back to the support screen
        if badgeStateUnavailable { return .support }
        guard badgeModel.userId == userId else { return .loading }
        if let badgeState = badgeModel.badgeState, badgeState.shown { return .badge(badgeState) }
        return .support
    }

    var body: some View {
        Group {
            switch screen {
            case .loading:
                ProgressView()
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            case .support:
                supportSimpleX
            case let .badge(badgeState):
                BadgesYourBadgeView(badgeState: badgeState, showsAsSheet: showsAsSheet)
            }
        }
        .task(id: chatModel.currentUser?.userId) { await loadBadgeState() }
    }

    private func loadBadgeState() async {
        guard let user = chatModel.currentUser else { return }
        await MainActor.run { badgeStateUnavailable = false }
        do {
            let badgeState = try await apiGetBadgeState(user.userId)
            // switching profile cancels this task, and an answer that arrives after it must not
            // land on the profile that replaced it - it would leave the screen loading for good
            guard !Task.isCancelled else { return }
            await MainActor.run { BadgeModel.shared.set(userId: user.userId, badgeState: badgeState) }
        } catch let error {
            guard !Task.isCancelled else { return }
            logger.error("apiGetBadgeState: \(responseError(error))")
            await MainActor.run { badgeStateUnavailable = true }
        }
    }

    private var supportSimpleX: some View {
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

                // TODO [badges] restore whyBuiltButton() when in-app purchase lands: the level screen
                // returns to the flow and howItWorksButton() moves there, leaving this one alone here.
                howItWorksButton()

                Spacer(minLength: 0)

                PhoneSupporterHero()
                    .frame(maxWidth: g.size.width * 0.55)
                    .layoutPriority(-1)

                Spacer(minLength: 0)

                VStack(spacing: 10) {
                    redeemCodeButton()
                        .padding(.vertical, 10)
                    getCodeButton()
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

    private func howItWorksButton() -> some View {
        ZStack {
            Button { howItWorksActive = true } label: {
                HStack(spacing: 4) {
                    Image(systemName: "info.circle")
                    Text("How private badges work").fontWeight(.medium)
                }
                .font(.body)
            }
            NavigationLink(isActive: $howItWorksActive) {
                BadgesHowItWorksView()
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
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: false))

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

    private func getCodeButton() -> some View {
        Button {
            openExternalLink(URL(string: "https://simplex.chat/badges/")!)
        } label: {
            Text("Get your code")
                .font(.body)
                .fontWeight(.medium)
                .foregroundColor(theme.colors.primary)
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
