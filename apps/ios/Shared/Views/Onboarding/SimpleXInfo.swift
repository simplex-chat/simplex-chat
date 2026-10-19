//
//  SimpleXInfo.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/navigation.md

import SwiftUI
import SimpleXChat

struct SimpleXInfo: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @State private var showWhyBuilt = false
    @State private var createProfileNavLinkActive = false
    var onboarding: Bool

    var body: some View {
        GeometryReader { g in
            VStack(alignment: .center, spacing: 10) {
                Image(colorScheme == .light ? "logo" : "logo-light")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: (g.size.width - 50) * 0.55)
                    .padding(.leading, 4)
                    .frame(maxWidth: .infinity, minHeight: 48, alignment: .top)

                #if SIMPLEX_ASSETS
                Image(colorScheme == .light ? "intro" : "intro-light")
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
                    Image(systemName: "bubble.left.and.bubble.right")
                        .font(.system(size: 72))
                        .foregroundColor(theme.colors.primary)
                }
                .aspectRatio(1.0, contentMode: .fit)
                .clipShape(RoundedRectangle(cornerRadius: 24))
                .padding(.horizontal, 25)
                .frame(maxWidth: .infinity)
                #endif

                Text("Be free\nin your network")
                    .font(.largeTitle)
                    .bold()
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)

                Text("Private and secure messaging.")
                    .font(.title3)
                    .fontWeight(.medium)
                    .foregroundColor(theme.colors.secondary)
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)

                Text("The first network where you own\nyour contacts and groups.")
                    .font(.footnote)
                    .foregroundColor(theme.colors.secondary)
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)

                if onboarding {
                    Spacer(minLength: 0)

                    createFirstProfileButton()
                        .padding(.vertical, 10)

                    Button {
                        showWhyBuilt = true
                    } label: {
                        Label("Why SimpleX is built.", systemImage: "info.circle")
                            .font(.headline)
                    }
                }
            }
            .padding(.horizontal, 25)
            .padding(.top, 28)
            .padding(.bottom, 20)
            .frame(minHeight: g.size.height)
            .sheet(isPresented: Binding(
                get: { m.migrationState != nil && !createProfileNavLinkActive },
                set: { _ in
                    m.migrationState = nil
                    MigrationToDeviceState.save(nil) }
            )) {
                NavigationView {
                    VStack(alignment: .leading) {
                        MigrateToDevice(migrationState: $m.migrationState)
                    }
                    .navigationTitle("Migrate here")
                    .modifier(ThemedBackground(grouped: true))
                }
            }
            .sheet(isPresented: $showWhyBuilt) {
                WhySimpleX(
                    onboarding: onboarding,
                    createProfileNavLinkActive: $createProfileNavLinkActive
                )
            }
        }
        .onAppear() {
            setLastVersionDefault()
        }
        .frame(maxHeight: .infinity)
        .navigationBarHidden(true) // necessary on iOS 15
    }

    private func createFirstProfileButton() -> some View {
        ZStack {
            Button {
                createProfileNavLinkActive = true
            } label: {
                Text("Get started")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: false))

            NavigationLink(isActive: $createProfileNavLinkActive) {
                CreateFirstProfile()
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }
}

let textSpace = Text(verbatim: " ")

let textNewLine = Text(verbatim: "\n")

struct SimpleXInfo_Previews: PreviewProvider {
    static var previews: some View {
        SimpleXInfo(onboarding: true)
    }
}
