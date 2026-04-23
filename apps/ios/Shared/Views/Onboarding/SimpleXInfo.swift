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
    @State private var showHowItWorks = false
    @State private var createProfileNavLinkActive = false
    var onboarding: Bool

    var body: some View {
        GeometryReader { g in
            let v = ScrollView {
                VStack(alignment: .center, spacing: 10) {
                    Image(colorScheme == .light ? "logo" : "logo-light")
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(width: g.size.width * 0.67)
                        .padding(.leading, 4)
                        .frame(maxWidth: .infinity, minHeight: 48, alignment: .top)

                    #if SIMPLEX_ASSETS
                    Image(colorScheme == .light ? "intro" : "intro-light")
                        .resizable()
                        .scaledToFit()
                        .frame(maxWidth: .infinity)
                    #else
                    Image(systemName: "bubble.left.and.bubble.right")
                        .font(.system(size: 80))
                        .foregroundColor(theme.colors.primary)
                        .padding(.vertical, 40)
                        .frame(maxWidth: .infinity)
                    #endif

                    Text("Be free\nin your network")
                        .font(.largeTitle)
                        .bold()
                        .multilineTextAlignment(.center)

                    Text("Private and secure messaging.")
                        .font(.title2)
                        .multilineTextAlignment(.center)

                    Text("The first network where you own your\ncontacts and groups.")
                        .font(.callout)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)

                    Spacer()

                    if onboarding {
                        VStack(spacing: 10) {
                            createFirstProfileButton()

                            Button {
                                showHowItWorks = true
                            } label: {
                                Label("Why SimpleX is built.", systemImage: "info.circle")
                                    .font(.headline)
                            }
                        }
                    }
                }
                .padding(.horizontal, 25)
                .padding(.top, 75)
                .padding(.bottom, 25)
                .frame(minHeight: g.size.height)
            }
            .sheet(isPresented: Binding(
                get: { m.migrationState != nil },
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
            .sheet(isPresented: $showHowItWorks) {
                HowItWorks(
                    onboarding: onboarding,
                    createProfileNavLinkActive: $createProfileNavLinkActive
                )
            }
            if #available(iOS 16.4, *) {
                v.scrollBounceBehavior(.basedOnSize)
            } else {
                v
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
