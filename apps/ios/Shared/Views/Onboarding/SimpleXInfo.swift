//
//  SimpleXInfo.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SimpleXInfo: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @State private var showHowItWorks = false
    @State private var createProfileNavLinkActive = false
    var onboarding: Bool

    var body: some View {
        GeometryReader { g in
            let v = ScrollView {
                VStack(alignment: .leading) {
                    VStack(alignment: .center, spacing: 10) {
                        Image(colorScheme == .light ? "logo" : "logo-light")
                            .resizable()
                            .aspectRatio(contentMode: .fit)
                            .frame(width: g.size.width * 0.67)
                            .padding(.bottom, 8)
                            .padding(.leading, 4)
                            .frame(maxWidth: .infinity, minHeight: 48, alignment: .top)
                        
                        Button {
                            showHowItWorks = true
                        } label: {
                            Label("The future of messaging", systemImage: "info.circle")
                                .font(.headline)
                        }
                    }

                    Spacer()

                    VStack(alignment: .leading) {
                        onboardingInfoRow("privacy", "Privacy redefined",
                                "No user identifiers.", width: 48)
                        onboardingInfoRow("shield", "Immune to spam",
                                "You decide who can connect.", width: 46)
                        onboardingInfoRow(colorScheme == .light ? "decentralized" : "decentralized-light", "Decentralized",
                                "Anybody can host servers.", width: 46)
                    }
                    .padding(.leading, 16)

                    Spacer()

                    if onboarding {
                        VStack(spacing: 10) {
                            createFirstProfileButton()

                            Button {
                                m.migrationState = .pasteOrScanLink
                            } label: {
                                Label("Migrate from another device", systemImage: "tray.and.arrow.down")
                                    .font(.system(size: 17, weight: .semibold))
                                    .frame(minHeight: 40)
                            }
                            .frame(maxWidth: .infinity)
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

    private func onboardingInfoRow(_ image: String, _ title: LocalizedStringKey, _ text: LocalizedStringKey, width: CGFloat) -> some View {
        HStack(alignment: .top) {
            Image(image)
                .resizable()
                .scaledToFit()
                .frame(width: width, height: 54)
                .frame(width: 54)
                .padding(.trailing, 10)
            VStack(alignment: .leading, spacing: 4) {
                Text(title).font(.headline)
                Text(text).frame(minHeight: 40, alignment: .top)
                    .font(.callout)
                    .lineLimit(3)
                    .fixedSize(horizontal: false, vertical: true)
            }
            .padding(.top, 4)
        }
        .padding(.bottom, 12)
    }

    private func createFirstProfileButton() -> some View {
        ZStack {
            Button {
                createProfileNavLinkActive = true
            } label: {
                Text("Create your profile")
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
