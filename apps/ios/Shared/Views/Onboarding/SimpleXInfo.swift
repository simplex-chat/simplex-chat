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
        NavigationView {
            GeometryReader { g in
                ScrollView {
                    VStack(alignment: .leading, spacing: 20) {
                        Image(colorScheme == .light ? "logo" : "logo-light")
                            .resizable()
                            .aspectRatio(contentMode: .fit)
                            .frame(width: g.size.width * 0.67)
                            .padding(.bottom, 8)
                            .frame(maxWidth: .infinity, minHeight: 48, alignment: .top)

                        VStack(alignment: .leading) {
                            Text("The next generation of private messaging")
                                .font(.title2)
                                .padding(.bottom, 30)
                                .padding(.horizontal, 40)
                                .frame(maxWidth: .infinity)
                                .multilineTextAlignment(.center)
                            infoRow("privacy", "Privacy redefined",
                                    "The 1st platform without any user identifiers – private by design.", width: 48)
                            infoRow("shield", "Immune to spam and abuse",
                                    "People can connect to you only via the links you share.", width: 46)
                            infoRow(colorScheme == .light ? "decentralized" : "decentralized-light", "Decentralized",
                                    "Open-source protocol and code – anybody can run the servers.", width: 44)
                        }

                        Spacer()

                        if onboarding {
                            onboardingActionButton()

                            Button {
                                m.migrationState = .pasteOrScanLink
                            } label: {
                                Label("Migrate from another device", systemImage: "tray.and.arrow.down")
                                    .font(.subheadline)
                            }
                            .frame(maxWidth: .infinity)
                        }

                        Button {
                            showHowItWorks = true
                        } label: {
                            Label("How it works", systemImage: "info.circle")
                                .font(.subheadline)
                        }
                        .frame(maxWidth: .infinity)
                        .padding(.bottom)
                    }
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
            }
            .frame(maxHeight: .infinity)
            .padding()
        }
    }

    private func infoRow(_ image: String, _ title: LocalizedStringKey, _ text: LocalizedStringKey, width: CGFloat) -> some View {
        HStack(alignment: .top) {
            Image(image)
                .resizable()
                .scaledToFit()
                .frame(width: width, height: 54)
                .frame(width: 54)
                .padding(.top, 4)
                .padding(.leading, 4)
                .padding(.trailing, 10)
            VStack(alignment: .leading, spacing: 4) {
                Text(title).font(.headline)
                Text(text).frame(minHeight: 40, alignment: .top)
            }
        }
        .padding(.bottom, 20)
        .padding(.trailing, 6)
    }

    @ViewBuilder private func onboardingActionButton() -> some View {
        if m.currentUser == nil {
            createFirstProfileButton()
        } else {
            userExistsFallbackButton()
        }
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
                createProfileDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func createProfileDestinationView() -> some View {
        CreateFirstProfile()
            .navigationTitle("Create your profile")
            .navigationBarTitleDisplayMode(.large)
    }

    private func userExistsFallbackButton() -> some View {
        Button {
            withAnimation {
                onboardingStageDefault.set(.onboardingComplete)
                m.onboardingStage = .onboardingComplete
            }
        } label: {
            Text("Make a private connection")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
    }
}

struct SimpleXInfo_Previews: PreviewProvider {
    static var previews: some View {
        SimpleXInfo(onboarding: true)
    }
}
