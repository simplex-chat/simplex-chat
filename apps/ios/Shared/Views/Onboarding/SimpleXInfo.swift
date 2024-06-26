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
    var onboarding: Bool

    var body: some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .leading) {
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
                        OnboardingActionButton()
                        Spacer()

                        Button {
                            m.migrationState = .pasteOrScanLink
                        } label: {
                            Label("Migrate from another device", systemImage: "tray.and.arrow.down")
                                .font(.subheadline)
                        }
                        .padding(.bottom, 8)
                        .frame(maxWidth: .infinity)
                    }

                    Button {
                        showHowItWorks = true
                    } label: {
                        Label("How it works", systemImage: "info.circle")
                            .font(.subheadline)
                    }
                    .padding(.bottom, 8)
                    .frame(maxWidth: .infinity)

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
                HowItWorks(onboarding: onboarding)
            }
        }
        .frame(maxHeight: .infinity)
        .padding()
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
}

struct OnboardingActionButton: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme

    var body: some View {
        if m.currentUser == nil {
            actionButton("Create your profile", onboarding: .step2_CreateProfile)
        } else {
            actionButton("Make a private connection", onboarding: .onboardingComplete)
        }
    }

    private func actionButton(_ label: LocalizedStringKey, onboarding: OnboardingStage) -> some View {
        Button {
            withAnimation {
                onboardingStageDefault.set(onboarding)
                m.onboardingStage = onboarding
            }
        } label: {
            HStack {
                Text(label).font(.title2)
                Image(systemName: "greaterthan")
            }
        }
        .frame(maxWidth: .infinity)
        .padding(.bottom)
    }

    private func actionButton(_ label: LocalizedStringKey, action: @escaping () -> Void) -> some View {
        Button {
            withAnimation {
                action()
            }
        } label: {
            HStack {
                Text(label).font(.title2)
                Image(systemName: "greaterthan")
            }
        }
        .frame(maxWidth: .infinity)
        .padding(.bottom)
    }
}

struct SimpleXInfo_Previews: PreviewProvider {
    static var previews: some View {
        SimpleXInfo(onboarding: true)
    }
}
