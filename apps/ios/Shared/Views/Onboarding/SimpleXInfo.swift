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
    var onboarding: Bool

    var body: some View {
        GeometryReader { g in
            ScrollView {
                HStack {
                    Spacer()
                    InfoSheetButton {
                        HowItWorks(onboarding: onboarding)
                    }
                }
                .padding(.bottom, 40)
                VStack(alignment: .leading) {
                    Image(colorScheme == .light ? "logo" : "logo-light")
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(width: g.size.width * 0.67)
                        .padding(.bottom, 40)
                        .frame(maxWidth: .infinity, minHeight: 48, alignment: .top)

                    VStack(alignment: .leading) {
                        Text("The next generation of private messaging")
                            .font(.title3)
                            .padding(.bottom, 30)
                            .padding(.horizontal, 40)
                            .frame(maxWidth: .infinity)
                            .multilineTextAlignment(.center)
                        VStack(alignment: .leading) {
                            infoRow("privacy", "Privacy redefined",
                                    "No user identifiers.", width: 48)
                            infoRow("shield", "Immune to spam",
                                    "You decide who can connect.", width: 46)
                            infoRow(colorScheme == .light ? "decentralized" : "decentralized-light", "Decentralized",
                                    "Anybody can host servers.", width: 44)
                        }
                        .frame(maxWidth: .infinity)
                    }

                    if onboarding {
                        VStack {
                            OnboardingActionButton()
                            Button {
                                m.migrationState = .pasteOrScanLink
                            } label: {
                                Label("Migrate from another device", systemImage: "tray.and.arrow.down")
                                    .font(.subheadline)
                            }
                            .frame(maxWidth: .infinity)
                        }
                        .padding(.top, 40)
                    }
                }
                //.frame(minHeight: g.size.height)
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
                //.padding(.top, 4)
                //.padding(.leading, 50)
                .padding(.trailing, 4)
            VStack(alignment: .leading, spacing: 8) {
                Text(title).font(.headline)
                Text(text).font(.subheadline).frame(alignment: .top)
            }
            .padding(4)
        }
        .padding(.bottom, 20)
        .padding(.trailing, 6)
    }
}

struct OnboardingActionButton: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme

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
                Text(label)
                    .font(.headline)
                    .foregroundColor(theme.colors.background)
                    .padding(.vertical, 20)
                    .padding(.horizontal, 80)
            }
        }
        .background(theme.colors.primary)
        .clipShape(RoundedRectangle(cornerRadius: 50))
        .frame(maxWidth: .infinity)
        .padding(.bottom, 20)
        .padding(.horizontal)
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
