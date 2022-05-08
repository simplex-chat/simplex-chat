//
//  SimpleXInfo.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SimpleXInfo: View {
    @EnvironmentObject var m: ChatModel
    @State private var showHowItWorks = false
    var onboarding: Bool

    var body: some View {
        GeometryReader { g in
            VStack(alignment: .leading) {
                Image("logo")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: g.size.width * 0.7)
                    .padding(.bottom)
                Text("The next generation of private messaging")
                    .font(.title)
                    .padding(.bottom)

                infoRow("🎭", "Privacy redefined",
                        "The 1st platform without any user identifiers – private by design.")
                infoRow("📭", "Immune to spam and abuse",
                        "People can connect to you only via the links you share.")
                infoRow("🤝", "Decentralized",
                        "Open-source protocol – anybody can run the servers.")

                Spacer()

                if onboarding {
                    OnboardingActionButton()
                    Spacer()
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
            .sheet(isPresented: $showHowItWorks) {
                HowItWorks(onboarding: onboarding)
            }
        }
        .padding()
    }

    private func infoRow(_ emoji: String, _ title: LocalizedStringKey, _ text: LocalizedStringKey) -> some View {
        HStack(alignment: .top) {
            Text(emoji)
                .font(mediumEmojiFont)
                .frame(width: 40)
            VStack(alignment: .leading) {
                Text(title).font(.headline)
                Text(text)
            }
        }
        .padding(.bottom)
    }
}

struct OnboardingActionButton: View {
    @EnvironmentObject var m: ChatModel

    var body: some View {
        if m.currentUser == nil {
            actionButton("Create your profile", onboarding: .step2_CreateProfile)
        } else {
            actionButton("Make a private connection", onboarding: .step3_MakeConnection)
        }
    }

    private func actionButton(_ label: LocalizedStringKey, onboarding: OnboardingStage) -> some View {
        Button {
            withAnimation {
                m.onboardingStage = onboarding
            }
        } label: {
            HStack {
                Text(label).font(.title2)
                Image(systemName: "greaterthan")
            }
        }
        .frame(maxWidth: .infinity)
    }
}

struct SimpleXInfo_Previews: PreviewProvider {
    static var previews: some View {
        SimpleXInfo(onboarding: true)
    }
}
