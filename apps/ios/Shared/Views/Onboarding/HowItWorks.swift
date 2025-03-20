//
//  HowItWorks.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 08/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct HowItWorks: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var m: ChatModel
    var onboarding: Bool
    @Binding var createProfileNavLinkActive: Bool

    var body: some View {
        VStack(alignment: .leading) {
            Text("How SimpleX works")
                .font(.largeTitle)
                .bold()
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading) {
                    Group {
                        Text("To protect your privacy, SimpleX uses separate IDs for each of your contacts.")
                        Text("Only client devices store user profiles, contacts, groups, and messages.")
                        Text("All messages and files are sent **end-to-end encrypted**, with post-quantum security in direct messages.")
                        if !onboarding {
                            Text("Read more in our [GitHub repository](https://github.com/simplex-chat/simplex-chat#readme).")
                        }
                    }
                    .padding(.bottom)
                }
            }

            Spacer()

            if onboarding {
                VStack(spacing: 10) {
                    createFirstProfileButton()
                    onboardingButtonPlaceholder()
                }
            }
        }
        .lineLimit(10)
        .padding(onboarding ? 25 : 16)
        .frame(maxHeight: .infinity, alignment: .top)
        .modifier(ThemedBackground())
    }

    private func createFirstProfileButton() -> some View {
        Button {
            dismiss()
            createProfileNavLinkActive = true
        } label: {
            Text("Create your profile")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
    }
}

struct HowItWorks_Previews: PreviewProvider {
    static var previews: some View {
        HowItWorks(
            onboarding: true,
            createProfileNavLinkActive: Binding.constant(false)
        )
    }
}
