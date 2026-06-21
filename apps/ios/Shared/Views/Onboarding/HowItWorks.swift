//
//  HowItWorks.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 08/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/navigation.md

import SwiftUI

struct OldHowItWorks: View {
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
                            ExternalLink("Read more in our GitHub repository.", destination: URL(string: "https://github.com/simplex-chat/simplex-chat#readme")!)
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

struct WhySimpleX: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var m: ChatModel
    var onboarding: Bool
    @Binding var createProfileNavLinkActive: Bool

    var body: some View {
        VStack(alignment: .leading) {
            ScrollView {
                VStack(alignment: .leading, spacing: 16) {
                    Text("You were born without an account")
                        .font(.title)
                        .bold()
                        .padding(.top)
                    Text("Nobody tracked your conversations. No one drew a map of where you'd been. Privacy was never a feature - it was the way of life.")
                    Text("Then we moved online, and every platform asked for a piece of you - your name, your number, your friends. We accepted that the price of talking to others is letting someone know who we talk to. Every generation, people and tech, had it this way - telephone, email, messengers, social media. It seemed the only way possible.")
                    Text("There is another way. A network with no phone numbers. No usernames. No accounts. No user identities of any kind. A network that connects people and carries encrypted messages without knowing who is connected.")
                    Text("Not a better lock on someone else's door. Not a nicer landlord that respects your privacy, but still keeps the record of all visitors. You are not a guest. You are home. No king can enter it - you are sovereign.")
                    Text("Your conversations belong to you, as it had always been before the Internet. The network is not a place you visit. It is a place you create and own. And nobody can take it from you, whether you make it private or public.")
                    Text("The oldest human freedom - to speak to another person without being watched - built on infrastructure that cannot betray it.")
                    Text("Because we destroyed the power to know who you are. So that your power can never be taken.")
                    Text("Be free in your network.")
                }
            }
            .padding(.bottom, 16)

            Spacer()

            if onboarding {
                createFirstProfileButton()
            }
        }
        .padding(onboarding ? 25 : 16)
        .frame(maxHeight: .infinity, alignment: .top)
        .modifier(ThemedBackground())
    }

    private func createFirstProfileButton() -> some View {
        Button {
            dismiss()
            createProfileNavLinkActive = true
        } label: {
            Text("Get started")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
    }
}

struct WhySimpleX_Previews: PreviewProvider {
    static var previews: some View {
        WhySimpleX(
            onboarding: true,
            createProfileNavLinkActive: Binding.constant(false)
        )
    }
}
