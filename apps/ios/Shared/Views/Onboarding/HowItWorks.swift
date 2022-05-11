//
//  HowItWorks.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 08/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct HowItWorks: View {
    @EnvironmentObject var m: ChatModel
    var onboarding: Bool

    var body: some View {
        VStack(alignment: .leading) {
            Text("How SimpleX works")
                .font(.largeTitle)
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading) {
                    Group {
                        Text("Many people asked: *if SimpleX has no user identifiers, how can it deliver messages?*")
                        Text("To protect privacy, instead of user IDs used by all other platforms, SimpleX has identifiers for message queues, separate for each of your contacts.")
                        Text("You control through which server(s) **to receive** the messages, your contacts – the servers you use to message them.")
                        Text("Only client devices store user profiles, contacts, groups, and messages sent with **2-layer end-to-end encryption**.")
                        if onboarding {
                            Text("Read more in our GitHub repository.")
                        } else {
                            Text("Read more in our [GitHub repository](https://github.com/simplex-chat/simplex-chat#readme).")
                        }
                    }
                    .padding(.bottom)
                }
            }

            Spacer()

            if onboarding {
                OnboardingActionButton()
                    .padding(.bottom, 8)
            }
        }
        .lineLimit(10)
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
    }
}

struct HowItWorks_Previews: PreviewProvider {
    static var previews: some View {
        HowItWorks(onboarding: true)
    }
}
