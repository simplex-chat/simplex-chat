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

    var body: some View {
        VStack(alignment: .leading) {
            Text("How SimpleX works")
                .font(.largeTitle)
                .padding(.vertical)
            Group {
                Text("Many people asked: *if SimpleX has no user identifiers, how can it deliver messages?*")
                Text("To protect users' privacy, instead of user identifiers used by other messaging platforms, SimpleX uses identifiers for message queues.")
                Text("Users control through which server(s) **to receive** the messages by creating queues and sharing them with their contacts as links or QR codes, that include asymmetric end-to-end encryption keys.")
                Text("Only the client devices store information about users, their contacts and groups.")
                Text("Read more in our GitHub repository.")
            }
            .font(.subheadline)
            .padding(.bottom)

            Spacer()

            OnboardingActionButton()

            Spacer()
        }
        .lineLimit(10)
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
    }
}

struct HowItWorks_Previews: PreviewProvider {
    static var previews: some View {
        HowItWorks()
    }
}
