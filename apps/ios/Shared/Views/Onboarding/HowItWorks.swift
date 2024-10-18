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
        List {
            Text("How SimpleX works")
                .font(.largeTitle)
                .bold()
                .fixedSize(horizontal: false, vertical: true)
                .padding(.vertical)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            VStack(alignment: .leading, spacing: 18) {
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
            .listRowBackground(Color.clear)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .lineLimit(10)
        .frame(maxHeight: .infinity, alignment: .top)
        .modifier(ThemedBackground(grouped: true))
    }
}

struct HowItWorks_Previews: PreviewProvider {
    static var previews: some View {
        HowItWorks(onboarding: true)
    }
}
