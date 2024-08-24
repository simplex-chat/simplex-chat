//
//  IncognitoHelp.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 22.08.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct IncognitoHelp: View {
    var body: some View {
        List {
            Text("Incognito mode")
                .font(.largeTitle)
                .bold()
                .fixedSize(horizontal: false, vertical: true)
                .padding(.vertical)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            VStack(alignment: .leading, spacing: 18) {
                Text("Incognito mode protects your privacy by using a new random profile for each contact.")
                Text("It allows having many anonymous connections without any shared data between them in a single chat profile.")
                Text("When you share an incognito profile with somebody, this profile will be used for the groups they invite you to.")
                Text("Read more in [User Guide](https://simplex.chat/docs/guide/chat-profiles.html#incognito-mode).")
            }
            .listRowBackground(Color.clear)
        }
        .modifier(ThemedBackground())
    }
}

struct IncognitoHelp_Previews: PreviewProvider {
    static var previews: some View {
        IncognitoHelp()
    }
}
