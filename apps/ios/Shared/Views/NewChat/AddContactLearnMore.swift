//
//  AddContactLearnMore.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 27.04.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct AddContactLearnMore: View {
    var showTitle: Bool

    var body: some View {
        List {
            if showTitle {
                Text("One-time invitation link")
                    .font(.largeTitle)
                    .bold()
                    .fixedSize(horizontal: false, vertical: true)
                    .padding(.vertical)
                    .listRowBackground(Color.clear)
                    .listRowSeparator(.hidden)
                    .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            }
            VStack(alignment: .leading, spacing: 18) {
                Text("To connect, your contact can scan QR code or use the link in the app.")
                Text("If you can't meet in person, show QR code in a video call, or share the link.")
                Text("Read more in [User Guide](https://simplex.chat/docs/guide/readme.html#connect-to-friends).")
            }
            .listRowBackground(Color.clear)
        }
    }
}

struct AddContactLearnMore_Previews: PreviewProvider {
    static var previews: some View {
        AddContactLearnMore(showTitle: true)
    }
}
