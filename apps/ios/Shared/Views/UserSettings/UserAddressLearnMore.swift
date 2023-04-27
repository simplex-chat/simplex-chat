//
//  UserAddressLearnMore.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 27.04.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct UserAddressLearnMore: View {
    var body: some View {
        List {
            VStack(alignment: .leading, spacing: 18) {
                Text("You can create a long term address that can be used by other people to connect with you.")
                Text("Unlike 1-time invitation links, these addresses can be used many times, that makes them good to share online.")
                Text("When people connect to you via this address, you will receive a connection request that you can accept or reject.")
                Text("Read more in [User Guide](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/guide/app-settings.md#your-simplex-contact-address).")
            }
            .listRowBackground(Color.clear)
        }
    }
}

struct UserAddressLearnMore_Previews: PreviewProvider {
    static var previews: some View {
        UserAddressLearnMore()
    }
}
