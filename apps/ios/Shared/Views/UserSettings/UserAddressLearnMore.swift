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
                Text("You can share your address as a link or QR code - anybody can to connect to you.")
                Text("You won't lose your contacts if you later delete your address.")
                Text("When people request to connect, you can accept or reject it.")
                Text("Learn more in [User Guide](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/guide/app-settings.md#your-simplex-contact-address).")
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
