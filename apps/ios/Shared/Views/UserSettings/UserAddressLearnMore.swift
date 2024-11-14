//
//  UserAddressLearnMore.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 27.04.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct UserAddressLearnMore: View {
    @State var showCreateAddressButton = false
    @State private var createAddressLinkActive = false
    
    var body: some View {
        VStack {
            List {
                VStack(alignment: .leading, spacing: 18) {
                    Text("You can share your address as a link or QR code - anybody can connect to you.")
                    Text("You won't lose your contacts if you later delete your address.")
                    Text("When people request to connect, you can accept or reject it.")
                    Text("Read more in [User Guide](https://simplex.chat/docs/guide/app-settings.html#your-simplex-contact-address).")
                }
                .listRowBackground(Color.clear)
            }
            .frame(maxHeight: .infinity)

            if showCreateAddressButton {
                addressCreationButton()
                    .padding()
            }
        }
    }
    
    private func addressCreationButton() -> some View {
        ZStack {
            Button {
                createAddressLinkActive = true
            } label: {
                Text("Create SimpleX address")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: false))

            NavigationLink(isActive: $createAddressLinkActive) {
                UserAddressView(autoCreate: true)
                    .navigationTitle("SimpleX address")
                    .navigationBarTitleDisplayMode(.large)
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }
}

struct UserAddressLearnMore_Previews: PreviewProvider {
    static var previews: some View {
        UserAddressLearnMore()
    }
}
