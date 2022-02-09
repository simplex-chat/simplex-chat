//
//  UserAddress.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct UserAddress: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var shareAddressLink = false
    @State private var deleteAddressAlert = false

    var body: some View {
        VStack (alignment: .leading) {
            Text("You can share your address as a link or as a QR code - anybody will be able to connect to you, and if you later delete it - you won't lose your contacts.")
                .padding(.bottom)
            if let userAdress = chatModel.userAddress {
                QRCode(uri: userAdress)
                HStack {
                    Button { shareAddressLink = true } label: {
                        Label("Share link", systemImage: "square.and.arrow.up")
                    }
                    .padding()
                    .shareSheet(isPresented: $shareAddressLink, items: [userAdress])

                    Button { deleteAddressAlert = true } label: {
                        Label("Delete address", systemImage: "trash")
                    }
                    .padding()
                    .alert(isPresented: $deleteAddressAlert) {
                        Alert(
                            title: Text("Delete address?"),
                            message: Text("All your contacts will remain connected"),
                            primaryButton: .destructive(Text("Delete")) {
                                do {
                                    try apiDeleteUserAddress()
                                    chatModel.userAddress = nil
                                } catch let error {
                                    print("apiDeleteUserAddress: \(error)")
                                }
                            }, secondaryButton: .cancel()
                        )
                    }
                    .shareSheet(isPresented: $shareAddressLink, items: [userAdress])
                }
                .frame(maxWidth: .infinity)
            } else {
                Button {
                    do {
                        chatModel.userAddress = try apiCreateUserAddress()
                    } catch let error {
                        print("apiCreateUserAddress: \(error)")
                    }
                } label: { Label("Create address", systemImage: "qrcode") }
                .frame(maxWidth: .infinity)
            }
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
    }
}

struct UserAddress_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.userAddress = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"
        return UserAddress()
            .environmentObject(chatModel)
    }
}
