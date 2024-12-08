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
    @State private var createOneTimeLinkActive = false

    var body: some View {
        VStack {
            List {
                VStack(alignment: .leading, spacing: 12) {
                    (Text(Image(systemName: "envelope")).foregroundColor(.secondary) + textSpace + Text("Share address publicly").bold().font(.title2))
                    Text("Share SimpleX address on social media.")
                    Text("You won't lose your contacts if you later delete your address.")

                    (Text(Image(systemName: "link.badge.plus")).foregroundColor(.secondary) + textSpace + Text("Share 1-time link with a friend").font(.title2).bold())
                        .padding(.top)
                    Text("1-time link can be used *with one contact only* - share in person or via any messenger.")
                    Text("You can set connection name, to remember who the link was shared with.")

                    if !showCreateAddressButton {
                        (Text(Image(systemName: "shield")).foregroundColor(.secondary) + textSpace + Text("Connection security").font(.title2).bold())
                            .padding(.top)
                        Text("SimpleX address and 1-time links are safe to share via any messenger.")
                        Text("To protect against your link being replaced, you can compare contact security codes.")
                        Text("Read more in [User Guide](https://simplex.chat/docs/guide/making-connections.html#comparison-of-1-time-invitation-links-and-simplex-contact-addresses).")
                            .padding(.top)
                    }

                }
                .listRowBackground(Color.clear)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
            }
            .frame(maxHeight: .infinity, alignment: .top)

            Spacer()
            
            if showCreateAddressButton {
                VStack {
                    addressCreationButton()
                        .padding(.bottom)

                    createOneTimeLinkButton()
                }
                .padding()
            }
        }
        .frame(maxHeight: .infinity, alignment: .top)
    }
    
    private func addressCreationButton() -> some View {
        ZStack {
            Button {
                createAddressLinkActive = true
            } label: {
                Text("Create SimpleX address")
            }
            .buttonStyle(OnboardingButtonStyle())

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

    private func createOneTimeLinkButton() -> some View {
        ZStack {
            Button {
                createOneTimeLinkActive = true
            } label: {
                Text("Create 1-time link")
                    .font(.callout)
            }

            NavigationLink(isActive: $createOneTimeLinkActive) {
                NewChatView(selection: .invite)
                    .navigationTitle("New chat")
                    .navigationBarTitleDisplayMode(.large)
                    .modifier(ThemedBackground(grouped: true))
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
