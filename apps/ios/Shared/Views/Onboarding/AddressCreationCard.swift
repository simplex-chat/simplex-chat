//
//  AddressCreationCard.swift
//  SimpleX (iOS)
//
//  Created by Diogo Cunha on 13/11/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct AddressCreationCard: View {
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject private var chatModel: ChatModel
    @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
    @AppStorage(DEFAULT_ADDRESS_CREATION_CARD_SHOWN) private var addressCreationCardShown = false
    @State private var showAddressCreationAlert = false
    @State private var showAddressSheet = false
    @State private var showAddressInfoSheet = false

    var body: some View {
        let addressExists = chatModel.userAddress != nil
        let chats = chatModel.chats.filter { chat in
            !chat.chatInfo.chatDeleted && !chat.chatInfo.contactCard
        }
        ZStack(alignment: .topTrailing) {
            HStack(alignment: .top, spacing: 16) {
                let envelopeSize = dynamicSize(userFont).profileImageSize
                Image(systemName: "envelope.circle.fill")
                    .resizable()
                    .frame(width: envelopeSize, height: envelopeSize)
                    .foregroundColor(.accentColor)
                VStack(alignment: .leading) {
                    Text("Your SimpleX address")
                        .font(.title3)
                    Spacer()
                    Text("How to use it") + textSpace + Text(Image(systemName: "info.circle")).foregroundColor(theme.colors.secondary)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            VStack(alignment: .trailing) {
                Image(systemName: "multiply")
                    .foregroundColor(theme.colors.secondary)
                    .onTapGesture {
                        showAddressCreationAlert = true
                    }
                Spacer()
                Text("Create")
                    .foregroundColor(.accentColor)
                    .onTapGesture {
                        showAddressSheet = true
                    }
            }
        }
        .onTapGesture {
            showAddressInfoSheet = true
        }
        .padding()
        .background(theme.appColors.sentMessage)
        .cornerRadius(12)
        .frame(height: dynamicSize(userFont).rowHeight)
        .alert(isPresented: $showAddressCreationAlert) {
            Alert(
                title: Text("SimpleX address"),
                message: Text("Tap Create SimpleX address in the menu to create it later."),
                dismissButton: .default(Text("Ok")) {
                    withAnimation {
                        addressCreationCardShown = true
                    }
                }
            )
        }
        .sheet(isPresented: $showAddressSheet) {
            NavigationView {
                UserAddressView(autoCreate: true)
                    .navigationTitle("SimpleX address")
                    .navigationBarTitleDisplayMode(.large)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
        .sheet(isPresented: $showAddressInfoSheet) {
            NavigationView {
                UserAddressLearnMore(showCreateAddressButton: true)
                    .navigationTitle("Address or 1-time link?")
                    .navigationBarTitleDisplayMode(.inline)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
        .onChange(of: addressExists) { exists in
            if exists, !addressCreationCardShown {
                addressCreationCardShown = true
            }
        }
        .onChange(of: chats.count) { size in
            if size >= 3, !addressCreationCardShown  {
                addressCreationCardShown = true
            }
        }
        .onAppear {
            if addressExists, !addressCreationCardShown {
                addressCreationCardShown = true
            }
        }
    }
}

#Preview {
    AddressCreationCard()
}
