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
        
        ZStack(alignment: .topTrailing) {
            VStack(alignment: .leading, spacing: 8) {
                HStack(spacing: 8) {
                    Image(systemName: "envelope.circle.fill")
                    Text("SimpleX address:")
                        .font(.title3)
                }
                .frame(maxWidth: .infinity, alignment: .leading)
                HStack(spacing: 0) {
                    Button {
                        showAddressSheet = true
                    } label: {
                        Text("Create SimpleX address")
                    }
                    VStack {
                        Image(systemName: "info.circle")
                            .padding(8)
                    }
                    .onTapGesture {
                        showAddressInfoSheet = true
                    }
                }
            }
            Image(systemName: "multiply")
                .foregroundColor(theme.colors.secondary)
                .onTapGesture {
                    showAddressCreationAlert = true
                }
        }
        .padding()
        .background(theme.appColors.sentMessage)
        .cornerRadius(12)
        .frame(height: dynamicSize(userFont).rowHeight)
        .padding(.vertical, 12)
        .alert(isPresented: $showAddressCreationAlert) {
            Alert(
                title: Text("SimpleX address"),
                message: Text("You can create it in user picker."),
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
                UserAddressLearnMore()
                    .navigationTitle("SimpleX address")
                    .navigationBarTitleDisplayMode(.large)
                    .modifier(ThemedBackground(grouped: true))
            }
        }
        .onChange(of: addressExists) { exists in
            if exists, !addressCreationCardShown {
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
