//
//  OneHandUICard.swift
//  SimpleX (iOS)
//
//  Created by EP on 06/08/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct OneHandUICard: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true
    @AppStorage(DEFAULT_ONE_HAND_UI_CARD_SHOWN) private var oneHandUICardShown = false
    @State private var showOneHandUIAlert = false

    var body: some View {
        ZStack(alignment: .topTrailing) {
            VStack(alignment: .leading, spacing: 8) {
                Text("Toggle chat list:").font(.title3)
                Toggle("Reachable chat toolbar", isOn: $oneHandUI)
            }
            Image(systemName: "multiply")
                .foregroundColor(theme.colors.secondary)
                .onTapGesture {
                    showOneHandUIAlert = true
                }
        }
        .padding()
        .background(theme.appColors.sentMessage)
        .cornerRadius(12)
        .frame(height: dynamicSize(userFont).rowHeight)
        .padding(.vertical, 12)
        .alert(isPresented: $showOneHandUIAlert) {
            Alert(
                title: Text("Reachable chat toolbar"),
                message: Text("You can change it in Appearance settings."),
                dismissButton: .default(Text("Ok")) {
                    withAnimation {
                        oneHandUICardShown = true
                    }
                }
            )
        }
    }
}

#Preview {
    OneHandUICard()
}
