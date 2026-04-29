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
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true
    @AppStorage(DEFAULT_ONE_HAND_UI_CARD_SHOWN) private var oneHandUICardShown = false
    @AppStorage(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial
    @State private var showOneHandUIAlert = false

    var body: some View {
        HStack(spacing: 2) {
            segment(
                icon: "platter.filled.bottom.and.arrow.down.iphone",
                text: "Bottom bar",
                isSelected: oneHandUI
            ) {
                withAnimation { oneHandUI = true }
            }
            .background { if oneHandUI { Color(uiColor: .systemGray5) } }
            .background(ToolbarMaterial.material(toolbarMaterial))
            ZStack(alignment: .trailing) {
                segment(
                    icon: "platter.filled.top.and.arrow.up.iphone",
                    text: "Top bar",
                    isSelected: !oneHandUI
                ) {
                    withAnimation { oneHandUI = false }
                }
                Image(systemName: "multiply")
                    .foregroundColor(theme.colors.secondary)
                    .frame(width: 12, height: 12)
                    .padding(.vertical, 4)
                    .padding(.trailing, 16)
                    .padding(.leading, 4)
                    .contentShape(Rectangle())
                    .onTapGesture {
                        showOneHandUIAlert = true
                    }
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .background { if !oneHandUI { Color(uiColor: .systemGray5) } }
            .background(ToolbarMaterial.material(toolbarMaterial))
        }
        .clipShape(Capsule())
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

    private func segment(icon: String, text: LocalizedStringKey, isSelected: Bool, action: @escaping () -> Void) -> some View {
        HStack(spacing: 8) {
            Image(systemName: icon)
                .font(.body)
                .foregroundColor(isSelected ? theme.colors.secondary : theme.colors.primary)
            Text(text)
                .font(.subheadline)
                .foregroundColor(isSelected ? theme.colors.secondary : theme.colors.onBackground)
        }
        .padding(.leading, 16)
        .padding(.vertical, 4)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .leading)
        .contentShape(Rectangle())
        .onTapGesture { action() }
    }
}

#Preview {
    OneHandUICard()
}
