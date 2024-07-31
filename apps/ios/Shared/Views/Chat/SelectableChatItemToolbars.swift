//
//  SelectableChatItemToolbars.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 30.07.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SelectedItemsTopToolbar: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
    @Binding var selectedChatItems: [Int64]?

    var body: some View {
        return Text("Selected \(selectedChatItems?.count ?? 0)").font(.headline)
            .foregroundColor(theme.colors.onBackground)
            .frame(width: 220)
    }
}

struct SelectedItemsBottomToolbar: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
    var chatItems: [ChatItem]
    @Binding var selectedChatItems: [Int64]?
    var chatInfo: ChatInfo
    // Bool - delete for everyone is possible
    var deleteItems: (Bool) -> Void
    var moderateItems: () -> Void
    //var shareItems: () -> Void
    @State var deleteButtonEnabled: Bool = false
    @State var possibleToDeleteForEveryone: Bool = false

    @State var possibleToModerate: Bool = false
    @State var moderateButtonEnabled: Bool = false

    @State var allButtonsDisabled = false

    var body: some View {
        VStack(spacing: 0) {
            Divider()

            HStack(alignment: .center) {
                Button {
                    deleteItems(possibleToDeleteForEveryone)
                } label: {
                    Image(systemName: "trash")
                        .resizable()
                        .frame(width: 20, height: 20, alignment: .center)
                        .foregroundColor(!deleteButtonEnabled || allButtonsDisabled ? theme.colors.secondary: .red)
                }
                .disabled(!deleteButtonEnabled || allButtonsDisabled)

                Spacer()
                Button {
                    moderateItems()
                } label: {
                    Image(systemName: "flag")
                        .resizable()
                        .frame(width: 20, height: 20, alignment: .center)
                        .foregroundColor(!moderateButtonEnabled || allButtonsDisabled ? theme.colors.secondary : .red)
                }
                .disabled(!moderateButtonEnabled || allButtonsDisabled)
                .opacity(possibleToModerate ? 1 : 0)


                Spacer()
                Button {
                    //shareItems()
                } label: {
                    Image(systemName: "square.and.arrow.up")
                        .resizable()
                        .frame(width: 20, height: 20, alignment: .center)
                        .foregroundColor(allButtonsDisabled ? theme.colors.secondary : theme.colors.primary)
                }
                .disabled(allButtonsDisabled)
                .opacity(0)
            }
            .frame(maxHeight: .infinity)
            .padding([.leading, .trailing], 12)
        }
        .onAppear {
            recheckItems(chatInfo, chatItems, selectedChatItems)
        }
        .onChange(of: chatInfo) { info in
            recheckItems(info, chatItems, selectedChatItems)
        }
        .onChange(of: chatItems) { items in
            recheckItems(chatInfo, items, selectedChatItems)
        }
        .onChange(of: selectedChatItems) { selected in
            recheckItems(chatInfo, chatItems, selected)
        }
        .frame(height: 55.5)
        .background(.thinMaterial)
    }

    private func recheckItems(_ chatInfo: ChatInfo, _ chatItems: [ChatItem], _ selectedItems: [Int64]?) {
        cleanUpNonExistent(chatItems, selectedItems)

        let count = selectedItems?.count ?? 0
        allButtonsDisabled = count == 0 || count > 20
        deleteButtonEnabled = deleteButtonEnabled(chatItems, selectedItems)
        possibleToModerate = possibleToModerate(chatInfo)
        moderateButtonEnabled = moderateButtonEnabled(chatInfo, chatItems, selectedItems)
    }

    private func cleanUpNonExistent(_ chatItems: [ChatItem], _ selectedItems: [Int64]?) {
        var selected = selectedItems ?? []
        selected.removeAll(where: { selectedId in !chatItems.contains(where: { $0.id == selectedId }) })
        if selectedChatItems != nil && selectedChatItems != selected {
            selectedChatItems = selected
        }
    }

    private func deleteButtonEnabled(_ chatItems: [ChatItem], _ selectedItems: [Int64]?) -> Bool {
        guard let selected = selectedItems, selected.count > 0 else {
            return false
        }
        possibleToDeleteForEveryone = true
        return chatItems.filter { item in selected.contains(item.id) }.allSatisfy({ ci in
            if !ci.meta.deletable || ci.localNote {
                possibleToDeleteForEveryone = false
            }
            return ci.canBeDeletedForSelf
        })
    }

    private func possibleToModerate(_ chatInfo: ChatInfo) -> Bool {
        return switch chatInfo {
        case let .group(groupInfo):
            groupInfo.membership.memberRole >= .admin
        default: false
        }
    }

    private func moderateButtonEnabled(_ chatInfo: ChatInfo, _ chatItems: [ChatItem], _ selectedItems: [Int64]?) -> Bool {
        guard let selected = selectedItems, selected.count > 0 else {
            return false
        }
        var onlyOwnItems = true
        let canModerate = chatItems.filter { item in selected.contains(item.id) }.allSatisfy({ ci in
            if ci.chatDir != .groupSnd {
                onlyOwnItems = false
            }
            return ci.content.msgContent != nil && ci.memberToModerate(chatInfo) != nil
        })
        return canModerate && !onlyOwnItems
    }
}
