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
    @Binding var selectedChatItems: Set<Int64>?

    var body: some View {
        return Text("Selected \(selectedChatItems?.count ?? 0)").font(.headline)
            .foregroundColor(theme.colors.onBackground)
            .frame(width: 220)
    }
}

struct SelectedItemsBottomToolbar: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
    let chatItems: [ChatItem]
    @Binding var selectedChatItems: Set<Int64>?
    var chatInfo: ChatInfo
    // Bool - delete for everyone is possible
    var deleteItems: (Bool) -> Void
    var moderateItems: () -> Void
    //var shareItems: () -> Void
    @State var deleteEnabled: Bool = false
    @State var deleteForEveryoneEnabled: Bool = false

    @State var canModerate: Bool = false
    @State var moderateEnabled: Bool = false

    @State var allButtonsDisabled = false

    var body: some View {
        VStack(spacing: 0) {
            Divider()

            HStack(alignment: .center) {
                Button {
                    deleteItems(deleteForEveryoneEnabled)
                } label: {
                    Image(systemName: "trash")
                        .resizable()
                        .frame(width: 20, height: 20, alignment: .center)
                        .foregroundColor(!deleteEnabled || allButtonsDisabled ? theme.colors.secondary: .red)
                }
                .disabled(!deleteEnabled || allButtonsDisabled)

                Spacer()
                Button {
                    moderateItems()
                } label: {
                    Image(systemName: "flag")
                        .resizable()
                        .frame(width: 20, height: 20, alignment: .center)
                        .foregroundColor(!moderateEnabled || allButtonsDisabled ? theme.colors.secondary : .red)
                }
                .disabled(!moderateEnabled || allButtonsDisabled)
                .opacity(canModerate ? 1 : 0)


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

    private func recheckItems(_ chatInfo: ChatInfo, _ chatItems: [ChatItem], _ selectedItems: Set<Int64>?) {
        let count = selectedItems?.count ?? 0
        allButtonsDisabled = count == 0 || count > 20
        canModerate = possibleToModerate(chatInfo)
        if let selected = selectedItems {
            (deleteEnabled, deleteForEveryoneEnabled, moderateEnabled, _, selectedChatItems) = chatItems.reduce((true, true, true, true, [])) { (r, ci) in
                if selected.contains(ci.id) {
                    var (de, dee, me, onlyOwnGroupItems, sel) = r
                    de = de && ci.canBeDeletedForSelf
                    dee = dee && ci.meta.deletable && !ci.localNote
                    onlyOwnGroupItems = onlyOwnGroupItems && ci.chatDir == .groupSnd
                    me = me && !onlyOwnGroupItems && ci.content.msgContent != nil && ci.memberToModerate(chatInfo) != nil
                    sel.insert(ci.id) // we are collecting new selected items here to account for any changes in chat items list
                    return (de, dee, me, onlyOwnGroupItems, sel)
                } else {
                    return r
                }
            }
        }
    }

    private func possibleToModerate(_ chatInfo: ChatInfo) -> Bool {
        return switch chatInfo {
        case let .group(groupInfo):
            groupInfo.membership.memberRole >= .admin
        default: false
        }
    }
}
