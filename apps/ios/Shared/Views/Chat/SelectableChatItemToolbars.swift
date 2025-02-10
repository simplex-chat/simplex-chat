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
        let count = selectedChatItems?.count ?? 0
        return Text(count == 0 ? "Nothing selected" : "Selected \(count)").font(.headline)
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
    var archiveItems: () -> Void
    var moderateItems: () -> Void
    //var shareItems: () -> Void
    var forwardItems: () -> Void
    @State var deleteEnabled: Bool = false
    @State var deleteForEveryoneEnabled: Bool = false

    @State var canArchiveReports: Bool = false

    @State var canModerate: Bool = false
    @State var moderateEnabled: Bool = false

    @State var forwardEnabled: Bool = false

    @State var deleteCountProhibited = false
    @State var forwardCountProhibited = false

    var body: some View {
        VStack(spacing: 0) {
            Divider()

            HStack(alignment: .center) {
                Button {
                    if canArchiveReports {
                        archiveItems()
                    } else {
                        deleteItems(deleteForEveryoneEnabled)
                    }
                } label: {
                    Image(systemName: "trash")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 20, height: 20, alignment: .center)
                        .foregroundColor(!deleteEnabled || deleteCountProhibited ? theme.colors.secondary: .red)
                }
                .disabled(!deleteEnabled || deleteCountProhibited)

                Spacer()
                Button {
                    moderateItems()
                } label: {
                    Image(systemName: "flag")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 20, height: 20, alignment: .center)
                        .foregroundColor(!moderateEnabled || deleteCountProhibited ? theme.colors.secondary : .red)
                }
                .disabled(!moderateEnabled || deleteCountProhibited)
                .opacity(canModerate ? 1 : 0)

                Spacer()
                Button {
                    forwardItems()
                } label: {
                    Image(systemName: "arrowshape.turn.up.forward")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 20, height: 20, alignment: .center)
                        .foregroundColor(!forwardEnabled || forwardCountProhibited ? theme.colors.secondary : theme.colors.primary)
                }
                .disabled(!forwardEnabled || forwardCountProhibited)
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
        deleteCountProhibited = count == 0 || count > 200
        forwardCountProhibited = count == 0 || count > 20
        canModerate = possibleToModerate(chatInfo)
        let groupInfo: GroupInfo? = if case let ChatInfo.group(groupInfo: info) = chatInfo {
            info
        } else {
             nil
        }
        if let selected = selectedItems {
            let me: Bool
            let onlyOwnGroupItems: Bool
            (deleteEnabled, deleteForEveryoneEnabled, canArchiveReports, me, onlyOwnGroupItems, forwardEnabled, selectedChatItems) = chatItems.reduce((true, true, true, true, true, true, [])) { (r, ci) in
                if selected.contains(ci.id) {
                    var (de, dee, ar, me, onlyOwnGroupItems, fe, sel) = r
                    de = de && ci.canBeDeletedForSelf
                    dee = dee && ci.meta.deletable && !ci.localNote && !ci.isReport
                    ar = ar && ci.isActiveReport && ci.chatDir != .groupSnd && groupInfo != nil && groupInfo!.membership.memberRole >= .moderator
                    onlyOwnGroupItems = onlyOwnGroupItems && ci.chatDir == .groupSnd && !ci.isReport
                    me = me && ci.content.msgContent != nil && ci.memberToModerate(chatInfo) != nil && !ci.isReport
                    fe = fe && ci.content.msgContent != nil && ci.meta.itemDeleted == nil && !ci.isLiveDummy && !ci.isReport
                    sel.insert(ci.id) // we are collecting new selected items here to account for any changes in chat items list
                    return (de, dee, ar, me, onlyOwnGroupItems, fe, sel)
                } else {
                    return r
                }
            }
            moderateEnabled = me && !onlyOwnGroupItems
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
