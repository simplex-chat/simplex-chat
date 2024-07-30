//
//  ChatInfoToolbar.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatInfoToolbar: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    var imageSize: CGFloat = 32

    var body: some View {
        let cInfo = chat.chatInfo
        return HStack {
            if (cInfo.incognito) {
                Image(systemName: "theatermasks").frame(maxWidth: 24, maxHeight: 24, alignment: .center).foregroundColor(.indigo)
                Spacer().frame(width: 16)
            }
            ChatInfoImage(
                chat: chat,
                size: imageSize,
                color: Color(uiColor: .tertiaryLabel)
            )
            .padding(.trailing, 4)
            let t = Text(cInfo.displayName).font(.headline)
            (cInfo.contact?.verified == true ? contactVerifiedShield + t : t)
                .lineLimit(1)
                .if (cInfo.fullName != "" && cInfo.displayName != cInfo.fullName) { v in
                    VStack(spacing: 0) {
                        v
                        Text(cInfo.fullName).font(.subheadline)
                            .lineLimit(1)
                            .padding(.top, -2)
                    }
                }
        }
        .foregroundColor(theme.colors.onBackground)
        .frame(width: 220)
    }

    private var contactVerifiedShield: Text {
        (Text(Image(systemName: "checkmark.shield")) + Text(" "))
            .font(.caption)
            .foregroundColor(theme.colors.secondary)
            .baselineOffset(1)
            .kerning(-2)
    }
}

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
        HStack(alignment: .center) {
            Button {
                deleteItems(possibleToDeleteForEveryone)
            } label: {
                Image(systemName: "trash")
                    .resizable()
                    .frame(width: 20, height: 20, alignment: .center)
                    .foregroundColor(!deleteButtonEnabled && allButtonsDisabled ? theme.colors.secondary: .red)
            }
            .disabled(!deleteButtonEnabled || allButtonsDisabled)

            Spacer()
            Button {
                moderateItems()
            } label: {
                Image(systemName: "flag")
                    .resizable()
                    .frame(width: 20, height: 20, alignment: .center)
                    .foregroundColor(moderateButtonEnabled ? .red : theme.colors.secondary)
            }
            .disabled(!moderateButtonEnabled || allButtonsDisabled)
            .opacity(possibleToModerate ? 1 : 0)
            .allowsHitTesting(possibleToModerate)


            Spacer()
            Button {
                //shareItems()
            } label: {
                Image(systemName: "square.and.arrow.up")
                    .resizable()
                    .frame(width: 20, height: 20, alignment: .center)
                    .foregroundColor(theme.colors.primary)
            }
            .disabled(allButtonsDisabled)
            .opacity(0)
            .allowsHitTesting(false)
        }
        .onAppear {
            cleanUpNonExistent(chatItems, selectedChatItems)

            allButtonsDisabled = !(1...20).contains(selectedChatItems?.count ?? 0)
            deleteButtonEnabled = deleteButtonEnabled(chatItems, selectedChatItems)
            possibleToModerate = possibleToModerate(chatInfo)
            moderateButtonEnabled = moderateButtonEnabled(chatInfo, chatItems, selectedChatItems)
            logger.debug("LALAL1 \(selectedChatItems?.count ?? -1) \(allButtonsDisabled) \(deleteButtonEnabled) \(possibleToModerate) \(moderateButtonEnabled)")
        }
        .onChange(of: chatItems) { items in
            cleanUpNonExistent(items, selectedChatItems)

            deleteButtonEnabled = deleteButtonEnabled(items, selectedChatItems)
            possibleToModerate = possibleToModerate(chatInfo)
            moderateButtonEnabled = possibleToModerate && !allButtonsDisabled ? moderateButtonEnabled(chatInfo, items, selectedChatItems) : false
            logger.debug("LALAL2 \(selectedChatItems?.count ?? -1) \(allButtonsDisabled) \(deleteButtonEnabled) \(possibleToModerate) \(moderateButtonEnabled)")
        }
        .onChange(of: selectedChatItems) { selected in
            cleanUpNonExistent(chatItems, selected)

            allButtonsDisabled = !(1...20).contains(selectedChatItems?.count ?? 0)
            deleteButtonEnabled = deleteButtonEnabled(chatItems, selected)
            moderateButtonEnabled = possibleToModerate && !allButtonsDisabled ? moderateButtonEnabled(chatInfo, chatItems, selected) : false
            logger.debug("LALAL3 \(selectedChatItems?.count ?? -1) \(allButtonsDisabled) \(deleteButtonEnabled) \(possibleToModerate) \(moderateButtonEnabled)")
        }
        .padding([.leading, .trailing], 10)
        .frame(height: 54)
    }

    func cleanUpNonExistent(_ chatItems: [ChatItem], _ selectedItems: [Int64]?) {
        var selected = selectedItems ?? []
        selected.removeAll(where: { selectedId in !chatItems.contains(where: { $0.id == selectedId }) })
        if selectedChatItems != nil && selectedChatItems != selected {
            selectedChatItems = selected
        }
    }

    func deleteButtonEnabled(_ chatItems: [ChatItem], _ selectedItems: [Int64]?) -> Bool {
        guard let selected = selectedItems, selected.count > 0 else {
            return false
        }
        return chatItems.filter { item in selected.contains(item.id) }.allSatisfy({ ci in
            if !ci.meta.deletable || ci.localNote {
                possibleToDeleteForEveryone = false
            }
            return (ci.content.msgContent != nil && !ci.meta.isLive) || ci.meta.itemDeleted != nil || ci.isDeletedContent || ci.mergeCategory != nil || ci.showLocalDelete
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

struct ChatInfoToolbar_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoToolbar(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
            .environmentObject(CurrentColors.toAppTheme())
    }
}
