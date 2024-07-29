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
    var deleteItems: () -> Void = {}
    var moderateItems: () -> Void = {}
    //var shareItems: () -> Void
    @State var possibleToDelete: Bool = false
    @State var deleteButtonEnabled: Bool = false
    @State var possibleToModerate: Bool = false
    @State var moderateButtonEnabled: Bool = false

    var body: some View {
        HStack(alignment: .center) {
            Button {
                deleteItems()
            } label: {
                Image(systemName: "trash")
                    .resizable()
                    .frame(width: 20, height: 20, alignment: .center)
                    .foregroundColor(deleteButtonEnabled ? .red : theme.colors.secondary)
            }
            .disabled(!deleteButtonEnabled)

            Spacer()
            Button {
                moderateItems()
            } label: {
                Image(systemName: "flag")
                    .resizable()
                    .frame(width: 20, height: 20, alignment: .center)
                    .foregroundColor(moderateButtonEnabled ? .red : theme.colors.secondary)
            }
            .disabled(!moderateButtonEnabled)
            .opacity(possibleToModerate ? 1 : 0)
            .allowsHitTesting(possibleToModerate ? true : false)


            Spacer()
            Button {
                //shareItems()
            } label: {
                Image(systemName: "square.and.arrow.up")
                    .resizable()
                    .frame(width: 20, height: 20, alignment: .center)
                    .foregroundColor(theme.colors.primary)
            }
            .opacity(0)
            .allowsHitTesting(false)
        }
        .onAppear {
            possibleToModerate = possibleToModerate(chatInfo)
            moderateButtonEnabled = moderateButtonEnabled(chatInfo, chatItems, selectedChatItems)
        }
        .onChange(of: chatItems) { items in
            possibleToModerate = possibleToModerate(chatInfo)
            moderateButtonEnabled = moderateButtonEnabled(chatInfo, items, selectedChatItems)
        }
        .onChange(of: selectedChatItems) { selected in
            moderateButtonEnabled = moderateButtonEnabled(chatInfo, chatItems, selected)
        }
        .padding([.leading, .trailing], 10)
        .frame(height: 54)
    }

    private func possibleToModerate(_ chatInfo: ChatInfo) -> Bool {
        return switch chatInfo {
        case let .group(groupInfo):
            groupInfo.membership.memberRole >= .observer
        default: false
        }
    }

    private func moderateButtonEnabled(_ chatInfo: ChatInfo, _ chatItems: [ChatItem], _ selectedItems: [Int64]?) -> Bool {
        if possibleToModerate {
            var selected = selectedItems ?? []
            selected.removeAll(where: { selectedId in !chatItems.contains(where: { $0.id == selectedId }) })
            if selectedChatItems != nil && selectedChatItems != selected {
                selectedChatItems = selected
            }
            return selected.count > 0 && chatItems.filter { item in selected.contains(item.id) }.allSatisfy({ ci in ci.memberToModerate(chatInfo) != nil })
        } else {
            return false
        }
    }
}

struct ChatInfoToolbar_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoToolbar(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
            .environmentObject(CurrentColors.toAppTheme())
    }
}
