//
//  ChatItemInfoView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 09.05.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatItemInfoView: View {
    @Environment(\.colorScheme) var colorScheme
    var ci: ChatItem
    @Binding var chatItemInfo: ChatItemInfo?
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    
    var body: some View {
        if let chatItemInfo = chatItemInfo {
            NavigationView {
                itemInfoView(chatItemInfo)
                    .toolbar {
                        ToolbarItem(placement: .navigationBarTrailing) {
                            Button { showShareSheet(items: [itemInfoShareText(chatItemInfo)]) } label: {
                                Image(systemName: "square.and.arrow.up")
                            }
                        }
                    }
            }
        } else {
            Text("No message details")
        }
    }

    @ViewBuilder private func itemInfoView(_ chatItemInfo: ChatItemInfo) -> some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .leading, spacing: 12) {
                    Text("Message details")
                        .font(.largeTitle)
                        .bold()
                        .padding(.bottom)

                    let maxWidth = (g.size.width - 32) * 0.84
                    infoRow("Sent at", localTimestamp(ci.meta.itemTs))
                    if !ci.chatDir.sent {
                        infoRow("Received at", localTimestamp(ci.meta.createdAt))
                    }
                    switch (ci.meta.itemDeleted) {
                    case let .deleted(deletedTs):
                        if let deletedTs = deletedTs {
                            infoRow("Deleted at", localTimestamp(deletedTs))
                        }
                    case let .moderated(deletedTs, _):
                        if let deletedTs = deletedTs {
                            infoRow("Moderated at", localTimestamp(deletedTs))
                        }
                    default: EmptyView()
                    }
                    if let deleteAt = ci.meta.itemTimed?.deleteAt {
                        infoRow("Disappears at", localTimestamp(deleteAt))
                    }
                    if developerTools {
                        infoRow("Database ID", "\(ci.meta.itemId)")
                        infoRow("Record updated at", localTimestamp(ci.meta.updatedAt))
                    }

                    if !chatItemInfo.itemVersions.isEmpty {
                        Divider()
                            .padding(.top)

                        Text("History")
                            .font(.title)
                            .padding(.bottom, 4)
                        LazyVStack(alignment: .leading, spacing: 12)  {
                            ForEach(Array(chatItemInfo.itemVersions.enumerated()), id: \.element.chatItemVersionId) { index, itemVersion in
                                itemVersionView(itemVersion, maxWidth, current: index == 0 && ci.meta.itemDeleted == nil)
                            }
                        }
                    }
                }
            }
            .padding()
            .frame(maxHeight: .infinity, alignment: .top)
        }
    }
    
    @ViewBuilder private func itemVersionView(_ itemVersion: ChatItemVersion, _ maxWidth: CGFloat, current: Bool) -> some View {
        let uiMenu: Binding<UIMenu> = Binding(
            get: { UIMenu(title: "", children: itemVersionMenu(itemVersion)) },
            set: { _ in }
        )
        VStack(alignment: .leading, spacing: 4) {
            messageText(itemVersion.msgContent.text, parseSimpleXMarkdown(itemVersion.msgContent.text), nil)
                .allowsHitTesting(false)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(ciDirFrameColor(chatItemSent: ci.chatDir.sent, colorScheme: colorScheme))
                .cornerRadius(18)
                .uiKitContextMenu(menu: uiMenu, allowMenu: Binding.constant(true))
            Text(
                localTimestamp(itemVersion.itemVersionTs)
                + (current
                   ? (" (" + NSLocalizedString("Current", comment: "designation of the current version of the message") + ")")
                   : "")
            )
            .foregroundStyle(.secondary)
            .font(.caption)
            .padding(.horizontal, 12)
        }
        .frame(maxWidth: maxWidth, alignment: .leading)
    }
    
    func itemVersionMenu(_ itemVersion: ChatItemVersion) -> [UIAction] {[
        UIAction(
            title: NSLocalizedString("Share", comment: "chat item action"),
            image: UIImage(systemName: "square.and.arrow.up")
        ) { _ in
            showShareSheet(items: [itemVersion.msgContent.text])
        },
        UIAction(
            title: NSLocalizedString("Copy", comment: "chat item action"),
            image: UIImage(systemName: "doc.on.doc")
        ) { _ in
            UIPasteboard.general.string = itemVersion.msgContent.text
        }
    ]}

    func itemInfoShareText(_ chatItemInfo: ChatItemInfo) -> String {
        var shareText = ""
        let nl = "\n"
        shareText += "Message details" + nl + nl
        shareText += "Sent at: \(localTimestamp(ci.meta.itemTs))" + nl
        if !ci.chatDir.sent {
            shareText += "Received at: \(localTimestamp(ci.meta.createdAt))" + nl
        }
        switch (ci.meta.itemDeleted) {
        case let .deleted(deletedTs):
            if let deletedTs = deletedTs {
                shareText += "Deleted at: \(localTimestamp(deletedTs))"
            }
        case let .moderated(deletedTs, _):
            if let deletedTs = deletedTs {
                shareText += "Moderated at: \(localTimestamp(deletedTs))"
            }
        default: break
        }
        if let deleteAt = ci.meta.itemTimed?.deleteAt {
            shareText += "Disappears at: \(localTimestamp(deleteAt))" + nl
        }
        if developerTools {
            shareText += "Database ID: \(ci.meta.itemId)" + nl
            shareText += "Record updated at: \(localTimestamp(ci.meta.updatedAt))"
        }
        if !chatItemInfo.itemVersions.isEmpty {
            shareText += nl + "History" + nl + nl
            for (index, itemVersion) in chatItemInfo.itemVersions.enumerated() {
                shareText += localTimestamp(itemVersion.itemVersionTs) + (index == 0 && ci.meta.itemDeleted == nil ? " (Current)" : "") + ":" + nl
                shareText += itemVersion.msgContent.text + nl + nl
            }
        }
        return shareText.trimmingCharacters(in: .newlines)
    }
}

func localTimestamp(_ date: Date) -> String {
    let localDateFormatter = DateFormatter()
    localDateFormatter.dateStyle = .medium
    localDateFormatter.timeStyle = .medium
    return localDateFormatter.string(from: date)
}

struct ChatItemInfoView_Previews: PreviewProvider {
    static var previews: some View {
        ChatItemInfoView(ci: ChatItem.getSample(1, .directSnd, .now, "hello"), chatItemInfo: Binding.constant(nil))
    }
}
