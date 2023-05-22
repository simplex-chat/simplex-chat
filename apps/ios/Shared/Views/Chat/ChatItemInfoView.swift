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
        NavigationView {
            itemInfoView()
                .toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        Button { showShareSheet(items: [itemInfoShareText()]) } label: {
                            Image(systemName: "square.and.arrow.up")
                        }
                    }
                }
        }
    }

    private var title: String {
        ci.chatDir.sent
        ? NSLocalizedString("Sent message", comment: "message info title")
        : NSLocalizedString("Received message", comment: "message info title")
    }

    @ViewBuilder private func itemInfoView() -> some View {
        let meta = ci.meta
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .leading, spacing: 16) {
                    Text(title)
                        .font(.largeTitle)
                        .bold()
                        .padding(.bottom)

                    let maxWidth = (g.size.width - 32) * 0.84
                    infoRow("Sent at", localTimestamp(meta.itemTs))
                    if !ci.chatDir.sent {
                        infoRow("Received at", localTimestamp(meta.createdAt))
                    }
                    switch (meta.itemDeleted) {
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
                    if let deleteAt = meta.itemTimed?.deleteAt {
                        infoRow("Disappears at", localTimestamp(deleteAt))
                    }
                    if developerTools {
                        infoRow("Database ID", "\(meta.itemId)")
                        infoRow("Record updated at", localTimestamp(meta.updatedAt))
                    }

                    if let chatItemInfo = chatItemInfo,
                       !chatItemInfo.itemVersions.isEmpty {
                        Divider().padding(.vertical)

                        Text("History")
                            .font(.title2)
                            .padding(.bottom, 4)
                        LazyVStack(alignment: .leading, spacing: 16)  {
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
        VStack(alignment: .leading, spacing: 4) {
            versionText(itemVersion)
                .allowsHitTesting(false)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(chatItemFrameColor(ci, colorScheme))
                .cornerRadius(18)
                .contextMenu {
                    if itemVersion.msgContent.text != "" {
                        Button {
                            showShareSheet(items: [itemVersion.msgContent.text])
                        } label: {
                            Label("Share", systemImage: "square.and.arrow.up")
                        }
                        Button {
                            UIPasteboard.general.string = itemVersion.msgContent.text
                        } label: {
                            Label("Copy", systemImage: "doc.on.doc")
                        }
                    }
                }
            let ts = localTimestamp(itemVersion.itemVersionTs)
            (current ? Text("\(ts) (current)") : Text(ts))
                .foregroundStyle(.secondary)
                .font(.caption)
                .padding(.horizontal, 12)
        }
        .frame(maxWidth: maxWidth, alignment: .leading)
    }

    @ViewBuilder private func versionText(_ itemVersion: ChatItemVersion) -> some View {
        if itemVersion.msgContent.text != "" {
            messageText(itemVersion.msgContent.text, itemVersion.formattedText, nil)
        } else {
            Text("[no text]")
                .italic()
                .foregroundColor(.secondary)
        }
    }

    private func itemInfoShareText() -> String {
        let meta = ci.meta
        var shareText: [String] = [title, ""]
        shareText += [String.localizedStringWithFormat(NSLocalizedString("Sent at: %@", comment: "copied message info"), localTimestamp(meta.itemTs))]
        if !ci.chatDir.sent {
            shareText += [String.localizedStringWithFormat(NSLocalizedString("Received at: %@", comment: "copied message info"), localTimestamp(meta.createdAt))]
        }
        switch (ci.meta.itemDeleted) {
        case let .deleted(deletedTs):
            if let deletedTs = deletedTs {
                shareText += [String.localizedStringWithFormat(NSLocalizedString("Deleted at: %@", comment: "copied message info"), localTimestamp(deletedTs))]
            }
        case let .moderated(deletedTs, _):
            if let deletedTs = deletedTs {
                shareText += [String.localizedStringWithFormat(NSLocalizedString("Moderated at: %@", comment: "copied message info"), localTimestamp(deletedTs))]
            }
        default: ()
        }
        if let deleteAt = meta.itemTimed?.deleteAt {
            shareText += [String.localizedStringWithFormat(NSLocalizedString("Disappears at: %@", comment: "copied message info"), localTimestamp(deleteAt))]
        }
        if developerTools {
            shareText += [
                String.localizedStringWithFormat(NSLocalizedString("Database ID: %d", comment: "copied message info"), meta.itemId),
                String.localizedStringWithFormat(NSLocalizedString("Record updated at: %@", comment: "copied message info"), localTimestamp(meta.updatedAt))
            ]
        }
        if let chatItemInfo = chatItemInfo,
           !chatItemInfo.itemVersions.isEmpty {
            shareText += ["", NSLocalizedString("History", comment: "copied message info")]
            for (index, itemVersion) in chatItemInfo.itemVersions.enumerated() {
                shareText += [
                    "",
                    String.localizedStringWithFormat(
                        index == 0 && ci.meta.itemDeleted == nil
                        ? NSLocalizedString("%@ (current):", comment: "copied message info")
                        : NSLocalizedString("%@:", comment: "copied message info"),
                        localTimestamp(itemVersion.itemVersionTs)
                    ),
                    itemVersion.msgContent.text != "" ? itemVersion.msgContent.text : NSLocalizedString("[no text]", comment: "copied message info")
                ]
            }
        }
        return shareText.joined(separator: "\n")
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
