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
    var chatItem: ChatItem
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

    private var title: String {
        chatItem.chatDir.sent
        ? NSLocalizedString("Sent message", comment: "message info title")
        : NSLocalizedString("Received message", comment: "message info title")
    }

    @ViewBuilder private func itemInfoView(_ chatItemInfo: ChatItemInfo) -> some View {
        let meta = chatItem.meta
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .leading, spacing: 12) {
                    Text(title)
                        .font(.largeTitle)
                        .bold()
                        .padding(.bottom)

                    let maxWidth = (g.size.width - 32) * 0.84
                    if developerTools {
                        infoRow("Database ID", "\(meta.itemId)")
                    }
                    infoRow("Sent at", localTimestamp(meta.itemTs))
                    if !chatItem.chatDir.sent {
                        infoRow("Received at", localTimestamp(meta.createdAt))
                    }
                    if let deleteAt = meta.itemTimed?.deleteAt {
                        infoRow("To be deleted at", localTimestamp(deleteAt))
                    }

                    if !chatItemInfo.itemVersions.isEmpty {
                        Divider()
                            .padding(.top)

                        Text("Edit history")
                            .font(.title)
                            .padding(.bottom, 4)
                        LazyVStack(alignment: .leading, spacing: 12)  {
                            ForEach(Array(chatItemInfo.itemVersions.enumerated()), id: \.element.chatItemVersionId) { index, itemVersion in
                                itemVersionView(itemVersion, maxWidth, current: index == 0)
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
            messageText(itemVersion.msgContent.text, parseSimpleXMarkdown(itemVersion.msgContent.text), nil)
                .allowsHitTesting(false)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(ciDirFrameColor(chatItemSent: chatItem.chatDir.sent, colorScheme: colorScheme))
                .cornerRadius(18)
                .contextMenu {
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
            let ts = localTimestamp(itemVersion.itemVersionTs)
            (current ? Text("\(ts) (current)") : Text(ts))
            .foregroundStyle(.secondary)
            .font(.caption)
            .padding(.horizontal, 12)
        }
        .frame(maxWidth: maxWidth, alignment: .leading)
    }

    private func itemInfoShareText(_ chatItemInfo: ChatItemInfo) -> String {
        let meta = chatItem.meta
        var shareText: [String] = [title, ""]
        if developerTools {
            shareText += [String.localizedStringWithFormat(NSLocalizedString("Database ID: %d", comment: "copied message info"), meta.itemId) ]
        }
        shareText += [String.localizedStringWithFormat(NSLocalizedString("Sent at: %@", comment: "copied message info"), localTimestamp(meta.itemTs))]
        if !chatItem.chatDir.sent {
            shareText += [String.localizedStringWithFormat(NSLocalizedString("Received at: %@", comment: "copied message info"), localTimestamp(meta.createdAt))]
        }
        if let deleteAt = meta.itemTimed?.deleteAt {
            shareText += [String.localizedStringWithFormat(NSLocalizedString("To be deleted at: %@", comment: "copied message info"), localTimestamp(deleteAt))]
        }
        if !chatItemInfo.itemVersions.isEmpty {
            shareText += ["", NSLocalizedString("Edit history", comment: "copied message info"), ""]
            for (index, itemVersion) in chatItemInfo.itemVersions.enumerated() {
                shareText += [
                    String.localizedStringWithFormat(
                        index == 0 ? NSLocalizedString("%@ (current):", comment: "copied message info") : NSLocalizedString("%@:", comment: "copied message info"),
                        localTimestamp(itemVersion.itemVersionTs)
                    ),
                    itemVersion.msgContent.text,
                    ""
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
        ChatItemInfoView(chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"), chatItemInfo: Binding.constant(nil))
    }
}
