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
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss
    @EnvironmentObject var theme: AppTheme
    var ci: ChatItem
    @Binding var chatItemInfo: ChatItemInfo?
    @State private var selection: CIInfoTab = .history
    @State private var alert: CIInfoViewAlert? = nil
    @State private var messageStatusLimited: Bool = true
    @State private var fileStatusLimited: Bool = true
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    enum CIInfoTab {
        case history
        case quote
        case forwarded
        case delivery
    }

    enum CIInfoViewAlert: Identifiable {
        case alert(title: String, text: String)

        var id: String {
            switch self {
            case let .alert(title, text): return "alert \(title) \(text)"
            }
        }
    }
    
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
                .alert(item: $alert) { a in
                    switch(a) {
                    case let .alert(title, text): return Alert(title: Text(title), message: Text(text))
                    }
                }
        }
    }

    private var title: String {
        ci.localNote
        ? NSLocalizedString("Saved message", comment: "message info title")
        : ci.chatDir.sent
        ? NSLocalizedString("Sent message", comment: "message info title")
        : NSLocalizedString("Received message", comment: "message info title")
    }

    private var numTabs: Int {
        var numTabs = 1
        if chatItemInfo?.memberDeliveryStatuses != nil {
            numTabs += 1
        }
        if ci.quotedItem != nil {
            numTabs += 1
        }
        if chatItemInfo?.forwardedFromChatItem != nil {
            numTabs += 1
        }
        return numTabs
    }

    private var local: Bool {
        switch ci.chatDir {
        case .localSnd: true
        case .localRcv: true
        default: false
        }
    }

    @ViewBuilder private func itemInfoView() -> some View {
        if numTabs > 1 {
            TabView(selection: $selection) {
                if let mdss = chatItemInfo?.memberDeliveryStatuses {
                    deliveryTab(mdss)
                        .tabItem {
                            Label("Delivery", systemImage: "checkmark.message")
                        }
                        .tag(CIInfoTab.delivery)
                }
                historyTab()
                    .tabItem {
                        Label("History", systemImage: "clock")
                    }
                    .tag(CIInfoTab.history)
                    .modifier(ThemedBackground())
                if let qi = ci.quotedItem {
                    quoteTab(qi)
                        .tabItem {
                            Label("In reply to", systemImage: "arrowshape.turn.up.left")
                        }
                        .tag(CIInfoTab.quote)
                        .modifier(ThemedBackground())
                }
                if let forwardedFromItem = chatItemInfo?.forwardedFromChatItem {
                    forwardedFromTab(forwardedFromItem)
                        .tabItem {
                            Label(local ? "Saved" : "Forwarded", systemImage: "arrowshape.turn.up.forward")
                        }
                        .tag(CIInfoTab.forwarded)
                        .modifier(ThemedBackground())
                }
            }
            .onAppear {
                if chatItemInfo?.memberDeliveryStatuses != nil {
                    selection = .delivery
                }
            }
        } else {
            historyTab()
            .modifier(ThemedBackground())
        }
    }

    @ViewBuilder private func details() -> some View {
        let meta = ci.meta
        VStack(alignment: .leading, spacing: 16) {
            Text(title)
                .font(.largeTitle)
                .bold()
                .padding(.bottom)

            if ci.localNote {
                infoRow("Created at", localTimestamp(meta.itemTs))
            } else {
                infoRow("Sent at", localTimestamp(meta.itemTs))
            }
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
                let msv = infoRow("Message status", ci.meta.itemStatus.id)
                Group {
                    if messageStatusLimited {
                        msv.lineLimit(1)
                    } else {
                        msv
                    }
                }
                .onTapGesture {
                    withAnimation {
                        messageStatusLimited.toggle()
                    }
                }

                if let file = ci.file {
                    let fsv = infoRow("File status", file.fileStatus.id)
                    Group {
                        if fileStatusLimited {
                            fsv.lineLimit(1)
                        } else {
                            fsv
                        }
                    }
                    .onTapGesture {
                        withAnimation {
                            fileStatusLimited.toggle()
                        }
                    }
                }
            }
        }
    }

    @ViewBuilder private func historyTab() -> some View {
        GeometryReader { g in
            let maxWidth = (g.size.width - 32) * 0.84
            ScrollView {
                VStack(alignment: .leading, spacing: 16) {
                    details()
                    Divider().padding(.vertical)
                    if let chatItemInfo = chatItemInfo,
                       !chatItemInfo.itemVersions.isEmpty {
                        Text("History")
                            .font(.title2)
                            .padding(.bottom, 4)
                        LazyVStack(alignment: .leading, spacing: 16)  {
                            ForEach(Array(chatItemInfo.itemVersions.enumerated()), id: \.element.chatItemVersionId) { index, itemVersion in
                                itemVersionView(itemVersion, maxWidth, current: index == 0 && ci.meta.itemDeleted == nil)
                            }
                        }
                    }
                    else {
                        Text("No history")
                            .foregroundColor(theme.colors.secondary)
                            .frame(maxWidth: .infinity)
                    }
                }
                .padding()
            }
            .frame(maxHeight: .infinity, alignment: .top)
        }
    }

    @ViewBuilder private func itemVersionView(_ itemVersion: ChatItemVersion, _ maxWidth: CGFloat, current: Bool) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            textBubble(itemVersion.msgContent.text, itemVersion.formattedText, nil)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(chatItemFrameColor(ci, theme))
                .modifier(ChatItemClipped())
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

    @ViewBuilder private func textBubble(_ text: String, _ formattedText: [FormattedText]?, _ sender: String? = nil) -> some View {
        if text != "" {
            TextBubble(text: text, formattedText: formattedText, sender: sender)
        } else {
            Text("no text")
                .italic()
                .foregroundColor(theme.colors.secondary)
        }
    }

    private struct TextBubble: View {
        @EnvironmentObject var theme: AppTheme
        var text: String
        var formattedText: [FormattedText]?
        var sender: String? = nil
        @State private var showSecrets = false

        var body: some View {
            toggleSecrets(formattedText, $showSecrets, messageText(text, formattedText, sender, showSecrets: showSecrets, secondaryColor: theme.colors.secondary))
        }
    }

    @ViewBuilder private func quoteTab(_ qi: CIQuote) -> some View {
        GeometryReader { g in
            let maxWidth = (g.size.width - 32) * 0.84
            ScrollView {
                VStack(alignment: .leading, spacing: 16) {
                    details()
                    Divider().padding(.vertical)
                    Text("In reply to")
                        .font(.title2)
                        .padding(.bottom, 4)
                    quotedMsgView(qi, maxWidth)
                }
                .padding()
            }
            .frame(maxHeight: .infinity, alignment: .top)
        }
    }

    @ViewBuilder private func quotedMsgView(_ qi: CIQuote, _ maxWidth: CGFloat) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            textBubble(qi.text, qi.formattedText, qi.getSender(nil))
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(quotedMsgFrameColor(qi, theme))
                .modifier(ChatItemClipped())
                .contextMenu {
                    if qi.text != "" {
                        Button {
                            showShareSheet(items: [qi.text])
                        } label: {
                            Label("Share", systemImage: "square.and.arrow.up")
                        }
                        Button {
                            UIPasteboard.general.string = qi.text
                        } label: {
                            Label("Copy", systemImage: "doc.on.doc")
                        }
                    }
                }
            Text(localTimestamp(qi.sentAt))
                .foregroundStyle(.secondary)
                .font(.caption)
                .padding(.horizontal, 12)
        }
        .frame(maxWidth: maxWidth, alignment: .leading)
    }

    func quotedMsgFrameColor(_ qi: CIQuote, _ theme: AppTheme) -> Color {
        (qi.chatDir?.sent ?? false)
        ? theme.appColors.sentMessage
        : theme.appColors.receivedMessage
    }

    @ViewBuilder private func forwardedFromTab(_ forwardedFromItem: AChatItem) -> some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                details()
                Divider().padding(.vertical)
                Text(local ? "Saved from" : "Forwarded from")
                    .font(.title2)
                    .padding(.bottom, 4)
                forwardedFromView(forwardedFromItem)
            }
            .padding()
        }
        .frame(maxHeight: .infinity, alignment: .top)
    }

    private func forwardedFromView(_ forwardedFromItem: AChatItem) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            Button {
                Task {
                    await MainActor.run {
                        chatModel.chatId = forwardedFromItem.chatInfo.id
                        dismiss()
                    }
                }
            } label: {
                forwardedFromSender(forwardedFromItem)
            }

            if !local {
                Divider().padding(.top, 32)
                Text("Recipient(s) can't see who this message is from.")
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
        }
    }

    @ViewBuilder private func forwardedFromSender(_ forwardedFromItem: AChatItem) -> some View {
        HStack {
            ChatInfoImage(chat: Chat(chatInfo: forwardedFromItem.chatInfo), size: 48)
                .padding(.trailing, 6)

            if forwardedFromItem.chatItem.chatDir.sent {
                VStack(alignment: .leading) {
                    Text("you")
                        .italic()
                        .foregroundColor(theme.colors.onBackground)
                    Text(forwardedFromItem.chatInfo.chatViewName)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(1)
                }
            } else if case let .groupRcv(groupMember) = forwardedFromItem.chatItem.chatDir {
                VStack(alignment: .leading) {
                    Text(groupMember.chatViewName)
                        .foregroundColor(theme.colors.onBackground)
                        .lineLimit(1)
                    Text(forwardedFromItem.chatInfo.chatViewName)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(1)
                }
            } else {
                Text(forwardedFromItem.chatInfo.chatViewName)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)
            }
        }
    }

    @ViewBuilder private func deliveryTab(_ memberDeliveryStatuses: [MemberDeliveryStatus]) -> some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                details()
                Divider().padding(.vertical)
                Text("Delivery")
                    .font(.title2)
                    .padding(.bottom, 4)
                memberDeliveryStatusesView(memberDeliveryStatuses)
            }
            .padding()
        }
        .frame(maxHeight: .infinity, alignment: .top)
    }

    @ViewBuilder private func memberDeliveryStatusesView(_ memberDeliveryStatuses: [MemberDeliveryStatus]) -> some View {
        VStack(alignment: .leading, spacing: 12) {
            let mss = membersStatuses(memberDeliveryStatuses)
            if !mss.isEmpty {
                ForEach(mss, id: \.0.groupMemberId) { memberStatus in
                    memberDeliveryStatusView(memberStatus.0, memberStatus.1, memberStatus.2)
                }
            } else {
                Text("No delivery information")
                    .foregroundColor(theme.colors.secondary)
            }
        }
    }

    private func membersStatuses(_ memberDeliveryStatuses: [MemberDeliveryStatus]) -> [(GroupMember, GroupSndStatus, Bool?)] {
        memberDeliveryStatuses.compactMap({ mds in
            if let mem = chatModel.getGroupMember(mds.groupMemberId) {
                return (mem.wrapped, mds.memberDeliveryStatus, mds.sentViaProxy)
            } else {
                return nil
            }
        })
    }

    private func memberDeliveryStatusView(_ member: GroupMember, _ status: GroupSndStatus, _ sentViaProxy: Bool?) -> some View {
        HStack{
            ProfileImage(imageStr: member.image, size: 30)
                .padding(.trailing, 2)
            Text(member.chatViewName)
                .lineLimit(1)
            Spacer()
            if sentViaProxy == true {
                Image(systemName: "arrow.forward")
                    .foregroundColor(theme.colors.secondary).opacity(0.67)
            }
            let v = Group {
                if let (icon, statusColor) = status.statusIcon(theme.colors.secondary, theme.colors.primary) {
                    switch status {
                    case .rcvd:
                        ZStack(alignment: .trailing) {
                            Image(systemName: icon)
                                .foregroundColor(statusColor.opacity(0.67))
                                .padding(.trailing, 6)
                            Image(systemName: icon)
                                .foregroundColor(statusColor.opacity(0.67))
                        }
                    default:
                        Image(systemName: icon)
                            .foregroundColor(statusColor)
                    }
                } else {
                    Image(systemName: "ellipsis")
                        .foregroundColor(Color.secondary)
                }
            }

            if let (title, text) = status.statusInfo {
                v.onTapGesture {
                    alert = .alert(title: title, text: text)
                }
            } else {
                v
            }
        }
    }

    private func itemInfoShareText() -> String {
        let meta = ci.meta
        var shareText: [String] = [String.localizedStringWithFormat(NSLocalizedString("# %@", comment: "copied message info title, # <title>"), title), ""]
        shareText += [String.localizedStringWithFormat(
            ci.localNote
                ? NSLocalizedString("Created at: %@", comment: "copied message info")
                : NSLocalizedString("Sent at: %@", comment: "copied message info"),
            localTimestamp(meta.itemTs))
        ]
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
                String.localizedStringWithFormat(NSLocalizedString("Record updated at: %@", comment: "copied message info"), localTimestamp(meta.updatedAt)),
                String.localizedStringWithFormat(NSLocalizedString("Message status: %@", comment: "copied message info"), meta.itemStatus.id)
            ]
            if let file = ci.file {
                shareText += [String.localizedStringWithFormat(NSLocalizedString("File status: %@", comment: "copied message info"), file.fileStatus.id)]
            }
        }
        if let qi = ci.quotedItem {
            shareText += ["", NSLocalizedString("## In reply to", comment: "copied message info")]
            let t = qi.text
            shareText += [""]
            if let sender = qi.getSender(nil) {
                shareText += [String.localizedStringWithFormat(
                    NSLocalizedString("%@ at %@:", comment: "copied message info, <sender> at <time>"),
                    sender,
                    localTimestamp(qi.sentAt)
                )]
            } else {
                shareText += [String.localizedStringWithFormat(
                    NSLocalizedString("%@:", comment: "copied message info"),
                    localTimestamp(qi.sentAt)
                )]
            }
            shareText += [t != "" ? t : NSLocalizedString("no text", comment: "copied message info in history")]
        }
        if let chatItemInfo = chatItemInfo,
           !chatItemInfo.itemVersions.isEmpty {
            shareText += ["", NSLocalizedString("## History", comment: "copied message info")]
            for (index, itemVersion) in chatItemInfo.itemVersions.enumerated() {
                let t = itemVersion.msgContent.text
                shareText += [
                    "",
                    String.localizedStringWithFormat(
                        index == 0 && ci.meta.itemDeleted == nil
                        ? NSLocalizedString("%@ (current):", comment: "copied message info")
                        : NSLocalizedString("%@:", comment: "copied message info"),
                        localTimestamp(itemVersion.itemVersionTs)
                    ),
                    t != "" ? t : NSLocalizedString("no text", comment: "copied message info in history")
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
