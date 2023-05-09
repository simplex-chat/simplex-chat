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
    var chatItemSent: Bool
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
                    if developerTools {
                        infoRow("Database ID", "\(chatItemInfo.chatItemId)")
                    }
                    infoRow("Sent at", localTimestamp(chatItemInfo.itemTs))
                    if !chatItemSent {
                        infoRow("Received at", localTimestamp(chatItemInfo.createdAt))
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
        let uiMenu: Binding<UIMenu> = Binding(
            get: { UIMenu(title: "", children: itemVersionMenu(itemVersion)) },
            set: { _ in }
        )
        VStack(alignment: .leading, spacing: 4) {
            ExpandableText(itemVersion.msgContent.text, lineLimit: 3)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(ciDirFrameColor(chatItemSent: chatItemSent, colorScheme: colorScheme))
                .cornerRadius(18)
                // .uiKitContextMenu(menu: uiMenu)
            Text(localTimestamp(itemVersion.itemVersionTs) + (current ? " (Current)" : ""))
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
        if developerTools {
            shareText += "Database ID: \(chatItemInfo.chatItemId)" + nl
        }
        shareText += "Sent at: \(localTimestamp(chatItemInfo.itemTs))" + nl
        if !chatItemSent {
            shareText += "Received at: \(localTimestamp(chatItemInfo.createdAt))" + nl
        }
        if !chatItemInfo.itemVersions.isEmpty {
            shareText += nl + "Edit history" + nl + nl
            for (index, itemVersion) in chatItemInfo.itemVersions.enumerated() {
                shareText += localTimestamp(itemVersion.itemVersionTs) + (index == 0 ? " (Current)" : "") + ":" + nl
                shareText += itemVersion.msgContent.text + nl + nl
            }
        }
        return shareText.trimmingCharacters(in: .newlines)
    }
}

struct ExpandableText: View {
    @State private var expanded: Bool = false
    @State private var truncated: Bool = false
    @State private var shrinkText: String

    private var text: String
    let font: UIFont
    let lineLimit: Int

    init(_ text: String, lineLimit: Int, font: UIFont = UIFont.preferredFont(forTextStyle: UIFont.TextStyle.body)) {
        self.text = text
        _shrinkText = State(wrappedValue: text)
        self.lineLimit = lineLimit
        self.font = font
    }

    var body: some View {
        ZStack(alignment: .bottomLeading) {
            Group {
                let t = self.expanded ? text : shrinkText
                messageText(t, parseSimpleXMarkdown(t), nil)
                + moreLessText()
            }
            .lineLimit(expanded ? nil : lineLimit)
            .background(
                Text(text)
                    .lineLimit(lineLimit)
                    .background(GeometryReader { visibleTextGeometry in
                        Color.clear.onAppear() {
                            let size = CGSize(width: visibleTextGeometry.size.width, height: .greatestFiniteMagnitude)
                            let attributes:[NSAttributedString.Key:Any] = [NSAttributedString.Key.font: font]

                            var low  = 0
                            var high = shrinkText.count
                            var mid = high
                            while ((high - low) > 1) {
                                let attributedText = NSAttributedString(string: shrinkText + moreLessStr, attributes: attributes)
                                let boundingRect = attributedText.boundingRect(with: size, options: NSStringDrawingOptions.usesLineFragmentOrigin, context: nil)
                                if boundingRect.size.height > visibleTextGeometry.size.height {
                                    truncated = true
                                    high = mid
                                    mid = (high + low) / 2
                                } else {
                                    if mid == text.count {
                                        break
                                    } else {
                                        low = mid
                                        mid = (low + high) / 2
                                    }
                                }
                                shrinkText = String(text.prefix(mid)).trimmingCharacters(in: .whitespaces)
                            }
                        }
                    })
                    .hidden()
            )
            .font(Font(font))
            if truncated {
                Button(action: {
                    expanded.toggle()
                }, label: {
                    HStack {
                        Spacer()
                        Text("")
                    }.opacity(0)
                })
            }
        }
    }

    private var moreLessStr: String {
        if !truncated {
            return ""
        } else {
            return self.expanded
            ? "\n" + NSLocalizedString("Read less", comment: "long text button")
            : "… " + NSLocalizedString("Read more", comment: "long text button")
        }
    }

    private func moreLessText() -> Text {
        if !truncated {
            return Text("")
        } else {
            return self.expanded
            ? Text("\n") + Text("Read less").foregroundColor(.accentColor)
            : Text("… ") + Text("Read more").foregroundColor(.accentColor)
        }
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
        ChatItemInfoView(chatItemSent: true, chatItemInfo: Binding.constant(nil))
    }
}
