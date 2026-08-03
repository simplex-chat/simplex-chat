import AppKit
import SwiftUI

struct ConversationView: View {
    @ObservedObject var model: AppModel
    @FocusState private var composerFocused: Bool
    @FocusState private var transcriptFocused: Bool
    @State private var dropTargeted = false

    var body: some View {
        Group {
            if let chat = model.selectedChat {
                VStack(spacing: 0) {
                    if model.conversationSearchPresented {
                        ConversationSearchBar(model: model)
                        Divider()
                    }
                    transcript(chat: chat)
                    Divider()
                    composer(chat: chat)
                }
                .background(Color(nsColor: .textBackgroundColor))
                .navigationTitle("")
                .toolbar { conversationToolbar(chat: chat) }
                .dropDestination(for: URL.self) { urls, _ in
                    model.stageAttachments(urls)
                    composerFocused = true
                    return !urls.isEmpty
                } isTargeted: { dropTargeted = $0 }
                .overlay {
                    if dropTargeted {
                        DropTargetOverlay()
                    }
                }
                .confirmationDialog(
                    "Delete selected messages?",
                    isPresented: $model.showingDeleteConfirmation,
                    titleVisibility: .visible
                ) {
                    Button("Delete Locally", role: .destructive, action: model.deleteSelectedMessages)
                    Button("Cancel", role: .cancel) {}
                } message: {
                    Text("This removes the selected messages from this Mac. It does not delete them for other people.")
                }
                .alert("Couldn’t Add Attachment", isPresented: Binding(
                    get: { model.attachmentError != nil },
                    set: { if !$0 { model.attachmentError = nil } }
                )) {
                    Button("OK") { model.attachmentError = nil }
                } message: {
                    Text(model.attachmentError ?? "")
                }
                .alert("Couldn’t Open Attachment", isPresented: Binding(
                    get: { model.attachmentOpenError != nil },
                    set: { if !$0 { model.attachmentOpenError = nil } }
                )) {
                    Button("OK") { model.attachmentOpenError = nil }
                } message: {
                    Text(model.attachmentOpenError ?? "")
                }
            } else {
                ContentUnavailableView {
                    Label("No Conversation Selected", systemImage: "bubble.left.and.bubble.right")
                } description: {
                    Text("Choose a chat from the sidebar to start messaging.")
                }
            }
        }
        .onChange(of: transcriptFocused) { _, focused in
            model.transcriptFocused = focused
        }
        .onChange(of: composerFocused) { _, focused in
            if focused { model.transcriptFocused = false }
        }
        .onChange(of: model.composerFocusRequest) { _, _ in
            composerFocused = true
        }
    }

    @ToolbarContentBuilder
    private func conversationToolbar(chat: NativeChat) -> some ToolbarContent {
        ToolbarItem(placement: .principal) {
            HStack(spacing: 8) {
                ProfileAvatar(image: chat.image, name: chat.displayName, size: 28)
                    .accessibilityHidden(true)
                VStack(alignment: .leading, spacing: 0) {
                    Text(chat.displayName)
                        .font(.headline)
                        .lineLimit(1)
                    Text(chat.kind.toolbarSubtitle)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            .accessibilityElement(children: .combine)
            .accessibilityLabel("Conversation with \(chat.displayName)")
        }

        ToolbarItem(placement: .primaryAction) {
            Button(action: model.refresh) {
                Image(systemName: "arrow.clockwise")
            }
            .help("Refresh Conversation")
            .accessibilityLabel("Refresh Conversation") // [VERIFY] Matches the visible tooltip.
            .accessibilityInputLabels(["Refresh Conversation", "Refresh"])
        }
    }

    private func transcript(chat: NativeChat) -> some View {
        ScrollViewReader { proxy in
            ScrollView {
                LazyVStack(spacing: model.density.tokens.transcriptGap) {
                    ForEach(Array(model.messages.enumerated()), id: \.element.id) { index, message in
                        if startsNewDay(at: index), let timestamp = message.timestamp {
                            TranscriptDateHeader(date: timestamp)
                        }
                        MessageRow(
                            message: message,
                            chat: chat,
                            selected: model.selectedMessageIDs.contains(message.id),
                            density: model.density,
                            startsGroup: startsGroup(at: index),
                            endsGroup: endsGroup(at: index),
                            openingAttachment: model.openingAttachmentIDs.contains(message.id),
                            canReply: chat.kind.canReply && !model.isSending
                        ) {
                            transcriptFocused = true
                            model.selectMessage(message.id, modifiers: NSApp.currentEvent?.modifierFlags ?? [])
                        } copy: {
                            transcriptFocused = true
                            model.selectMessage(message.id, modifiers: [])
                            model.copySelectedMessages()
                        } delete: {
                            transcriptFocused = true
                            model.selectMessage(message.id, modifiers: [])
                            model.requestDeleteSelectedMessages()
                        } reply: {
                            model.beginReply(to: message)
                            composerFocused = true
                        } openQuote: { messageID in
                            model.openQuotedMessage(messageID)
                        } openAttachment: {
                            model.openAttachment(message)
                        }
                        .id(message.id)
                    }
                }
                .padding(.horizontal, 20)
                .padding(.vertical, 16)
            }
            .defaultScrollAnchor(.bottom)
            .background(Color(nsColor: .textBackgroundColor))
            .focusable()
            .focused($transcriptFocused)
            .focusEffectDisabled()
            .overlay {
                if model.isLoadingConversation { ProgressView() }
            }
            .onChange(of: model.messages.last?.id) { _, lastID in
                guard let lastID else { return }
                proxy.scrollTo(lastID, anchor: .bottom)
            }
            .onChange(of: model.selectedMessagesInTranscriptOrder.last?.id) { _, selectedID in
                guard let selectedID else { return }
                proxy.scrollTo(selectedID, anchor: .center)
            }
            .onChange(of: model.targetMessageID) { _, targetID in
                guard let targetID else { return }
                proxy.scrollTo(targetID, anchor: .center)
                model.targetMessageID = nil
            }
            .onKeyPress(.upArrow) {
                model.moveMessageSelection(by: -1)
                return .handled
            }
            .onKeyPress(.downArrow) {
                model.moveMessageSelection(by: 1)
                return .handled
            }
            .onKeyPress(.delete) {
                model.requestDeleteSelectedMessages()
                return model.selectedMessageIDs.isEmpty ? .ignored : .handled
            }
            .onKeyPress(.escape) {
                model.dismissNearestState()
                return .handled
            }
        }
    }

    private func startsNewDay(at index: Int) -> Bool {
        guard index > 0 else { return true }
        guard let current = model.messages[index].timestamp,
              let previous = model.messages[index - 1].timestamp else { return false }
        return !Calendar.current.isDate(current, inSameDayAs: previous)
    }

    private func startsGroup(at index: Int) -> Bool {
        guard index > 0 else { return true }
        return !messagesBelongToSameGroup(model.messages[index - 1], model.messages[index])
    }

    private func endsGroup(at index: Int) -> Bool {
        guard index + 1 < model.messages.count else { return true }
        return !messagesBelongToSameGroup(model.messages[index], model.messages[index + 1])
    }

    private func messagesBelongToSameGroup(_ first: NativeMessage, _ second: NativeMessage) -> Bool {
        guard first.sent == second.sent, first.author == second.author,
              let firstDate = first.timestamp, let secondDate = second.timestamp else { return false }
        return abs(secondDate.timeIntervalSince(firstDate)) <= 5 * 60
    }

    private func composer(chat: NativeChat) -> some View {
        VStack(spacing: 0) {
            if let message = model.replyingTo {
                ReplyContextBar(message: message, chat: chat, cancel: model.cancelReply)
                Divider()
            }

            if !model.pendingAttachments.isEmpty {
                AttachmentTray(model: model)
                Divider()
            }

            HStack(alignment: .bottom, spacing: 8) {
                Menu {
                    Button("Choose Files…", action: model.chooseAttachments)
                    Button("Paste Files", action: model.stageFilesFromPasteboard)
                        .keyboardShortcut("v", modifiers: [.command, .option])
                } label: {
                    Image(systemName: "paperclip")
                        .frame(width: 32, height: 32)
                        .contentShape(Rectangle())
                }
                .menuStyle(.borderlessButton)
                .fixedSize()
                .help("Add Attachments")
                .accessibilityLabel("Attachments") // [VERIFY] Matches the attachment menu action.
                .accessibilityInputLabels(["Attachments", "Attach Files"])

                TextField("Message", text: $model.draft, axis: .vertical)
                    .textFieldStyle(.plain)
                    .lineLimit(1...8)
                    .focused($composerFocused)
                    .padding(.horizontal, 12)
                    .padding(.vertical, model.density.tokens.composerPadding)
                    .background(Color(nsColor: .textBackgroundColor), in: RoundedRectangle(cornerRadius: 10))
                    .overlay {
                        RoundedRectangle(cornerRadius: 10)
                            .stroke(
                                composerFocused ? Color.accentColor : Color(nsColor: .separatorColor),
                                lineWidth: composerFocused ? 2 : 1
                            )
                    }
                    .onSubmit(model.sendDraft)

                Button(action: model.sendDraft) {
                    ZStack {
                        Circle()
                            .fill(model.canSendDraft ? Color.accentColor : Color(nsColor: .tertiaryLabelColor))
                        if model.isSending {
                            ProgressView()
                                .controlSize(.small)
                                .tint(Color(nsColor: .selectedControlTextColor))
                        } else {
                            Image(systemName: "arrow.up")
                                .font(.body.weight(.bold))
                                .foregroundStyle(Color(nsColor: .selectedControlTextColor))
                        }
                    }
                    .frame(width: 32, height: 32)
                }
                .buttonStyle(.plain)
                .disabled(!model.canSendDraft || !chat.kind.canSend)
                .help("Send Message")
                .accessibilityLabel("Send Message") // [VERIFY] Matches the visible tooltip.
                .accessibilityInputLabels(["Send Message", "Send"])
            }
            .padding(12)
        }
        .background(Color(nsColor: .windowBackgroundColor))
    }
}

private struct ConversationSearchBar: View {
    @ObservedObject var model: AppModel
    @FocusState private var searchFocused: Bool

    var body: some View {
        HStack(spacing: 8) {
            Image(systemName: "magnifyingglass")
                .foregroundStyle(.secondary)
                .accessibilityHidden(true)

            TextField("Search Conversation", text: $model.conversationSearchText)
                .textFieldStyle(.roundedBorder)
                .focused($searchFocused)
                .frame(maxWidth: 320)
                .onSubmit { model.moveConversationSearchResult(by: 1) }

            Text(model.conversationSearchResultDescription)
                .font(.caption)
                .foregroundStyle(.secondary)
                .frame(minWidth: 64, alignment: .leading)

            Button {
                model.moveConversationSearchResult(by: -1)
            } label: {
                Image(systemName: "chevron.up")
            }
            .disabled(model.conversationSearchMatches.isEmpty)
            .help("Previous Result")
            .accessibilityLabel("Previous Result") // [VERIFY] Matches the visible tooltip.
            .accessibilityInputLabels(["Previous Result", "Previous"])

            Button {
                model.moveConversationSearchResult(by: 1)
            } label: {
                Image(systemName: "chevron.down")
            }
            .disabled(model.conversationSearchMatches.isEmpty)
            .help("Next Result")
            .accessibilityLabel("Next Result") // [VERIFY] Matches the visible tooltip.
            .accessibilityInputLabels(["Next Result", "Next"])

            Button(action: model.dismissConversationSearch) {
                Image(systemName: "xmark")
            }
            .help("Close Search")
            .accessibilityLabel("Close Search") // [VERIFY] Matches the visible tooltip.
            .accessibilityInputLabels(["Close Search", "Close"])

            Spacer(minLength: 0)
        }
        .buttonStyle(.borderless)
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .background(Color(nsColor: .windowBackgroundColor))
        .onAppear { searchFocused = true }
        .onChange(of: model.conversationSearchText) { _, _ in
            model.updateConversationSearchSelection()
        }
        .onKeyPress(.escape) {
            model.dismissConversationSearch()
            return .handled
        }
    }
}

private struct MessageRow: View {
    let message: NativeMessage
    let chat: NativeChat
    let selected: Bool
    let density: DesktopChatDensity
    let startsGroup: Bool
    let endsGroup: Bool
    let openingAttachment: Bool
    let canReply: Bool
    let select: () -> Void
    let copy: () -> Void
    let delete: () -> Void
    let reply: () -> Void
    let openQuote: (Int64) -> Void
    let openAttachment: () -> Void

    var body: some View {
        HStack(alignment: .bottom, spacing: 8) {
            if !message.sent {
                Group {
                    if endsGroup {
                        ProfileAvatar(image: chat.image, name: message.author ?? chat.displayName, size: 28)
                    } else {
                        Color.clear
                    }
                }
                .frame(width: 28, height: 28)
                .accessibilityHidden(true)
            }

            if message.sent { Spacer(minLength: 80) }

            VStack(alignment: message.sent ? .trailing : .leading, spacing: 4) {
                if startsGroup, !message.sent, let author = message.author, chat.kind == .group {
                    Text(author)
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                        .padding(.leading, 8)
                }

                MessageContentView(
                    message: message,
                    chatName: chat.displayName,
                    openingAttachment: openingAttachment,
                    openQuote: openQuote,
                    openAttachment: openAttachment
                )
                    .padding(.horizontal, 12)
                    .padding(.vertical, density.tokens.messagePadding)
                    .background(bubbleBackground, in: bubbleShape)
                    .foregroundStyle(message.sent ? Color(nsColor: .selectedControlTextColor) : Color.primary)
                    .overlay {
                        if selected {
                            bubbleShape
                                .stroke(Color.accentColor, lineWidth: 2)
                                .padding(-2)
                        }
                    }
                    .contentShape(bubbleShape)
                    .onTapGesture(perform: select)
                    .focusEffectDisabled()

                if endsGroup, let timestamp = message.timestamp {
                    Text(timestamp, format: .dateTime.hour().minute())
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                        .padding(.horizontal, 8)
                }
            }
            .frame(maxWidth: 520, alignment: message.sent ? .trailing : .leading)
            .accessibilityElement(children: .combine)
            .accessibilityAddTraits(selected ? [.isSelected, .isButton] : .isButton)
            .accessibilityActions {
                Button(selected ? "Deselect Message" : "Select Message", action: select)
                if canReply {
                    Button("Reply", action: reply)
                }
            }

            if !message.sent { Spacer(minLength: 80) }
        }
        .contextMenu {
            if canReply {
                Button("Reply", action: reply)
                Divider()
            }
            Button("Copy", action: copy)
            Button(selected ? "Deselect Message" : "Select Message", action: select)
            if message.deletable {
                Divider()
                Button("Delete…", role: .destructive, action: delete)
            }
        }
    }

    private var bubbleBackground: Color {
        message.sent
            ? Color(nsColor: .selectedContentBackgroundColor)
            : Color(nsColor: .unemphasizedSelectedContentBackgroundColor)
    }

    private var bubbleShape: UnevenRoundedRectangle {
        if message.sent {
            UnevenRoundedRectangle(
                topLeadingRadius: 16,
                bottomLeadingRadius: 16,
                bottomTrailingRadius: endsGroup ? 4 : 16,
                topTrailingRadius: 16
            )
        } else {
            UnevenRoundedRectangle(
                topLeadingRadius: 16,
                bottomLeadingRadius: endsGroup ? 4 : 16,
                bottomTrailingRadius: 16,
                topTrailingRadius: 16
            )
        }
    }
}

private struct TranscriptDateHeader: View {
    let date: Date

    var body: some View {
        Text(label)
            .font(.caption.weight(.medium))
            .foregroundStyle(.secondary)
            .frame(maxWidth: .infinity)
            .padding(.vertical, 8)
            .accessibilityAddTraits(.isHeader)
    }

    private var label: String {
        if Calendar.current.isDateInToday(date) { return "Today" }
        if Calendar.current.isDateInYesterday(date) { return "Yesterday" }
        return date.formatted(.dateTime.month(.abbreviated).day().year())
    }
}

private struct ReplyContextBar: View {
    let message: NativeMessage
    let chat: NativeChat
    let cancel: () -> Void

    var body: some View {
        HStack(spacing: 8) {
            Rectangle()
                .fill(.tint)
                .frame(width: 4)
                .accessibilityHidden(true)

            VStack(alignment: .leading, spacing: 4) {
                Text("Replying to \(sender)")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.tint)
                Text(preview)
                    .font(.callout)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
            }
            .accessibilityElement(children: .combine)

            Spacer(minLength: 8)

            Button(action: cancel) {
                Image(systemName: "xmark")
            }
            .buttonStyle(.borderless)
            .help("Cancel Reply")
            .accessibilityLabel("Cancel Reply") // [VERIFY] Matches the visible tooltip.
            .accessibilityInputLabels(["Cancel Reply", "Cancel"])
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .background(Color(nsColor: .windowBackgroundColor))
    }

    private var sender: String {
        message.sent ? "You" : (message.author ?? chat.displayName)
    }

    private var preview: String {
        message.text.isEmpty ? (message.content.attachmentDescription ?? "Message") : message.text
    }
}

private struct AttachmentTray: View {
    @ObservedObject var model: AppModel

    var body: some View {
        ScrollView(.horizontal) {
            HStack(spacing: 8) {
                ForEach(Array(model.pendingAttachments.enumerated()), id: \.element.id) { index, attachment in
                    AttachmentCard(
                        attachment: attachment,
                        canMoveEarlier: index > 0,
                        canMoveLater: index + 1 < model.pendingAttachments.count
                    ) {
                        model.removeAttachment(attachment.id)
                    } moveEarlier: {
                        model.moveAttachment(attachment.id, by: -1)
                    } moveLater: {
                        model.moveAttachment(attachment.id, by: 1)
                    }
                    .draggable(attachment.id.uuidString)
                    .dropDestination(for: String.self) { values, _ in
                        guard let value = values.first, let source = UUID(uuidString: value) else { return false }
                        model.reorderAttachment(source, before: attachment.id)
                        return true
                    }
                    .accessibilityActions {
                        if index > 0 {
                            Button("Move Earlier") { model.moveAttachment(attachment.id, by: -1) }
                        }
                        if index + 1 < model.pendingAttachments.count {
                            Button("Move Later") { model.moveAttachment(attachment.id, by: 1) }
                        }
                        Button("Remove Attachment") { model.removeAttachment(attachment.id) }
                    }
                }
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 8)
        }
        .scrollIndicators(.never)
        .accessibilityLabel("Pending Attachments")
    }
}

private struct AttachmentCard: View {
    let attachment: PendingAttachment
    let canMoveEarlier: Bool
    let canMoveLater: Bool
    let remove: () -> Void
    let moveEarlier: () -> Void
    let moveLater: () -> Void

    var body: some View {
        HStack(spacing: 8) {
            Group {
                if let image = NativeChatParser.image(from: attachment.previewImage) {
                    Image(nsImage: image)
                        .resizable()
                        .scaledToFill()
                        .accessibilityLabel("Preview of \(attachment.fileName)")
                        .accessibilityIgnoresInvertColors()
                } else {
                    Image(systemName: attachment.kind.symbolName)
                        .foregroundStyle(.secondary)
                        .accessibilityHidden(true)
                }
            }
            .frame(width: 32, height: 32)
            .clipShape(RoundedRectangle(cornerRadius: 4))

            VStack(alignment: .leading, spacing: 4) {
                Text(attachment.fileName)
                    .lineLimit(1)
                Text(attachment.byteCount, format: .byteCount(style: .file))
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            Button(action: remove) {
                Image(systemName: "xmark.circle.fill")
            }
            .buttonStyle(.plain)
            .foregroundStyle(.secondary)
            .help("Remove \(attachment.fileName)")
            .accessibilityLabel("Remove \(attachment.fileName)") // [VERIFY] Uses the visible file name.
            .accessibilityInputLabels(["Remove \(attachment.fileName)", "Remove Attachment"])
        }
        .padding(8)
        .frame(width: 224, alignment: .leading)
        .background(.quaternary, in: RoundedRectangle(cornerRadius: 10))
        .accessibilityElement(children: .contain)
        .contextMenu {
            Button("Move Earlier", action: moveEarlier)
                .disabled(!canMoveEarlier)
            Button("Move Later", action: moveLater)
                .disabled(!canMoveLater)
            Divider()
            Button("Remove Attachment", role: .destructive, action: remove)
        }
    }
}

private struct MessageContentView: View {
    let message: NativeMessage
    let chatName: String
    let openingAttachment: Bool
    let openQuote: (Int64) -> Void
    let openAttachment: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            if let quote = message.quotedItem {
                QuotedMessagePreview(
                    quote: quote,
                    chatName: chatName,
                    outgoing: message.sent,
                    open: openQuote
                )
            }

            switch message.content {
            case .text:
                messageText
            case .image(let preview, let fileName):
                mediaPreview(preview: preview, fileName: fileName ?? "Image", video: false)
                if !message.text.isEmpty { messageText }
            case .video(let preview, let fileName):
                mediaPreview(preview: preview, fileName: fileName ?? "Video", video: true)
                if !message.text.isEmpty { messageText }
            case .file(let fileName):
                fileAttachment(name: fileName ?? message.text)
                if !message.text.isEmpty, message.text != fileName { messageText }
            }
        }
    }

    private var messageText: some View {
        Text(message.text)
            .textSelection(.enabled)
    }

    @ViewBuilder
    private func mediaPreview(preview: String?, fileName: String, video: Bool) -> some View {
        let previewView = ZStack {
            if let image = NativeChatParser.image(from: preview) {
                Image(nsImage: image)
                    .resizable()
                    .scaledToFit()
                    .accessibilityIgnoresInvertColors()
            } else {
                Rectangle()
                    .fill(.quaternary)
                    .aspectRatio(4 / 3, contentMode: .fit)
                    .overlay {
                        Image(systemName: video ? "film" : "photo")
                            .font(.title)
                            .foregroundStyle(.secondary)
                    }
            }
            if video {
                Image(systemName: "play.circle.fill")
                    .font(.largeTitle)
                    .symbolRenderingMode(.hierarchical)
                    .foregroundStyle(.white)
                    .shadow(radius: 4)
                    .accessibilityHidden(true)
            }
            if openingAttachment {
                ProgressView()
                    .controlSize(.small)
                    .accessibilityLabel("Decrypting Attachment")
            }
        }
        .frame(maxWidth: 420, maxHeight: 320)
        .clipShape(RoundedRectangle(cornerRadius: 8))

        if attachmentExists {
            Button(action: openAttachment) {
                previewView
            }
            .buttonStyle(.plain)
            .disabled(openingAttachment)
            .help("Open \(fileName)")
            .accessibilityLabel("Open \(fileName)") // [VERIFY] Uses the attachment file name.
            .accessibilityInputLabels(["Open \(fileName)", "Open Attachment"])
        } else {
            previewView
                .accessibilityLabel(video ? "Video attachment, \(fileName)" : "Image attachment, \(fileName)")
        }
    }

    @ViewBuilder
    private func fileAttachment(name: String) -> some View {
        if attachmentExists {
            Button(action: openAttachment) {
                Label(name.isEmpty ? "File" : name, systemImage: "doc")
                if openingAttachment {
                    ProgressView()
                        .controlSize(.small)
                        .accessibilityLabel("Decrypting Attachment")
                }
            }
            .buttonStyle(.link)
            .disabled(openingAttachment)
            .help("Open \(name)")
        } else {
            Label(name.isEmpty ? "File" : name, systemImage: "doc")
                .foregroundStyle(.secondary)
        }
    }

    private var attachmentExists: Bool {
        guard let source = message.fileSource else { return false }
        return FileManager.default.fileExists(atPath: source.sourceURL.path)
    }
}

private struct QuotedMessagePreview: View {
    let quote: NativeQuote
    let chatName: String
    let outgoing: Bool
    let open: (Int64) -> Void

    var body: some View {
        Group {
            if let messageID = quote.messageID {
                Button { open(messageID) } label: { content }
                    .buttonStyle(.plain)
                    .help("Show Quoted Message")
                    .accessibilityHint("Moves to the original message in this conversation.")
            } else {
                content
            }
        }
    }

    private var content: some View {
        HStack(spacing: 8) {
            Rectangle()
                .fill(outgoing ? Color(nsColor: .selectedControlTextColor) : Color.accentColor)
                .frame(width: 4)
                .accessibilityHidden(true)

            VStack(alignment: .leading, spacing: 4) {
                Text(sender)
                    .font(.caption.weight(.semibold))
                Text(quote.text)
                    .font(.callout)
                    .lineLimit(2)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .accessibilityElement(children: .combine)
        .accessibilityLabel("Quoted message from \(sender): \(quote.text)")
    }

    private var sender: String {
        quote.author ?? (quote.sent ? "You" : chatName)
    }
}

private struct DropTargetOverlay: View {
    @Environment(\.accessibilityReduceTransparency) private var reduceTransparency

    var body: some View {
        Label("Drop files to attach", systemImage: "tray.and.arrow.down.fill")
            .font(.headline)
            .padding(24)
            .background {
                RoundedRectangle(cornerRadius: 12)
                    .fill(reduceTransparency
                        ? AnyShapeStyle(Color(nsColor: .windowBackgroundColor))
                        : AnyShapeStyle(.regularMaterial))
            }
            .overlay {
                RoundedRectangle(cornerRadius: 12)
                    .stroke(.tint, style: StrokeStyle(lineWidth: 2, dash: [8, 4]))
            }
            .accessibilityAddTraits(.isStaticText)
    }
}
