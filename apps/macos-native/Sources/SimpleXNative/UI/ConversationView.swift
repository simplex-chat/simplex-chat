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
                    transcript
                    Divider()
                    composer(chat: chat)
                }
                .navigationTitle(chat.displayName)
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
            } else {
                ContentUnavailableView("Select a conversation", systemImage: "bubble.left.and.bubble.right")
            }
        }
        .onChange(of: transcriptFocused) { _, focused in
            model.transcriptFocused = focused
        }
        .onChange(of: composerFocused) { _, focused in
            if focused { model.transcriptFocused = false }
        }
    }

    private var transcript: some View {
        ScrollViewReader { proxy in
            ScrollView {
                LazyVStack(spacing: model.density.tokens.transcriptGap) {
                    ForEach(model.messages) { message in
                        MessageRow(
                            message: message,
                            selected: model.selectedMessageIDs.contains(message.id),
                            density: model.density
                        ) {
                            transcriptFocused = true
                            model.selectMessage(message.id, modifiers: NSApp.currentEvent?.modifierFlags ?? [])
                        }
                        .id(message.id)
                    }
                }
                .padding(16)
            }
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

    private func composer(chat: NativeChat) -> some View {
        VStack(spacing: 0) {
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
                    Label("Attachments", systemImage: "paperclip")
                        .labelStyle(.iconOnly)
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
                    .background(.quaternary, in: RoundedRectangle(cornerRadius: 12))
                    .onSubmit(model.sendDraft)

                Button(action: model.sendDraft) {
                    Image(systemName: "arrow.up.circle.fill")
                        .font(.title2)
                }
                .buttonStyle(.plain)
                .foregroundStyle(model.canSendDraft ? Color.accentColor : Color.secondary)
                .disabled(!model.canSendDraft || !chat.kind.canSend)
                .help("Send Message")
                .accessibilityLabel("Send Message") // [VERIFY] Matches the visible tooltip.
                .accessibilityInputLabels(["Send Message", "Send"])
            }
            .padding(12)
        }
        .background(.bar)
    }
}

private struct MessageRow: View {
    let message: NativeMessage
    let selected: Bool
    let density: DesktopChatDensity
    let select: () -> Void

    var body: some View {
        HStack {
            if message.sent { Spacer(minLength: 80) }
            VStack(alignment: .leading, spacing: 4) {
                if !message.sent, let author = message.author {
                    Text(author)
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                }
                MessageContentView(message: message)
                if let timestamp = message.timestamp {
                    Text(timestamp, format: .dateTime.hour().minute())
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
            }
            .padding(.horizontal, 12)
            .padding(.vertical, density.tokens.messagePadding)
            .background(
                message.sent ? AnyShapeStyle(Color.accentColor.opacity(0.15)) : AnyShapeStyle(.quaternary),
                in: RoundedRectangle(cornerRadius: 12)
            )
            .overlay {
                if selected {
                    RoundedRectangle(cornerRadius: 12)
                        .stroke(.tint, lineWidth: 2)
                        .padding(-2)
                }
            }
            .contentShape(RoundedRectangle(cornerRadius: 12))
            .onTapGesture(perform: select)
            .accessibilityElement(children: .combine)
            .accessibilityAddTraits(selected ? .isSelected : [])
            .accessibilityAction(named: selected ? "Deselect Message" : "Select Message", select)
            if !message.sent { Spacer(minLength: 80) }
        }
    }
}

private struct AttachmentTray: View {
    @ObservedObject var model: AppModel

    var body: some View {
        ScrollView(.horizontal) {
            HStack(spacing: 8) {
                ForEach(model.pendingAttachments) { attachment in
                    AttachmentCard(attachment: attachment) {
                        model.removeAttachment(attachment.id)
                    }
                    .draggable(attachment.id.uuidString)
                    .dropDestination(for: String.self) { values, _ in
                        guard let value = values.first, let source = UUID(uuidString: value) else { return false }
                        model.reorderAttachment(source, before: attachment.id)
                        return true
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
    let remove: () -> Void

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
    }
}

private struct MessageContentView: View {
    let message: NativeMessage

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            switch message.content {
            case .text:
                messageText
            case .image(let preview, let fileName, _):
                mediaPreview(preview: preview, fileName: fileName ?? "Image", video: false)
                if !message.text.isEmpty { messageText }
            case .video(let preview, let fileName, _):
                mediaPreview(preview: preview, fileName: fileName ?? "Video", video: true)
                if !message.text.isEmpty { messageText }
            case .file(let fileName, _):
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
        }
        .frame(maxWidth: 420, maxHeight: 320)
        .clipShape(RoundedRectangle(cornerRadius: 8))

        if let url = message.content.fileURL, FileManager.default.fileExists(atPath: url.path) {
            Button {
                NSWorkspace.shared.open(url)
            } label: {
                previewView
            }
            .buttonStyle(.plain)
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
        if let url = message.content.fileURL, FileManager.default.fileExists(atPath: url.path) {
            Button {
                NSWorkspace.shared.open(url)
            } label: {
                Label(name.isEmpty ? "File" : name, systemImage: "doc")
            }
            .buttonStyle(.link)
            .help("Open \(name)")
        } else {
            Label(name.isEmpty ? "File" : name, systemImage: "doc")
                .foregroundStyle(.secondary)
        }
    }
}

private struct DropTargetOverlay: View {
    var body: some View {
        Label("Drop files to attach", systemImage: "tray.and.arrow.down.fill")
            .font(.headline)
            .padding(24)
            .background(.regularMaterial, in: RoundedRectangle(cornerRadius: 12))
            .overlay {
                RoundedRectangle(cornerRadius: 12)
                    .stroke(.tint, style: StrokeStyle(lineWidth: 2, dash: [8, 4]))
            }
            .accessibilityAddTraits(.isStaticText)
    }
}
