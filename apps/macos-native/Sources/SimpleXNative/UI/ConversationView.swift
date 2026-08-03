import SwiftUI

struct ConversationView: View {
    @ObservedObject var model: AppModel
    @FocusState private var composerFocused: Bool

    var body: some View {
        Group {
            if let chat = model.selectedChat {
                VStack(spacing: 0) {
                    transcript(chat: chat)
                    Divider()
                    composer(chat: chat)
                }
                .navigationTitle(chat.displayName)
            } else {
                ContentUnavailableView("Select a conversation", systemImage: "bubble.left.and.bubble.right")
            }
        }
    }

    private func transcript(chat: NativeChat) -> some View {
        ScrollViewReader { proxy in
            ScrollView {
                LazyVStack(spacing: 8) {
                    ForEach(model.messages) { message in
                        MessageRow(message: message)
                            .id(message.id)
                    }
                }
                .padding(16)
            }
            .overlay {
                if model.isLoadingConversation { ProgressView() }
            }
            .onChange(of: model.messages.last?.id) { _, lastID in
                guard let lastID else { return }
                proxy.scrollTo(lastID, anchor: .bottom)
            }
        }
    }

    private func composer(chat: NativeChat) -> some View {
        HStack(alignment: .bottom, spacing: 8) {
            TextField("Message", text: $model.draft, axis: .vertical)
                .textFieldStyle(.plain)
                .lineLimit(1...8)
                .focused($composerFocused)
                .padding(.horizontal, 12)
                .padding(.vertical, 8)
                .background(.quaternary, in: RoundedRectangle(cornerRadius: 12))
                .onSubmit(model.sendDraft)

            Button(action: model.sendDraft) {
                Image(systemName: "arrow.up.circle.fill")
                    .font(.title2)
            }
            .buttonStyle(.plain)
            .foregroundStyle(
                model.draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
                    ? Color.secondary
                    : Color.accentColor
            )
            .disabled(model.draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty || model.isSending || !chat.kind.canSend)
            .help("Send Message")
            .accessibilityLabel("Send Message") // [VERIFY] Matches the visible tooltip.
            .accessibilityInputLabels(["Send Message", "Send"])
        }
        .padding(12)
        .background(.bar)
    }
}

private struct MessageRow: View {
    let message: NativeMessage

    var body: some View {
        HStack {
            if message.sent { Spacer(minLength: 80) }
            VStack(alignment: .leading, spacing: 4) {
                if !message.sent, let author = message.author {
                    Text(author)
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                }
                Text(message.text)
                    .textSelection(.enabled)
                if let timestamp = message.timestamp {
                    Text(timestamp, format: .dateTime.hour().minute())
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 8)
            .background(
                message.sent ? AnyShapeStyle(Color.accentColor.opacity(0.16)) : AnyShapeStyle(.quaternary),
                in: RoundedRectangle(cornerRadius: 12)
            )
            .accessibilityElement(children: .combine)
            if !message.sent { Spacer(minLength: 80) }
        }
    }
}
