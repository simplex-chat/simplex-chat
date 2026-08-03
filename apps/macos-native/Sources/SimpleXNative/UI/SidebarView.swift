import SwiftUI

struct SidebarView: View {
    @ObservedObject var model: AppModel

    var body: some View {
        List(selection: Binding(
            get: { model.selectedChatID },
            set: { model.selectChat($0) }
        )) {
            ForEach(model.filteredChats) { chat in
                ChatSidebarRow(chat: chat, density: model.density)
                    .tag(chat.id)
            }
        }
        .listStyle(.sidebar)
        .overlay {
            if model.filteredChats.isEmpty {
                if model.searchText.isEmpty {
                    ContentUnavailableView("No Chats", systemImage: "bubble.left.and.bubble.right")
                } else {
                    ContentUnavailableView.search(text: model.searchText)
                }
            }
        }
        .navigationTitle("Chats")
        .searchable(
            text: $model.searchText,
            isPresented: $model.sidebarSearchPresented,
            placement: .sidebar,
            prompt: "Search"
        )
        .toolbar {
            ToolbarItem(placement: .navigation) {
                Menu {
                    if let name = model.profile?.displayName {
                        Text(name)
                    }
                    SettingsLink {
                        Label("Settings…", systemImage: "gear")
                    }
                } label: {
                    ProfileAvatar(image: model.profile?.image, name: model.profile?.displayName ?? "Profile", size: 24)
                }
                .menuStyle(.borderlessButton)
                .fixedSize()
                .help("Profile and Settings")
                .accessibilityLabel("Profile and Settings")
                .accessibilityInputLabels(["Profile and Settings", "Profile", "Settings"])
            }

        }
    }
}

private struct ChatSidebarRow: View {
    let chat: NativeChat
    let density: DesktopChatDensity

    var body: some View {
        HStack(spacing: 12) {
            ChatSidebarAvatar(chat: chat, size: density.tokens.avatarSize)
                .accessibilityHidden(true)

            VStack(alignment: .leading, spacing: 4) {
                HStack(spacing: 8) {
                    Text(chat.displayName)
                        .font(.headline)
                        .lineLimit(1)
                    Spacer(minLength: 8)
                    if let timestamp = chat.timestamp {
                        ChatTimestamp(date: timestamp)
                    }
                }
                HStack(spacing: 8) {
                    Text(chat.preview.isEmpty ? "No messages yet" : chat.preview)
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                    Spacer(minLength: 8)
                    if chat.unreadCount > 0 {
                        Text(chat.unreadCount, format: .number.grouping(.never))
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.white)
                            .padding(.horizontal, 8)
                            .padding(.vertical, 4)
                            .background(.tint, in: Capsule())
                    }
                }
            }
        }
        .padding(.vertical, density.tokens.chatRowPadding)
        .accessibilityElement(children: .ignore)
        .accessibilityLabel(chat.accessibilityDescription)
    }
}

private struct ChatSidebarAvatar: View {
    let chat: NativeChat
    let size: CGFloat

    var body: some View {
        if chat.image != nil || chat.kind == .direct {
            ProfileAvatar(image: chat.image, name: chat.displayName, size: size)
        } else {
            ZStack {
                Circle().fill(.quaternary)
                Image(systemName: chat.kind == .local ? "folder.fill" : "person.2.fill")
                    .font(.headline)
                    .foregroundStyle(.secondary)
            }
            .frame(width: size, height: size)
        }
    }
}

private struct ChatTimestamp: View {
    let date: Date

    var body: some View {
        Text(label)
            .font(.caption)
            .foregroundStyle(.secondary)
    }

    private var label: String {
        if Calendar.current.isDateInToday(date) {
            return date.formatted(.dateTime.hour().minute())
        }
        if Calendar.current.isDateInYesterday(date) { return "Yesterday" }
        return date.formatted(.dateTime.month(.abbreviated).day())
    }
}

struct ProfileAvatar: View {
    let image: String?
    let name: String
    let size: CGFloat

    var body: some View {
        Group {
            if let nsImage = NativeChatParser.image(from: image) {
                Image(nsImage: nsImage)
                    .resizable()
                    .scaledToFill()
            } else {
                ZStack {
                    Circle().fill(.quaternary)
                    Text(String(name.prefix(1)).uppercased())
                        .font(.headline)
                        .foregroundStyle(.secondary)
                }
            }
        }
        .frame(width: size, height: size)
        .clipShape(Circle())
    }
}
