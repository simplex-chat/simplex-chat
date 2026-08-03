import SwiftUI

struct SidebarView: View {
    @ObservedObject var model: AppModel

    var body: some View {
        List(selection: Binding(
            get: { model.selectedChatID },
            set: model.selectChat
        )) {
            ForEach(model.filteredChats) { chat in
                ChatSidebarRow(chat: chat)
                    .tag(chat.id)
            }
        }
        .listStyle(.sidebar)
        .navigationTitle("Chats")
        .searchable(text: $model.searchText, placement: .sidebar, prompt: "Search")
        .toolbar {
            ToolbarItem(placement: .navigation) {
                ProfileAvatar(image: model.profile?.image, name: model.profile?.displayName ?? "Profile", size: 24)
                    .help(model.profile?.displayName ?? "Profile")
                    .accessibilityLabel("Current profile, \(model.profile?.displayName ?? "Profile")")
            }
        }
    }
}

private struct ChatSidebarRow: View {
    let chat: NativeChat

    var body: some View {
        HStack(spacing: 12) {
            ProfileAvatar(image: chat.image, name: chat.displayName, size: 40)
                .accessibilityHidden(true)

            VStack(alignment: .leading, spacing: 4) {
                HStack(spacing: 8) {
                    Text(chat.displayName)
                        .font(.headline)
                        .lineLimit(1)
                    Spacer(minLength: 8)
                    if let timestamp = chat.timestamp {
                        Text(timestamp, format: .dateTime.hour().minute())
                            .font(.caption)
                            .foregroundStyle(.secondary)
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
        .padding(.vertical, 4)
        .accessibilityElement(children: .ignore)
        .accessibilityLabel(chat.accessibilityDescription)
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
