//
//  GroupChatInfoView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 14.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var alertManager = AlertManager.shared
    @ObservedObject var chat: Chat
    @Binding var chatViewSheet: ChatViewSheet?
    @State private var members: [GroupMember] = []
    @State private var selectedMember: GroupMember? = nil
    @State private var alert: ChatInfoViewAlert? = nil

    enum ChatInfoViewAlert: Identifiable {
        case deleteChatAlert
        case clearChatAlert

        var id: ChatInfoViewAlert { get { self } }
    }

    var body: some View {
        VStack{
            ChatInfoImage(chat: chat)
                .frame(width: 192, height: 192)
                .padding(.top, 48)
                .padding()
            Text(chat.chatInfo.localDisplayName).font(.largeTitle)
                .padding(.bottom, 2)
            Text(chat.chatInfo.fullName).font(.title)
                .padding(.bottom)

            List(members) { member in
                memberView(member)
                    .listRowBackground(Color.clear)
            }
            .listStyle(.plain)

            Spacer()
            Button() {
                alert = .clearChatAlert
            } label: {
                Label("Clear conversation", systemImage: "gobackward")
            }
            .tint(Color.orange)
            .padding()
        }
        .sheet(item: $selectedMember) { member in
            GroupMemberInfoView(member: member)
        }
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .deleteChatAlert: return deleteChatAlert()
            case .clearChatAlert: return clearChatAlert()
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .task {
            members = await apiListMembers(chat.chatInfo.apiId)
        }
    }

    func serverImage() -> some View {
        let status = chat.serverInfo.networkStatus
        return Image(systemName: status.imageName)
            .foregroundColor(status == .connected ? .green : .secondary)
    }

    func memberView(_ member: GroupMember) -> some View {
        return Button {
            selectedMember = member
        } label: {
            HStack{
                ProfileImage(imageStr: member.image)
                    .frame(width: 30, height: 30)
                    .padding(.trailing, 2)
                Text(member.chatViewName)
                    .lineLimit(1)
            }
        }
    }

    // TODO reuse this clearChatAlert with ChatInfoView
    private func deleteChatAlert() -> Alert {
        Alert(
            title: Text("Delete chat?"),
            message: Text("Chat and all messages will be deleted - this cannot be undone!"),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                        DispatchQueue.main.async {
                            chatModel.removeChat(chat.chatInfo.id)
                            chatViewSheet = nil
                        }
                    } catch let error {
                        logger.error("deleteChatAlert apiDeleteChat error: \(error.localizedDescription)")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }

    private func clearChatAlert() -> Alert {
        Alert(
            title: Text("Clear conversation?"),
            message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
            primaryButton: .destructive(Text("Clear")) {
                Task {
                    await clearChat(chat)
                    DispatchQueue.main.async {
                        chatViewSheet = nil
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }
}

struct GroupChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        @State var chatViewSheet = ChatViewSheet.chatInfo
        return GroupChatInfoView(chat: Chat(chatInfo: ChatInfo.sampleData.group, chatItems: []), chatViewSheet: Binding($chatViewSheet))
    }
}
