//
//  ContextProfilePickerView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 13.06.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let USER_ROW_SIZE: CGFloat = 60
let MAX_VISIBLE_USER_ROWS: CGFloat = 4.8

struct ContextProfilePickerView: View {
    @ObservedObject var chat: Chat
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State var selectedUser: User
    @State private var users: [User] = []
    @State private var listExpanded = false
    @State private var expandedListReady = false
    @State private var showIncognitoSheet = false

    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false

    var body: some View {
        viewBody()
            .onAppear {
                users = chatModel.users
                    .map { $0.user }
                    .filter { u in u.activeUser || !u.hidden }
            }
            .sheet(isPresented: $showIncognitoSheet) {
                IncognitoHelp()
            }
    }

    private func viewBody() -> some View {
        Group {
            if !listExpanded || chat.chatInfo.profileChangeProhibited {
                currentSelection()
            } else {
                profilePicker()
            }
        }
    }

    private func currentSelection() -> some View {
        VStack(spacing: 0) {
            HStack {
                Text("Your profile")
                    .font(.callout)
                    .foregroundColor(theme.colors.secondary)
                Spacer()
            }
            .padding(.top, 8)
            .padding(.bottom, -4)
            .padding(.leading, 12)
            .padding(.trailing)

            if chat.chatInfo.profileChangeProhibited {
                if chat.chatInfo.incognito {
                    incognitoOption()
                } else {
                    profilerPickerUserOption(selectedUser)
                }
            } else if incognitoDefault {
                incognitoOption()
            } else {
                profilerPickerUserOption(selectedUser)
            }
        }
    }

    private func profilePicker() -> some View {
        ScrollViewReader { proxy in
            Group {
                if expandedListReady {
                    let scroll = ScrollView {
                        LazyVStack(spacing: 0) {
                            let otherUsers = users
                                .filter { u in u.userId != selectedUser.userId }
                                .sorted(using: KeyPathComparator<User>(\.activeOrder))
                            ForEach(otherUsers) { p in
                                profilerPickerUserOption(p)
                                    .contentShape(Rectangle())
                                Divider()
                                    .padding(.leading)
                                    .padding(.leading, 48)
                            }

                            if incognitoDefault {
                                profilerPickerUserOption(selectedUser)
                                    .contentShape(Rectangle())
                                Divider()
                                    .padding(.leading)
                                    .padding(.leading, 48)

                                incognitoOption()
                                    .contentShape(Rectangle())
                                    .id("BOTTOM_ANCHOR")
                            } else {
                                incognitoOption()
                                    .contentShape(Rectangle())
                                Divider()
                                    .padding(.leading)
                                    .padding(.leading, 48)

                                profilerPickerUserOption(selectedUser)
                                    .contentShape(Rectangle())
                                    .id("BOTTOM_ANCHOR")
                            }
                        }
                    }
                        .frame(maxHeight: USER_ROW_SIZE * min(MAX_VISIBLE_USER_ROWS, CGFloat(users.count + 1))) // + 1 for incognito
                        .onAppear {
                            DispatchQueue.main.async {
                                withAnimation(nil) {
                                    proxy.scrollTo("BOTTOM_ANCHOR", anchor: .bottom)
                                }
                            }
                        }
                        .onDisappear {
                            expandedListReady = false
                        }

                    if #available(iOS 16.0, *) {
                        scroll.scrollDismissesKeyboard(.never)
                    } else {
                        scroll
                    }
                } else {
                    // Keep showing current selection to avoid flickering of scroll to bottom
                    currentSelection()
                        .onAppear {
                            // Delay rendering of expanded profile list
                            DispatchQueue.main.async {
                                expandedListReady = true
                            }
                        }
                }
            }
        }
    }
    
    private func profilerPickerUserOption(_ user: User) -> some View {
        Button {
            if !chat.chatInfo.profileChangeProhibited {
                if selectedUser == user {
                    if !incognitoDefault {
                        listExpanded.toggle()
                    } else {
                        incognitoDefault = false
                        listExpanded = false
                    }
                } else if selectedUser != user {
                    changeProfile(user)
                }
            } else {
                showCantChangeProfileAlert()
            }
        } label: {
            HStack {
                ProfileImage(imageStr: user.image, size: 38)
                Text(user.chatViewName)
                    .fontWeight(selectedUser == user && !incognitoDefault ? .medium : .regular)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)

                Spacer()

                if selectedUser == user && !incognitoDefault {
                    if listExpanded {
                        Image(systemName: "chevron.down")
                            .font(.system(size: 12, weight: .bold))
                            .foregroundColor(theme.colors.secondary)
                            .opacity(0.7)
                    } else if !chat.chatInfo.profileChangeProhibited {
                        Image(systemName: "chevron.up")
                            .font(.system(size: 12, weight: .bold))
                            .foregroundColor(theme.colors.secondary)
                            .opacity(0.7)
                    }
                }
            }
            .padding(.leading, 12)
            .padding(.trailing)
            .frame(height: USER_ROW_SIZE)
        }
    }

    private func changeProfile(_ newUser: User) {
        Task {
            do {
                if let contact = chat.chatInfo.contact {
                    let updatedContact = try await apiChangePreparedContactUser(contactId: contact.contactId, newUserId: newUser.userId)
                    await MainActor.run {
                        selectedUser = newUser
                        incognitoDefault = false
                        listExpanded = false
                        chatModel.updateContact(updatedContact)
                    }
                } else if let groupInfo = chat.chatInfo.groupInfo {
                    let updatedGroupInfo = try await apiChangePreparedGroupUser(groupId: groupInfo.groupId, newUserId: newUser.userId)
                    await MainActor.run {
                        selectedUser = newUser
                        incognitoDefault = false
                        listExpanded = false
                        chatModel.updateGroup(updatedGroupInfo)
                    }
                }
                do {
                    try await changeActiveUserAsync_(newUser.userId, viewPwd: nil, keepingChatId: chat.id)
                } catch {
                    await MainActor.run {
                        showAlert(
                            NSLocalizedString("Error switching profile", comment: "alert title"),
                            message: String.localizedStringWithFormat(NSLocalizedString("Your chat was moved to %@ but an unexpected error occurred while redirecting you to the profile.", comment: "alert message"), newUser.chatViewName)
                        )
                    }
                }
            } catch let error {
                await MainActor.run {
                    if let currentUser = chatModel.currentUser {
                        selectedUser = currentUser
                    }
                    showAlert(
                        NSLocalizedString("Error changing chat profile", comment: "alert title"),
                        message: responseError(error)
                    )
                }
            }
        }
    }

    private func incognitoOption() -> some View {
        Button {
            if !chat.chatInfo.profileChangeProhibited {
                if incognitoDefault {
                    listExpanded.toggle()
                } else {
                    incognitoDefault = true
                    listExpanded = false
                }
            } else {
                showCantChangeProfileAlert()
            }
        } label : {
            HStack {
                incognitoProfileImage()
                Text("Incognito")
                    .fontWeight(incognitoDefault ? .medium : .regular)
                    .foregroundColor(theme.colors.onBackground)
                Image(systemName: "info.circle")
                    .font(.system(size: 16))
                    .foregroundColor(theme.colors.primary)
                    .onTapGesture {
                        showIncognitoSheet = true
                    }

                Spacer()

                if incognitoDefault {
                    if listExpanded {
                        Image(systemName: "chevron.down")
                            .font(.system(size: 12, weight: .bold))
                            .foregroundColor(theme.colors.secondary)
                            .opacity(0.7)
                    } else if !chat.chatInfo.profileChangeProhibited {
                        Image(systemName: "chevron.up")
                            .font(.system(size: 12, weight: .bold))
                            .foregroundColor(theme.colors.secondary)
                            .opacity(0.7)
                    }
                }
            }
            .padding(.leading, 12)
            .padding(.trailing)
            .frame(height: USER_ROW_SIZE)
        }
    }

    private func incognitoProfileImage() -> some View {
        Image(systemName: "theatermasks.fill")
            .resizable()
            .scaledToFit()
            .frame(width: 38)
            .foregroundColor(.indigo)
    }

    private func showCantChangeProfileAlert() {
        showAlert(
            NSLocalizedString("Can't change profile", comment: "alert title"),
            message: NSLocalizedString("To use another profile after connection attempt, delete the chat and use the link again.", comment: "alert message")
        )
    }
}

#Preview {
    ContextProfilePickerView(
        chat: Chat.sampleData,
        selectedUser: User.sampleData
    )
}
