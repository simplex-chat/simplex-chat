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
    @State private var showAddProfile = false
    @State private var creatingProfile = false
    @State private var changingProfile = false

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
        // On the Group: the row and profilePicker() are both disposed while this is
        // presented. Stacking sheets is supported from iOS 14.5; the target is 15.
        .sheet(isPresented: $showAddProfile) {
            NavigationView {
                CreateProfile(onSubmit: { displayName, shortDescr, image in
                    try await createProfileForChat(displayName, shortDescr, image)
                }, submitting: creatingProfile)
            }
            .interactiveDismissDisabled(creatingProfile)
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
                            addProfileOption()
                                .contentShape(Rectangle())
                            Divider()
                                .padding(.leading)
                                .padding(.leading, 48)

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
                        .frame(maxHeight: USER_ROW_SIZE * min(MAX_VISIBLE_USER_ROWS, CGFloat(users.count + 2))) // + 1 for incognito, + 1 for "Add profile"
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

    private var busy: Bool { creatingProfile || changingProfile }

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
                    // Only the branch that starts work; expand/collapse is local
                    if busy { return }
                    changingProfile = true
                    changeProfile(user)
                }
            } else {
                showCantChangeProfileAlert()
            }
        } label: {
            HStack {
                ProfileImage(imageStr: user.image, size: 38)
                NameWithBadge(
                    Text(user.chatViewName)
                        .fontWeight(selectedUser == user && !incognitoDefault ? .medium : .regular)
                        .foregroundColor(theme.colors.onBackground),
                    user.profile.localBadge
                )
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

    private func addProfileOption() -> some View {
        Button {
            if chat.chatInfo.profileChangeProhibited {
                showCantChangeProfileAlert()
            } else {
                showAddProfile = true
            }
        } label: {
            HStack {
                Image(systemName: "person.crop.circle.badge.plus")
                    .resizable()
                    .scaledToFit()
                    .frame(width: 38, height: 38)
                    .foregroundColor(theme.colors.primary)
                Text("Add profile")
                    .foregroundColor(theme.colors.primary)
                    .lineLimit(1)

                Spacer()
            }
            .padding(.leading, 12)
            .padding(.trailing)
            .frame(height: USER_ROW_SIZE)
        }
        .disabled(busy)
    }

    // Created without becoming active: changeProfile below resolves the prepared chat
    // under the active user, so the profile that owns it must stay active until it moves.
    private func createProfileForChat(_ displayName: String, _ shortDescr: String?, _ image: String?) async throws {
        // Atomic check-and-set: check-then-set lets two submits through, and @State
        // must not be read off the main actor.
        let alreadyCreating = await MainActor.run { () -> Bool in
            if creatingProfile { return true }
            creatingProfile = true
            return false
        }
        if alreadyCreating { return }
        defer { Task { @MainActor in creatingProfile = false } }
        let ownerUserId = await MainActor.run { chatModel.currentUser?.userId }
        let profile = Profile(displayName: displayName, fullName: "", shortDescr: shortDescr, image: image)
        let newUser = try apiCreateActiveUser(profile, keepActiveUser: true)
        // Checked before refreshing the lists below: on this path the core has already
        // activated the new profile, so they would disagree with chatModel.currentUser
        // until the resync lands - and changeActiveUserAsync_ refreshes them anyway.
        if newUser.activeUser {
            // An older remote host ignored keepActiveUser, so the reassignment would
            // fail. Resync and report - not rethrown, or the form blames the creation.
            do {
                try await changeActiveUserAsync_(newUser.userId, viewPwd: nil)
            } catch {
                logger.error("changeActiveUserAsync_ error: \(responseError(error))")
            }
            await MainActor.run {
                // Unconditional: the switch only removes this view when it succeeded
                showAddProfile = false
                // Only if it switched, which the active user tells us: the prepared chat
                // is then gone from the reloaded list and would render blank.
                if chatModel.currentUser?.userId == newUser.userId && chatModel.chatId == chat.id {
                    chatModel.chatId = nil
                }
            }
            alertAfterDismissal(NSLocalizedString("Error changing chat profile", comment: "alert title"))
            return
        }
        // changeProfile resolves the prepared chat under whatever is active when it runs,
        // and a notification action can have switched it while we were creating.
        guard await MainActor.run({ chatModel.currentUser?.userId }) == ownerUserId else {
            await MainActor.run { showAddProfile = false }
            alertAfterDismissal(NSLocalizedString("Error changing chat profile", comment: "alert title"))
            return
        }
        let updatedUsers = try? await listUsersAsync()
        await MainActor.run {
            if let updatedUsers = updatedUsers {
                chatModel.users = updatedUsers
                // Only filled in onAppear otherwise, so the new profile is missing here
                users = updatedUsers.map { $0.user }.filter { u in u.activeUser || !u.hidden }
            } else if !users.contains(where: { $0.userId == newUser.userId }) {
                // changeProfile sets selectedUser to it, and otherUsers filters on that -
                // absent from users, nothing is filtered out and a row is clipped.
                users.append(newUser)
            }
            // changingProfile here too: the defer clears creatingProfile as soon as this returns
            showAddProfile = false
            changingProfile = true
        }
        changeProfile(newUser)
    }

    private func changeProfile(_ newUser: User) {
        Task {
            defer { Task { @MainActor in changingProfile = false } }
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
                    alertAfterDismissal(
                        NSLocalizedString("Error switching profile", comment: "alert title"),
                        String.localizedStringWithFormat(NSLocalizedString("Your chat was moved to %@ but an unexpected error occurred while redirecting you to the profile.", comment: "alert message"), newUser.chatViewName)
                    )
                }
            } catch let error {
                await MainActor.run {
                    if let currentUser = chatModel.currentUser {
                        selectedUser = currentUser
                    }
                }
                alertAfterDismissal(
                    NSLocalizedString("Error changing chat profile", comment: "alert title"),
                    responseError(error)
                )
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
