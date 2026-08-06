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
        // Attached here, not to the row and not to profilePicker(): the row lives in a
        // lazy container that may dispose it, and profilePicker() itself is replaced the
        // moment listExpanded flips - which changeProfile does while this sheet is still
        // dismissing, and which an incoming event can do at any time by setting
        // profileChangeProhibited. This Group survives both. Stacking it with the
        // IncognitoHelp sheet on body is fine from iOS 14.5; the app targets 15.
        .sheet(isPresented: $showAddProfile) {
            NavigationView {
                CreateProfile(onSubmit: { displayName, shortDescr, image in
                    try await createProfileForChat(displayName, shortDescr, image)
                })
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
                                // Descending, as every other profile list sorts: a profile
                                // that was never activated has active_order 0 and belongs
                                // at the end, not the front.
                                .sorted(using: KeyPathComparator<User>(\.activeOrder, order: .reverse))
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
                    // Only the branch that starts work is guarded - expanding and
                    // collapsing the list is local and stays available.
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
            showAddProfile = true
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

    // Creates a profile to use for this invitation. It is created without becoming
    // active, because changeProfile below reassigns the prepared chat and the API
    // resolves that chat under the currently active user - so the profile that owns the
    // invitation has to stay active until the chat has been moved.
    private func createProfileForChat(_ displayName: String, _ shortDescr: String?, _ image: String?) async throws {
        // Atomic check-and-set on the main actor: a plain check-then-set leaves a window
        // where two submits both pass, and @State must not be read off the main actor.
        let alreadyCreating = await MainActor.run { () -> Bool in
            if creatingProfile { return true }
            creatingProfile = true
            return false
        }
        if alreadyCreating { return }
        defer { Task { @MainActor in creatingProfile = false } }
        let profile = Profile(displayName: displayName, fullName: "", shortDescr: shortDescr, image: image)
        let newUser = try apiCreateProfileKeepingActive(profile)
        let updatedUsers = try? listUsers()
        await MainActor.run {
            if let updatedUsers = updatedUsers {
                chatModel.users = updatedUsers
                // This view's own list is otherwise only filled in onAppear, so without
                // this the profile just created is missing from the picker if the
                // reassignment below fails - and the row count the frame is sized from
                // is one short.
                users = updatedUsers.map { $0.user }.filter { u in u.activeUser || !u.hidden }
            }
        }
        if newUser.activeUser {
            // The core did not honour keepActiveUser and activated the profile - an older
            // remote host ignoring the unknown field. Reassigning would now fail, so
            // resync to what the host actually did and report it. The failure is the
            // switch, not the creation, so it is not rethrown into the form's "error
            // creating profile" handler.
            // let, not var: MainActor.run's body is @Sendable, and capturing a mutable
            // local in one is diagnosed under strict concurrency. Assigned on both
            // branches, so it is definitely initialised.
            let switched: Bool
            do {
                try await changeActiveUserAsync_(newUser.userId, viewPwd: nil)
                switched = true
            } catch {
                logger.error("changeActiveUserAsync_ error: \(responseError(error))")
                switched = false
            }
            await MainActor.run {
                // Only when the switch actually happened: the prepared chat then belongs
                // to a profile that is no longer active, so it is absent from the reloaded
                // list and a pushed chat view renders blank. If the switch failed nothing
                // moved and the chat is still fine - closing it would be the regression.
                if switched && chatModel.chatId == chat.id { chatModel.chatId = nil }
            }
            // The switch replaces the chat list, which removes this view - and the sheet
            // it presents - from the hierarchy. Both that teardown and an explicit
            // dismissal animate, and getTopViewController() keeps returning the sheet
            // until the transition ends, so an alert raised now is presented on a
            // controller being dismissed and dropped. Let it settle first.
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                showAlert(NSLocalizedString("Error changing chat profile", comment: "alert title"))
            }
            return
        }
        // changingProfile set here, not only inside changeProfile's Task: the defer above
        // clears creatingProfile as soon as this function returns, and that Task has not
        // necessarily started by then - the rows would be live in between.
        await MainActor.run {
            showAddProfile = false
            changingProfile = true
        }
        changeProfile(newUser)
    }

    private func changeProfile(_ newUser: User) {
        Task {
            // Two round trips follow; without this every row, including "Add profile",
            // stays live and a second change can be started on top of this one.
            await MainActor.run { changingProfile = true }
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
                    // Assert the open chat: nothing on this path clears chatId on iOS, so
                    // this is normally a no-op, but keepingChatId only keeps the chat's
                    // place in the reloaded list - it does not open it. The id is
                    // unchanged by the reassignment, it is the contact/group id.
                    await MainActor.run { chatModel.chatId = chat.id }
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
