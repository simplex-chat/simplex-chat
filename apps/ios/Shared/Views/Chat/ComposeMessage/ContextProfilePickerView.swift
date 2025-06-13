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
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State var selectedProfile: User
    @State private var profiles: [User] = []
    @State private var listExpanded = false
    @State private var expandedListReady = false
    @State private var showIncognitoSheet = false

    @UserDefault(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false

    var body: some View {
        viewBody()
            .onAppear {
                profiles = chatModel.users
                    .map { $0.user }
                    .filter { u in u.activeUser || !u.hidden }
                    .sorted { u, _ in u.activeUser }
                    .reversed()
            }
            .sheet(isPresented: $showIncognitoSheet) {
                IncognitoHelp()
            }
    }

    @ViewBuilder private func viewBody() -> some View {
        if !listExpanded {
            currentSelection()
        } else {
            profilePicker()
        }
    }

    @ViewBuilder private func currentSelection() -> some View {
        if incognitoDefault {
            incognitoOption()
        } else {
            profilerPickerUserOption(selectedProfile)
        }
    }

    private func profilePicker() -> some View {
        ScrollViewReader { proxy in
            Group {
                if expandedListReady {
                    let scroll = ScrollView {
                        LazyVStack(spacing: 0) {
                            let otherProfiles = profiles.filter { u in u.userId != selectedProfile.userId }
                            ForEach(otherProfiles) { p in
                                profilerPickerUserOption(p)
                                    .contentShape(Rectangle())
                                Divider()
                                    .padding(.leading)
                                    .padding(.leading, 48)
                            }

                            if incognitoDefault {
                                profilerPickerUserOption(selectedProfile)
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

                                profilerPickerUserOption(selectedProfile)
                                    .contentShape(Rectangle())
                                    .id("BOTTOM_ANCHOR")
                            }
                        }
                    }
                        .frame(maxHeight: USER_ROW_SIZE * min(MAX_VISIBLE_USER_ROWS, CGFloat(profiles.count + 1))) // + 1 for incognito
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
                    // Keep showing current selection to delay rendering picker and flickering of scroll to bottom
                    currentSelection()
                        .onAppear {
                            // Delay rendering for 1 frame
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
            if selectedProfile == user {
                if !incognitoDefault {
                    listExpanded.toggle()
                } else {
                    incognitoDefault = false
                    listExpanded = false
                }
            } else if selectedProfile != user {
                changeProfile(user)
            }
        } label: {
            HStack {
                ProfileImage(imageStr: user.image, size: 30)
                    .padding(.trailing, 2)
                Text(user.chatViewName)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)

                Spacer()

                if selectedProfile == user && !incognitoDefault {
                    if listExpanded {
                        Image(systemName: "chevron.down")
                            .foregroundColor(theme.colors.secondary)
                    } else {
                        Image(systemName: "chevron.up")
                            .foregroundColor(theme.colors.secondary)

                    }
                }
            }
            .frame(height: USER_ROW_SIZE)
        }
    }

    private func changeProfile(_ user: User) {
        // Task
        selectedProfile = user
        incognitoDefault = false
        listExpanded = false
    }

    private func incognitoOption() -> some View {
        Button {
            if incognitoDefault {
                listExpanded.toggle()
            } else {
                incognitoDefault = true
                listExpanded = false
            }
        } label : {
            HStack {
                incognitoProfileImage()
                Text("Incognito")
                    .foregroundColor(theme.colors.onBackground)
                Image(systemName: "info.circle")
                    .foregroundColor(theme.colors.primary)
                    .font(.system(size: 14))
                    .onTapGesture {
                        showIncognitoSheet = true
                    }

                Spacer()

                if incognitoDefault {
                    if listExpanded {
                        Image(systemName: "chevron.down")
                            .foregroundColor(theme.colors.secondary)
                    } else {
                        Image(systemName: "chevron.up")
                            .foregroundColor(theme.colors.secondary)

                    }
                }
            }
            .frame(height: USER_ROW_SIZE)
        }
    }

    private func incognitoProfileImage() -> some View {
        Image(systemName: "theatermasks.fill")
            .resizable()
            .scaledToFit()
            .frame(width: 30)
            .foregroundColor(.indigo)
    }
}

#Preview {
    ContextProfilePickerView(
        selectedProfile: User.sampleData
    )
}
