//
//  NewChatView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/navigation.md

import SwiftUI
import SimpleXChat
import CodeScanner
import AVFoundation
import SimpleXChat

struct SomeAlert: Identifiable {
    var alert: Alert
    var id: String
}

struct SomeActionSheet: Identifiable {
    var actionSheet: ActionSheet
    var id: String
}

struct SomeSheet<Content: View>: Identifiable {
    @ViewBuilder var content: Content
    var id: String
    var fraction = 0.4
}

private enum NewChatViewAlert: Identifiable {
    case newChatSomeAlert(alert: SomeAlert)
    var id: String {
        switch self {
        case let .newChatSomeAlert(alert): return "newChatSomeAlert \(alert.id)"
        }
    }
}

enum NewChatOption: Identifiable {
    case invite
    case connect

    var id: Self { self }
}

func showKeepInvitationAlert() {
    if let showingInvitation = ChatModel.shared.showingInvitation,
       !showingInvitation.connChatUsed {
        showAlert(
            NSLocalizedString("Keep unused invitation?", comment: "alert title"),
            message: NSLocalizedString("You can view invitation link again in connection details.", comment: "alert message"),
            actions: {[
                UIAlertAction(
                    title: NSLocalizedString("Keep", comment: "alert action"),
                    style: .default
                ),
                UIAlertAction(
                    title: NSLocalizedString("Delete", comment: "alert action"),
                    style: .destructive,
                    handler: { _ in
                        Task {
                            await deleteChat(Chat(
                                chatInfo: .contactConnection(contactConnection: showingInvitation.pcc),
                                chatItems: []
                            ))
                        }
                    }
                )
            ]}
        )
    }
    ChatModel.shared.showingInvitation = nil
}

struct NewChatView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State var selection: NewChatOption
    @State var showQRCodeScanner = false
    @State private var invitationUsed: Bool = false
    @State private var connLinkInvitation: CreatedConnLink = CreatedConnLink(connFullLink: "", connShortLink: nil)
    @State private var showShortLink = true
    @State private var creatingConnReq = false
    @State var choosingProfile = false
    @State private var pastedLink: String = ""
    @State private var alert: NewChatViewAlert?
    @State private var contactConnection: PendingContactConnection? = nil

    var body: some View {
        VStack(alignment: .leading) {
            Picker("New chat", selection: $selection) {
                Label("1-time link", systemImage: "link")
                    .tag(NewChatOption.invite)
                Label("Connect via link", systemImage: "qrcode")
                    .tag(NewChatOption.connect)
            }
            .pickerStyle(.segmented)
            .padding()
            .onChange(of: $selection.wrappedValue) { opt in
                if opt == NewChatOption.connect {
                    showQRCodeScanner = true
                }
            }

            VStack {
                // it seems there's a bug in iOS 15 if several views in switch (or if-else) statement have different transitions
                // https://developer.apple.com/forums/thread/714977?answerId=731615022#731615022
                if case .invite = selection {
                    prepareAndInviteView()
                        .transition(.move(edge: .leading))
                        .onAppear {
                            createInvitation()
                        }
                }
                if case .connect = selection {
                    ConnectView(showQRCodeScanner: $showQRCodeScanner, pastedLink: $pastedLink, alert: $alert)
                        .transition(.move(edge: .trailing))
                }
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .modifier(ThemedBackground(grouped: true))
            .background(
                // Rectangle is needed for swipe gesture to work on mostly empty views (creatingLinkProgressView and retryButton)
                Rectangle()
                    .fill(theme.base == DefaultTheme.LIGHT ? theme.colors.background.asGroupedBackground(theme.base.mode) : theme.colors.background)
            )
            .animation(.easeInOut(duration: 0.3333), value: selection)
            .gesture(DragGesture(minimumDistance: 20.0, coordinateSpace: .local)
                .onChanged { value in
                    switch(value.translation.width, value.translation.height) {
                    case (...0, -30...30): // left swipe
                        if selection == .invite {
                            selection = .connect
                        }
                    case (0..., -30...30): // right swipe
                        if selection == .connect {
                            selection = .invite
                        }
                    default: ()
                    }
                }
            )
        }
        .toolbar {
            ToolbarItem(placement: .navigationBarTrailing) {
                InfoSheetButton {
                    AddContactLearnMore(showTitle: true)
                }
            }
        }
        .modifier(ThemedBackground(grouped: true))
        .onChange(of: invitationUsed) { used in
            if used && !(m.showingInvitation?.connChatUsed ?? true) {
                m.markShowingInvitationUsed()
            }
        }
        .onDisappear {
            if !choosingProfile {
                showKeepInvitationAlert()
                contactConnection = nil
            }
        }
        .alert(item: $alert) { a in
            switch(a) {
            case let .newChatSomeAlert(a):
                return a.alert
            }
        }
    }

    private func prepareAndInviteView() -> some View {
        ZStack { // ZStack is needed for views to not make transitions between each other
            if connLinkInvitation.connFullLink != "" {
                InviteView(
                    invitationUsed: $invitationUsed,
                    contactConnection: $contactConnection,
                    connLinkInvitation: $connLinkInvitation,
                    showShortLink: $showShortLink,
                    choosingProfile: $choosingProfile
                )
            } else if creatingConnReq {
                creatingLinkProgressView()
            } else {
                retryButton()
            }
        }
    }

    private func createInvitation() {
        if connLinkInvitation.connFullLink == "" && contactConnection == nil && !creatingConnReq {
            creatingConnReq = true
            Task {
                _ = try? await Task.sleep(nanoseconds: 250_000000)
                let (r, apiAlert) = await apiAddContact(incognito: incognitoGroupDefault.get())
                if let (connLink, pcc) = r {
                    await MainActor.run {
                        m.updateContactConnection(pcc)
                        m.showingInvitation = ShowingInvitation(pcc: pcc, connChatUsed: false)
                        connLinkInvitation = connLink
                        contactConnection = pcc
                    }
                } else {
                    await MainActor.run {
                        creatingConnReq = false
                        if let apiAlert = apiAlert {
                            alert = .newChatSomeAlert(alert: SomeAlert(alert: apiAlert, id: "createInvitation error"))
                        }
                    }
                }
            }
        }
    }

    // Rectangle here and in retryButton are needed for gesture to work
    private func creatingLinkProgressView() -> some View {
        ProgressView("Creating link…")
            .progressViewStyle(.circular)
    }

    private func retryButton() -> some View {
        Button(action: createInvitation) {
            VStack(spacing: 6) {
                Image(systemName: "arrow.counterclockwise")
                Text("Retry")
            }
        }
    }
}

private func incognitoProfileImage() -> some View {
    Image(systemName: "theatermasks.fill")
        .resizable()
        .scaledToFit()
        .frame(width: 30)
        .foregroundColor(.indigo)
}

private struct InviteView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Binding var invitationUsed: Bool
    @Binding var contactConnection: PendingContactConnection?
    @Binding var connLinkInvitation: CreatedConnLink
    @Binding var showShortLink: Bool
    @Binding var choosingProfile: Bool

    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false

    var body: some View {
        List {
            Section(header: Text("Share this 1-time invite link").foregroundColor(theme.colors.secondary)) {
                shareLinkView()
            }
            .listRowInsets(EdgeInsets(top: 0, leading: 20, bottom: 0, trailing: 10))

            qrCodeView()
            if let selectedProfile = chatModel.currentUser {
                Section {
                    NavigationLink {
                        ActiveProfilePicker(
                            contactConnection: $contactConnection,
                            connLinkInvitation: $connLinkInvitation,
                            incognitoEnabled: $incognitoDefault,
                            choosingProfile: $choosingProfile,
                            selectedProfile: selectedProfile
                        )
                    } label: {
                        HStack {
                            if incognitoDefault {
                                incognitoProfileImage()
                                Text("Incognito")
                            } else {
                                ProfileImage(imageStr: chatModel.currentUser?.image, size: 30)
                                Text(chatModel.currentUser?.chatViewName ?? "")
                            }
                        }
                    }
                } header: {
                    Text("Share profile").foregroundColor(theme.colors.secondary)
                } footer: {
                     if incognitoDefault {
                         Text("A new random profile will be shared.")
                     }
                }
            }
        }
        .onChange(of: incognitoDefault) { incognito in
            setInvitationUsed()
        }
        .onChange(of: chatModel.currentUser) { u in
            setInvitationUsed()
        }
    }

    private func shareLinkView() -> some View {
        HStack {
            let link = connLinkInvitation.simplexChatUri(short: showShortLink)
            linkTextView(link)
            Button {
                showShareSheet(items: [link])
                setInvitationUsed()
            } label: {
                Image(systemName: "square.and.arrow.up")
                    .padding(.top, -7)
            }
        }
        .frame(maxWidth: .infinity)
    }

    private func qrCodeView() -> some View {
        Section {
            SimpleXCreatedLinkQRCode(link: connLinkInvitation, short: $showShortLink, onShare: setInvitationUsed)
                .id("simplex-qrcode-view-for-\(connLinkInvitation.simplexChatUri(short: showShortLink))")
                .padding()
                .background(
                    RoundedRectangle(cornerRadius: 12, style: .continuous)
                        .fill(Color(uiColor: .secondarySystemGroupedBackground))
                )
                .padding(.horizontal)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
        } header: {
            ToggleShortLinkHeader(text: Text("Or show this code"), link: connLinkInvitation, short: $showShortLink)
        }
    }

    private func setInvitationUsed() {
        if !invitationUsed {
            invitationUsed = true
        }
    }
}

private enum ProfileSwitchStatus {
    case switchingUser
    case switchingIncognito
    case idle
}

private struct ActiveProfilePicker: View {
    @Environment(\.dismiss) var dismiss
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Binding var contactConnection: PendingContactConnection?
    @Binding var connLinkInvitation: CreatedConnLink
    @Binding var incognitoEnabled: Bool
    @Binding var choosingProfile: Bool
    @State private var alert: SomeAlert?
    @State private var profileSwitchStatus: ProfileSwitchStatus = .idle
    @State private var switchingProfileByTimeout = false
    @State private var lastSwitchingProfileByTimeoutCall: Double?
    @State private var profiles: [User] = []
    @State private var searchTextOrPassword = ""
    @State private var showIncognitoSheet = false
    @State private var incognitoFirst: Bool = false
    @State var selectedProfile: User
    var trimmedSearchTextOrPassword: String { searchTextOrPassword.trimmingCharacters(in: .whitespaces)}

    var body: some View {
        viewBody()
            .navigationTitle("Select chat profile")
            .searchable(text: $searchTextOrPassword, placement: .navigationBarDrawer(displayMode: .always))
            .autocorrectionDisabled(true)
            .navigationBarTitleDisplayMode(.large)
            .onAppear {
                profiles = chatModel.users
                    .map { $0.user }
            }
            .onChange(of: incognitoEnabled) { incognito in
                if profileSwitchStatus != .switchingIncognito {
                    return
                }

                Task {
                    do {
                        if let contactConn = contactConnection,
                           let conn = try await apiSetConnectionIncognito(connId: contactConn.pccConnId, incognito: incognito) {
                            await MainActor.run {
                                contactConnection = conn
                                chatModel.updateContactConnection(conn)
                                profileSwitchStatus = .idle
                                dismiss()
                            }
                        }
                    } catch {
                        profileSwitchStatus = .idle
                        incognitoEnabled = !incognito
                        logger.error("apiSetConnectionIncognito error: \(responseError(error))")
                        let err = getErrorAlert(error, "Error changing to incognito!")

                        alert = SomeAlert(
                            alert: Alert(
                                title: Text(err.title),
                                message: Text(err.message ?? "Error: \(responseError(error))")
                            ),
                            id: "setConnectionIncognitoError"
                        )
                    }
                }
            }
            .onChange(of: profileSwitchStatus) { sp in
                if sp != .idle {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                        switchingProfileByTimeout = profileSwitchStatus != .idle
                    }
                } else {
                    switchingProfileByTimeout = false
                }
            }
            .onChange(of: selectedProfile) { profile in
                if (profileSwitchStatus != .switchingUser) {
                    return
                }
                Task {
                    do {
                        if let contactConn = contactConnection,
                           let conn = try await apiChangeConnectionUser(connId: contactConn.pccConnId, userId: profile.userId) {
                            await MainActor.run {
                                contactConnection = conn
                                connLinkInvitation = conn.connLinkInv ?? CreatedConnLink(connFullLink: "", connShortLink: nil)
                                incognitoEnabled = false
                                chatModel.updateContactConnection(conn)
                            }
                            do {
                                try await changeActiveUserAsync_(profile.userId, viewPwd: profile.hidden ? trimmedSearchTextOrPassword : nil)
                                await MainActor.run {
                                    profileSwitchStatus = .idle
                                    dismiss()
                                }
                            } catch {
                                await MainActor.run {
                                    profileSwitchStatus = .idle
                                    alert = SomeAlert(
                                        alert: Alert(
                                            title: Text("Error switching profile"),
                                            message: Text("Your connection was moved to \(profile.chatViewName) but an error happened when switching profile.")
                                        ),
                                        id: "switchingProfileError"
                                    )
                                }
                            }
                        }
                    } catch {
                        await MainActor.run {
                            profileSwitchStatus = .idle
                            if let currentUser = chatModel.currentUser {
                                selectedProfile = currentUser
                            }
                            let err = getErrorAlert(error, "Error changing connection profile")
                            alert = SomeAlert(
                                alert: Alert(
                                    title: Text(err.title),
                                    message: Text(err.message ?? "Error: \(responseError(error))")
                                ),
                                id: "changeConnectionUserError"
                            )
                        }
                    }
                }
            }
            .alert(item: $alert) { a in
                a.alert
            }
            .onAppear {
                incognitoFirst = incognitoEnabled
                choosingProfile = true
            }
            .onDisappear {
                choosingProfile = false
            }
            .sheet(isPresented: $showIncognitoSheet) {
                IncognitoHelp()
            }
    }


    @ViewBuilder private func viewBody() -> some View {
        profilePicker()
            .allowsHitTesting(!switchingProfileByTimeout)
            .modifier(ThemedBackground(grouped: true))
            .overlay {
                if switchingProfileByTimeout {
                    ProgressView()
                        .scaleEffect(2)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                }
            }
    }

    private func filteredProfiles() -> [User] {
        let s = trimmedSearchTextOrPassword
        let lower = s.localizedLowercase

        return profiles.filter { u in
            if (u.activeUser || !u.hidden) && (s == "" || u.chatViewName.localizedLowercase.contains(lower)) {
                return true
            }
            return correctPassword(u, s)
        }
    }

    private func profilerPickerUserOption(_ user: User) -> some View {
        Button {
            if selectedProfile == user && incognitoEnabled {
                incognitoEnabled = false
                profileSwitchStatus = .switchingIncognito
            } else if selectedProfile != user {
                selectedProfile = user
                profileSwitchStatus = .switchingUser
            }
        } label: {
            HStack {
                ProfileImage(imageStr: user.image, size: 30)
                    .padding(.trailing, 2)
                Text(user.chatViewName)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)
                Spacer()
                if selectedProfile == user, !incognitoEnabled {
                    Image(systemName: "checkmark")
                        .resizable().scaledToFit().frame(width: 16)
                        .foregroundColor(theme.colors.primary)
                }
            }
        }
    }

    @ViewBuilder private func profilePicker() -> some View {
        let incognitoOption = Button {
            if !incognitoEnabled {
                incognitoEnabled = true
                profileSwitchStatus = .switchingIncognito
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
                if incognitoEnabled {
                    Image(systemName: "checkmark")
                        .resizable().scaledToFit().frame(width: 16)
                        .foregroundColor(theme.colors.primary)
                }
            }
        }

        List {
            let filteredProfiles = filteredProfiles()
            let activeProfile = filteredProfiles.first { u in u.activeUser }

            if let selectedProfile = activeProfile {
                let otherProfiles = filteredProfiles
                    .filter { u in u.userId != activeProfile?.userId }
                    .sorted(using: KeyPathComparator<User>(\.activeOrder, order: .reverse))

                if incognitoFirst {
                    incognitoOption
                    profilerPickerUserOption(selectedProfile)
                } else {
                    profilerPickerUserOption(selectedProfile)
                    incognitoOption
                }

                ForEach(otherProfiles) { p in
                    profilerPickerUserOption(p)
                }
            } else {
                incognitoOption
                ForEach(filteredProfiles) { p in
                    profilerPickerUserOption(p)
                }
            }
        }
        .opacity(switchingProfileByTimeout ? 0.4 : 1)
    }
}

private struct ConnectView: View {
    @StateObject private var connectProgressManager = ConnectProgressManager.shared
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var showQRCodeScanner: Bool
    @Binding var pastedLink: String
    @Binding var alert: NewChatViewAlert?
    @State var scannerPaused: Bool = false
    @State private var pasteboardHasStrings = UIPasteboard.general.hasStrings

    var body: some View {
        List {
            Section(header: Text("Paste the link you received").foregroundColor(theme.colors.secondary)) {
                pasteLinkView()
            }
            Section(header: Text("Or scan QR code").foregroundColor(theme.colors.secondary)) {
                ScannerInView(showQRCodeScanner: $showQRCodeScanner, scannerPaused: $scannerPaused, processQRCode: processQRCode)
            }
        }
        .onDisappear {
            connectProgressManager.cancelConnectProgress()
        }
    }

    @ViewBuilder private func pasteLinkView() -> some View {
        if pastedLink == "" {
            ZStack(alignment: .trailing) {
                Button {
                    if let str = UIPasteboard.general.string {
                        if let link = strHasSingleSimplexLink(str.trimmingCharacters(in: .whitespaces)) {
                            pastedLink = link.text
                            // It would be good to hide it, but right now it is not clear how to release camera in CodeScanner
                            // https://github.com/twostraws/CodeScanner/issues/121
                            // No known tricks worked (changing view ID, wrapping it in another view, etc.)
                            // showQRCodeScanner = false
                            connect(pastedLink)
                        } else {
                            alert = .newChatSomeAlert(alert: SomeAlert(
                                alert: mkAlert(title: "Invalid link", message: "The text you pasted is not a SimpleX link."),
                                id: "pasteLinkView: code is not a SimpleX link"
                            ))
                        }
                    }
                } label: {
                    Text("Tap to paste link")
                }
                .disabled(!pasteboardHasStrings)
                .frame(maxWidth: .infinity, alignment: .center)
                if connectProgressManager.showConnectProgress != nil {
                    ProgressView()
                }
            }
        } else {
            HStack {
                linkTextView(pastedLink)
                if connectProgressManager.showConnectProgress != nil {
                    ProgressView()
                }
            }
        }
    }

    private func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            let link = r.string
            if strIsSimplexLink(r.string) {
                connect(link)
            } else {
                alert = .newChatSomeAlert(alert: SomeAlert(
                    alert: mkAlert(title: "Invalid QR code", message: "The code you scanned is not a SimpleX link QR code."),
                    id: "processQRCode: code is not a SimpleX link"
                ))
            }
        case let .failure(e):
            logger.error("processQRCode QR code error: \(e.localizedDescription)")
            alert = .newChatSomeAlert(alert: SomeAlert(
                alert: mkAlert(title: "Invalid QR code", message: "Error scanning code: \(e.localizedDescription)"),
                id: "processQRCode: failure"
            ))
        }
    }

    private func connect(_ link: String) {
        scannerPaused = true
        planAndConnect(
            link,
            theme: theme,
            dismiss: true,
            cleanup: {
                pastedLink = ""
                scannerPaused = false
            }
        )
    }
}

struct ScannerInView: View {
    @Binding var showQRCodeScanner: Bool
    var scannerPaused: Binding<Bool>? = nil
    let processQRCode: (_ resp: Result<ScanResult, ScanError>) -> Void
    @State private var cameraAuthorizationStatus: AVAuthorizationStatus?
    var scanMode: ScanMode = .continuous

    var body: some View {
        Group {
            if showQRCodeScanner, case .authorized = cameraAuthorizationStatus {
                CodeScannerView(codeTypes: [.qr], scanMode: scanMode, isPaused: scannerPaused?.wrappedValue ?? false, completion: processQRCode)
                    .aspectRatio(1, contentMode: .fit)
                    .cornerRadius(12)
                    .listRowBackground(Color.clear)
                    .listRowSeparator(.hidden)
                    .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                    .padding(.horizontal)
            } else {
                Button {
                    switch cameraAuthorizationStatus {
                    case .notDetermined: askCameraAuthorization { showQRCodeScanner = true }
                    case .restricted: ()
                    case .denied: UIApplication.shared.open(appSettingsURL)
                    case .authorized: showQRCodeScanner = true
                    default: askCameraAuthorization { showQRCodeScanner = true }
                    }
                } label: {
                    ZStack {
                        Rectangle()
                            .aspectRatio(contentMode: .fill)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                            .foregroundColor(Color.clear)
                        switch cameraAuthorizationStatus {
                        case .authorized, nil: EmptyView()
                        case .restricted: Text("Camera not available")
                        case .denied:  Label("Enable camera access", systemImage: "camera")
                        default: Label("Tap to scan", systemImage: "qrcode")
                        }
                    }
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .center)
                .padding()
                .background(
                    RoundedRectangle(cornerRadius: 12, style: .continuous)
                        .fill(Color(uiColor: .secondarySystemGroupedBackground))
                )
                .padding(.horizontal)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .disabled(cameraAuthorizationStatus == .restricted)
            }
        }
        .task {
            let status = AVCaptureDevice.authorizationStatus(for: .video)
            cameraAuthorizationStatus = status
            if showQRCodeScanner {
                switch status {
                case .notDetermined: await askCameraAuthorizationAsync()
                case .restricted: showQRCodeScanner = false
                case .denied: showQRCodeScanner = false
                case .authorized: ()
                @unknown default: await askCameraAuthorizationAsync()
                }
            }
        }
    }

    func askCameraAuthorizationAsync() async {
        await AVCaptureDevice.requestAccess(for: .video)
        cameraAuthorizationStatus = AVCaptureDevice.authorizationStatus(for: .video)
    }

    func askCameraAuthorization(_ cb: (() -> Void)? = nil) {
        AVCaptureDevice.requestAccess(for: .video) { allowed in
            cameraAuthorizationStatus = AVCaptureDevice.authorizationStatus(for: .video)
            if allowed { cb?() }
        }
    }
}


private func linkTextView(_ link: String) -> some View {
    Text(link)
        .lineLimit(1)
        .font(.caption)
        .truncationMode(.middle)
}

struct InfoSheetButton<Content: View>: View {
    @ViewBuilder let content: Content
    @State private var showInfoSheet = false

    var body: some View {
        Button {
            showInfoSheet = true
        } label: {
            Image(systemName: "info.circle")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
        }
        .sheet(isPresented: $showInfoSheet) {
            content
        }
    }
}

func strIsSimplexLink(_ str: String) -> Bool {
    if let parsedMd = parseSimpleXMarkdown(str),
       parsedMd.count == 1,
       case .simplexLink = parsedMd[0].format {
        return true
    } else {
        return false
    }
}

func strHasSingleSimplexLink(_ str: String) -> FormattedText? {
    if let parsedMd = parseSimpleXMarkdown(str) {
       let parsedLinks = parsedMd.filter({ $0.format?.isSimplexLink ?? false })
        if parsedLinks.count == 1 {
            return parsedLinks[0]
        } else {
            return nil
        }
    } else {
        return nil
    }
}

struct IncognitoToggle: View {
    @EnvironmentObject var theme: AppTheme
    @Binding var incognitoEnabled: Bool
    @State private var showIncognitoSheet = false

    var body: some View {
        ZStack(alignment: .leading) {
            Image(systemName: incognitoEnabled ? "theatermasks.fill" : "theatermasks")
                .frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                .foregroundColor(incognitoEnabled ? Color.indigo : theme.colors.secondary)
                .font(.system(size: 14))
            Toggle(isOn: $incognitoEnabled) {
                HStack(spacing: 6) {
                    Text("Incognito")
                    Image(systemName: "info.circle")
                        .foregroundColor(theme.colors.primary)
                        .font(.system(size: 14))
                }
                .onTapGesture {
                    showIncognitoSheet = true
                }
            }
            .padding(.leading, 36)
        }
        .sheet(isPresented: $showIncognitoSheet) {
            IncognitoHelp()
        }
    }
}

func sharedProfileInfo(_ incognito: Bool) -> Text {
    let name = ChatModel.shared.currentUser?.displayName ?? ""
    return Text(
        incognito
        ? "A new random profile will be shared."
        : "Your profile **\(name)** will be shared."
    )
}

private func showInvitationLinkConnectingAlert(cleanup: (() -> Void)?) {
    showAlert(
        NSLocalizedString("Already connecting!", comment: "new chat sheet title"),
        message: NSLocalizedString("You are already connecting via this one-time link!", comment: "new chat sheet message"),
        actions: {[
            okCleanupAlertAction(cleanup: cleanup)
        ]}
    )
}

private func showGroupLinkConnectingAlert(groupInfo: GroupInfo?, cleanup: (() -> Void)?) {
    if let groupInfo = groupInfo {
        if groupInfo.businessChat == nil {
            showAlert(
                NSLocalizedString("Group already exists!", comment: "new chat sheet title"),
                message:
                    String.localizedStringWithFormat(
                        NSLocalizedString("You are already joining the group %@.", comment: "new chat sheet message"),
                        groupInfo.displayName
                    ),
                actions: {[
                    okCleanupAlertAction(cleanup: cleanup)
                ]}
            )
        } else {
            showAlert(
                NSLocalizedString("Chat already exists!", comment: "new chat sheet title"),
                message:
                    String.localizedStringWithFormat(
                        NSLocalizedString("You are already connecting to %@.", comment: "new chat sheet message"),
                        groupInfo.displayName
                    ),
                actions: {[
                    okCleanupAlertAction(cleanup: cleanup)
                ]}
            )
        }
    } else {
        showAlert(
            NSLocalizedString("Already joining the group!", comment: "new chat sheet title"),
            message: NSLocalizedString("You are already joining the group via this link.", comment: "new chat sheet message"),
            actions: {[
                okCleanupAlertAction(cleanup: cleanup)
            ]}
        )
    }
}

private func okCleanupAlertAction(cleanup: (() -> Void)?) -> UIAlertAction {
    UIAlertAction(
        title: NSLocalizedString("Ok", comment: "new chat action"),
        style: .default,
        handler: { _ in
            cleanup?()
        }
    )
}

private func showAskCurrentOrIncognitoProfileSheet(
    title: String,
    actionStyle: UIAlertAction.Style = .default,
    connectionLink: CreatedConnLink,
    connectionPlan: ConnectionPlan?,
    dismiss: Bool,
    cleanup: (() -> Void)?
) {
    showSheet(
        title,
        actions: {[
            UIAlertAction(
                title: NSLocalizedString("Use current profile", comment: "new chat action"),
                style: actionStyle,
                handler: { _ in
                    connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false, cleanup: cleanup)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Use new incognito profile", comment: "new chat action"),
                style: actionStyle,
                handler: { _ in
                    connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true, cleanup: cleanup)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Cancel", comment: "new chat action"),
                style: .default,
                handler: { _ in
                    cleanup?()
                }
            )
        ]}
    )
}

private func showAskCurrentOrIncognitoProfileConnectContactViaAddressSheet(
    contact: Contact,
    dismiss: Bool,
    cleanup: (() -> Void)?
) {
    showSheet(
        String.localizedStringWithFormat(
            NSLocalizedString("Connect with %@", comment: "new chat action"),
            contact.chatViewName
        ),
        actions: {[
            UIAlertAction(
                title: NSLocalizedString("Use current profile", comment: "new chat action"),
                style: .default,
                handler: { _ in
                    connectContactViaAddress_(contact, dismiss: dismiss, incognito: false, cleanup: cleanup)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Use new incognito profile", comment: "new chat action"),
                style: .default,
                handler: { _ in
                    connectContactViaAddress_(contact, dismiss: dismiss, incognito: true, cleanup: cleanup)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Cancel", comment: "new chat action"),
                style: .default,
                handler: { _ in
                    cleanup?()
                }
            )
        ]}
    )
}

private func showOwnGroupLinkConfirmConnectSheet(
    groupInfo: GroupInfo,
    connectionLink: CreatedConnLink,
    connectionPlan: ConnectionPlan?,
    dismiss: Bool,
    cleanup: (() -> Void)?
) {
    showSheet(
        String.localizedStringWithFormat(
            NSLocalizedString("Join your group?\nThis is your link for group %@!", comment: "new chat action"),
            groupInfo.displayName
        ),
        actions: {[
            UIAlertAction(
                title: NSLocalizedString("Open group", comment: "new chat action"),
                style: .default,
                handler: { _ in
                    openKnownGroup(groupInfo, dismiss: dismiss, cleanup: cleanup)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Use current profile", comment: "new chat action"),
                style: .destructive,
                handler: { _ in
                    connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false, cleanup: cleanup)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Use new incognito profile", comment: "new chat action"),
                style: .destructive,
                handler: { _ in
                    connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true, cleanup: cleanup)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Cancel", comment: "new chat action"),
                style: .default,
                handler: { _ in
                    cleanup?()
                }
            )
        ]}
    )
}

private func showPrepareContactAlert(
    connectionLink: CreatedConnLink,
    contactShortLinkData: ContactShortLinkData,
    theme: AppTheme,
    dismiss: Bool,
    cleanup: (() -> Void)?
) {
    showOpenChatAlert(
        profileName: contactShortLinkData.profile.displayName,
        profileFullName: contactShortLinkData.profile.fullName,
        profileImage:
            ProfileImage(
                imageStr: contactShortLinkData.profile.image,
                iconName: contactShortLinkData.business
                            ? "briefcase.circle.fill"
                            : contactShortLinkData.profile.peerType == .bot
                            ? "cube.fill"
                            : "person.crop.circle.fill",
                size: alertProfileImageSize
            ),
        theme: theme,
        cancelTitle: NSLocalizedString("Cancel", comment: "new chat action"),
        confirmTitle: NSLocalizedString("Open new chat", comment: "new chat action"),
        onCancel: { cleanup?() },
        onConfirm: {
            Task {
                do {
                    let chat = try await apiPrepareContact(connLink: connectionLink, contactShortLinkData: contactShortLinkData)
                    await MainActor.run {
                        ChatModel.shared.addChat(Chat(chat))
                        openKnownChat(chat.id, dismiss: dismiss, cleanup: cleanup)
                    }
                } catch let error {
                    logger.error("showPrepareContactAlert apiPrepareContact error: \(error.localizedDescription)")
                    showAlert(NSLocalizedString("Error opening chat", comment: ""), message: responseError(error))
                    await MainActor.run {
                        cleanup?()
                    }
                }
            }
        }
    )
}

private func showPrepareGroupAlert(
    connectionLink: CreatedConnLink,
    groupShortLinkData: GroupShortLinkData,
    theme: AppTheme,
    dismiss: Bool,
    cleanup: (() -> Void)?
) {
    showOpenChatAlert(
        profileName: groupShortLinkData.groupProfile.displayName,
        profileFullName: groupShortLinkData.groupProfile.fullName,
        profileImage: ProfileImage(imageStr: groupShortLinkData.groupProfile.image, iconName: "person.2.circle.fill", size: alertProfileImageSize),
        theme: theme,
        cancelTitle: NSLocalizedString("Cancel", comment: "new chat action"),
        confirmTitle: NSLocalizedString("Open new group", comment: "new chat action"),
        onCancel: { cleanup?() },
        onConfirm: {
            Task {
                do {
                    let chat = try await apiPrepareGroup(connLink: connectionLink, groupShortLinkData: groupShortLinkData)
                    await MainActor.run {
                        ChatModel.shared.addChat(Chat(chat))
                        openKnownChat(chat.id, dismiss: dismiss, cleanup: cleanup)
                    }
                } catch let error {
                    logger.error("showPrepareGroupAlert apiPrepareGroup error: \(error.localizedDescription)")
                    showAlert(NSLocalizedString("Error opening group", comment: ""), message: responseError(error))
                    await MainActor.run {
                        cleanup?()
                    }
                }
            }
        }
    )
}

private func showOpenKnownContactAlert(
    _ contact: Contact,
    theme: AppTheme,
    dismiss: Bool
) {
    showOpenChatAlert(
        profileName: contact.profile.displayName,
        profileFullName: contact.profile.fullName,
        profileImage:
            ProfileImage(
                imageStr: contact.profile.image,
                iconName: contact.chatIconName,
                size: alertProfileImageSize
            ),
        theme: theme,
        cancelTitle: NSLocalizedString("Cancel", comment: "new chat action"),
        confirmTitle:
            contact.nextConnectPrepared
            ? NSLocalizedString("Open new chat", comment: "new chat action")
            : NSLocalizedString("Open chat", comment: "new chat action"),
        onConfirm: {
            openKnownContact(contact, dismiss: dismiss, cleanup: nil)
        }
    )
}

private func showOpenKnownGroupAlert(
    _ groupInfo: GroupInfo,
    theme: AppTheme,
    dismiss: Bool
) {
    showOpenChatAlert(
        profileName: groupInfo.groupProfile.displayName,
        profileFullName: groupInfo.groupProfile.fullName,
        profileImage:
            ProfileImage(
                imageStr: groupInfo.groupProfile.image,
                iconName: groupInfo.chatIconName,
                size: alertProfileImageSize
            ),
        theme: theme,
        cancelTitle: NSLocalizedString("Cancel", comment: "new chat action"),
        confirmTitle:
            groupInfo.businessChat == nil
            ? ( groupInfo.nextConnectPrepared
                ? NSLocalizedString("Open new group", comment: "new chat action")
                : NSLocalizedString("Open group", comment: "new chat action")
              )
            : ( groupInfo.nextConnectPrepared
                ? NSLocalizedString("Open new chat", comment: "new chat action")
                : NSLocalizedString("Open chat", comment: "new chat action")
              ),
        onConfirm: {
            openKnownGroup(groupInfo, dismiss: dismiss, cleanup: nil)
        }
    )
}

func planAndConnect(
    _ shortOrFullLink: String,
    theme: AppTheme,
    dismiss: Bool,
    cleanup: (() -> Void)? = nil,
    filterKnownContact: ((Contact) -> Void)? = nil,
    filterKnownGroup: ((GroupInfo) -> Void)? = nil
) {
    ConnectProgressManager.shared.cancelConnectProgress()
    let inProgress = BoxedValue(true)
    connectTask(inProgress)
    ConnectProgressManager.shared.startConnectProgress(NSLocalizedString("Loading profile…", comment: "in progress text")) {
        inProgress.boxedValue = false
        cleanup?()
    }

    func connectTask(_ inProgress: BoxedValue<Bool>) {
        Task {
            let (result, alert) = await apiConnectPlan(connLink: shortOrFullLink, inProgress: inProgress)
            await MainActor.run {
                ConnectProgressManager.shared.stopConnectProgress()
            }
            if !inProgress.boxedValue { return }
            if let (connectionLink, connectionPlan) = result {
                switch connectionPlan {
                case let .invitationLink(ilp):
                    switch ilp {
                    case let .ok(contactSLinkData_):
                        if let contactSLinkData = contactSLinkData_ {
                            logger.debug("planAndConnect, .invitationLink, .ok, short link data present")
                            await MainActor.run {
                                showPrepareContactAlert(
                                    connectionLink: connectionLink,
                                    contactShortLinkData: contactSLinkData,
                                    theme: theme,
                                    dismiss: dismiss,
                                    cleanup: cleanup
                                )
                            }
                        } else {
                            logger.debug("planAndConnect, .invitationLink, .ok, no short link data")
                            await MainActor.run {
                                showAskCurrentOrIncognitoProfileSheet(
                                    title: NSLocalizedString("Connect via one-time link", comment: "new chat sheet title"),
                                    connectionLink: connectionLink,
                                    connectionPlan: connectionPlan,
                                    dismiss: dismiss,
                                    cleanup: cleanup
                                )
                            }
                        }
                    case .ownLink:
                        logger.debug("planAndConnect, .invitationLink, .ownLink")
                        await MainActor.run {
                            showAskCurrentOrIncognitoProfileSheet(
                                title: NSLocalizedString("Connect to yourself?\nThis is your own one-time link!", comment: "new chat sheet title"),
                                actionStyle: .destructive,
                                connectionLink: connectionLink,
                                connectionPlan: connectionPlan,
                                dismiss: dismiss,
                                cleanup: cleanup
                            )
                        }
                    case let .connecting(contact_):
                        logger.debug("planAndConnect, .invitationLink, .connecting")
                        await MainActor.run {
                            if let contact = contact_ {
                                if let f = filterKnownContact {
                                    f(contact)
                                } else {
                                    showOpenKnownContactAlert(contact, theme: theme, dismiss: dismiss)
                                }
                            } else {
                                showInvitationLinkConnectingAlert(cleanup: cleanup)
                            }
                        }
                    case let .known(contact):
                        logger.debug("planAndConnect, .invitationLink, .known")
                        await MainActor.run {
                            if let f = filterKnownContact {
                                f(contact)
                            } else {
                                showOpenKnownContactAlert(contact, theme: theme, dismiss: dismiss)
                            }
                        }
                    }
                case let .contactAddress(cap):
                    switch cap {
                    case let .ok(contactSLinkData_):
                        if let contactSLinkData = contactSLinkData_ {
                            logger.debug("planAndConnect, .contactAddress, .ok, short link data present")
                            await MainActor.run {
                                showPrepareContactAlert(
                                    connectionLink: connectionLink,
                                    contactShortLinkData: contactSLinkData,
                                    theme: theme,
                                    dismiss: dismiss,
                                    cleanup: cleanup
                                )
                            }
                        } else {
                            logger.debug("planAndConnect, .contactAddress, .ok, no short link data")
                            await MainActor.run {
                                showAskCurrentOrIncognitoProfileSheet(
                                    title: NSLocalizedString("Connect via contact address", comment: "new chat sheet title"),
                                    connectionLink: connectionLink,
                                    connectionPlan: connectionPlan,
                                    dismiss: dismiss,
                                    cleanup: cleanup
                                )
                            }
                        }
                    case .ownLink:
                        logger.debug("planAndConnect, .contactAddress, .ownLink")
                        await MainActor.run {
                            showAskCurrentOrIncognitoProfileSheet(
                                title: NSLocalizedString("Connect to yourself?\nThis is your own SimpleX address!", comment: "new chat sheet title"),
                                actionStyle: .destructive,
                                connectionLink: connectionLink,
                                connectionPlan: connectionPlan,
                                dismiss: dismiss,
                                cleanup: cleanup
                            )
                        }
                    case .connectingConfirmReconnect:
                        logger.debug("planAndConnect, .contactAddress, .connectingConfirmReconnect")
                        await MainActor.run {
                            showAskCurrentOrIncognitoProfileSheet(
                                title: NSLocalizedString("You have already requested connection!\nRepeat connection request?", comment: "new chat sheet title"),
                                actionStyle: .destructive,
                                connectionLink: connectionLink,
                                connectionPlan: connectionPlan,
                                dismiss: dismiss,
                                cleanup: cleanup
                            )
                        }
                    case let .connectingProhibit(contact):
                        logger.debug("planAndConnect, .contactAddress, .connectingProhibit")
                        await MainActor.run {
                            if let f = filterKnownContact {
                                f(contact)
                            } else {
                                showOpenKnownContactAlert(contact, theme: theme, dismiss: dismiss)
                            }
                        }
                    case let .known(contact):
                        logger.debug("planAndConnect, .contactAddress, .known")
                        await MainActor.run {
                            if let f = filterKnownContact {
                                f(contact)
                            } else {
                                showOpenKnownContactAlert(contact, theme: theme, dismiss: dismiss)
                            }
                        }
                    case let .contactViaAddress(contact):
                        logger.debug("planAndConnect, .contactAddress, .contactViaAddress")
                        await MainActor.run {
                            showAskCurrentOrIncognitoProfileConnectContactViaAddressSheet(
                                contact: contact,
                                dismiss: dismiss,
                                cleanup: cleanup
                            )
                        }
                    }
                case let .groupLink(glp):
                    switch glp {
                    case let .ok(groupSLinkData_):
                        if let groupSLinkData = groupSLinkData_ {
                            logger.debug("planAndConnect, .groupLink, .ok, short link data present")
                            await MainActor.run {
                                showPrepareGroupAlert(
                                    connectionLink: connectionLink,
                                    groupShortLinkData: groupSLinkData,
                                    theme: theme,
                                    dismiss: dismiss,
                                    cleanup: cleanup
                                )
                            }
                        } else {
                            logger.debug("planAndConnect, .groupLink, .ok, no short link data")
                            await MainActor.run {
                                showAskCurrentOrIncognitoProfileSheet(
                                    title: NSLocalizedString("Join group", comment: "new chat sheet title"),
                                    connectionLink: connectionLink,
                                    connectionPlan: connectionPlan,
                                    dismiss: dismiss,
                                    cleanup: cleanup
                                )
                            }
                        }
                    case let .ownLink(groupInfo):
                        logger.debug("planAndConnect, .groupLink, .ownLink")
                        await MainActor.run {
                            if let f = filterKnownGroup {
                                f(groupInfo)
                            }
                            showOwnGroupLinkConfirmConnectSheet(
                                groupInfo: groupInfo,
                                connectionLink: connectionLink,
                                connectionPlan: connectionPlan,
                                dismiss: dismiss,
                                cleanup: cleanup
                            )
                        }
                    case .connectingConfirmReconnect:
                        logger.debug("planAndConnect, .groupLink, .connectingConfirmReconnect")
                        await MainActor.run {
                            showAskCurrentOrIncognitoProfileSheet(
                                title: NSLocalizedString("You are already joining the group!\nRepeat join request?", comment: "new chat sheet title"),
                                actionStyle: .destructive,
                                connectionLink: connectionLink,
                                connectionPlan: connectionPlan,
                                dismiss: dismiss,
                                cleanup: cleanup
                            )
                        }
                    case let .connectingProhibit(groupInfo_):
                        logger.debug("planAndConnect, .groupLink, .connectingProhibit")
                        await MainActor.run {
                            showGroupLinkConnectingAlert(groupInfo: groupInfo_, cleanup: cleanup)
                        }
                    case let .known(groupInfo):
                        logger.debug("planAndConnect, .groupLink, .known")
                        await MainActor.run {
                            if let f = filterKnownGroup {
                                f(groupInfo)
                            } else {
                                showOpenKnownGroupAlert(groupInfo, theme: theme, dismiss: dismiss)
                            }
                        }
                    }
                case let .error(chatError):
                    logger.debug("planAndConnect, .error \(chatErrorString(chatError))")
                    showAskCurrentOrIncognitoProfileSheet(
                        title: NSLocalizedString("Connect via link", comment: "new chat sheet title"),
                        connectionLink: connectionLink,
                        connectionPlan: nil,
                        dismiss: dismiss,
                        cleanup: cleanup
                    )
                }
            } else {
                await MainActor.run {
                    if let alert {
                        dismissAllSheets(animated: true) {
                            AlertManager.shared.showAlert(alert)
                            cleanup?()
                        }
                    } else {
                        cleanup?()
                    }
                }
            }
        }
    }
}

private func connectContactViaAddress_(_ contact: Contact, dismiss: Bool, incognito: Bool, cleanup: (() -> Void)? = nil) {
    Task {
        if dismiss {
            DispatchQueue.main.async {
                dismissAllSheets(animated: true)
            }
        }
        let ok = await connectContactViaAddress(contact.contactId, incognito, showAlert: { AlertManager.shared.showAlert($0) })
        if ok {
            AlertManager.shared.showAlert(connReqSentAlert(.contact))
        }
        cleanup?()
    }
}

private func connectViaLink(
    _ connectionLink: CreatedConnLink,
    connectionPlan: ConnectionPlan?,
    dismiss: Bool,
    incognito: Bool,
    cleanup: (() -> Void)?
) {
    Task {
        if let (connReqType, pcc) = await apiConnect(incognito: incognito, connLink: connectionLink) {
            await MainActor.run {
                ChatModel.shared.updateContactConnection(pcc)
            }
            let crt: ConnReqType
            crt = if let plan = connectionPlan {
                planToConnReqType(plan) ?? connReqType
            } else {
                connReqType
            }
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        AlertManager.shared.showAlert(connReqSentAlert(crt))
                    }
                } else {
                    AlertManager.shared.showAlert(connReqSentAlert(crt))
                }
            }
        } else {
            if dismiss {
                DispatchQueue.main.async {
                    dismissAllSheets(animated: true)
                }
            }
        }
        cleanup?()
    }
}

func openKnownContact(_ contact: Contact, dismiss: Bool, cleanup: (() -> Void)?) {
    if let c = ChatModel.shared.getContactChat(contact.contactId) {
        openKnownChat(c.id, dismiss: dismiss, cleanup: cleanup)
    }
}

func openKnownGroup(_ groupInfo: GroupInfo, dismiss: Bool, cleanup: (() -> Void)?) {
    if let g = ChatModel.shared.getGroupChat(groupInfo.groupId) {
        openKnownChat(g.id, dismiss: dismiss, cleanup: cleanup)
    }
}

func openKnownChat(_ chatId: ChatId, dismiss: Bool, cleanup: (() -> Void)?) {
    if dismiss {
        dismissAllSheets(animated: true) {
            ItemsModel.shared.loadOpenChat(chatId) {
                cleanup?()
            }
        }
    } else {
        ItemsModel.shared.loadOpenChat(chatId) {
            cleanup?()
        }
    }
}

func contactAlreadyConnectingAlert(_ contact: Contact) -> Alert {
    mkAlert(
        title: "Contact already exists",
        message: "You are already connecting to \(contact.displayName)."
    )
}

func groupAlreadyExistsAlert(_ groupInfo: GroupInfo) -> Alert {
    groupInfo.businessChat == nil
    ? mkAlert(
        title: "Group already exists",
        message: "You are already in group \(groupInfo.displayName)."
    )
    : mkAlert(
        title: "Chat already exists",
        message: "You are already connected with \(groupInfo.displayName)."
    )
}

enum ConnReqType: Equatable {
    case invitation
    case contact
    case groupLink

    var connReqSentText: LocalizedStringKey {
        switch self {
        case .invitation: return "You will be connected when your contact's device is online, please wait or check later!"
        case .contact: return "You will be connected when your connection request is accepted, please wait or check later!"
        case .groupLink: return "You will be connected when group link host's device is online, please wait or check later!"
        }
    }
}

private func planToConnReqType(_ connectionPlan: ConnectionPlan) -> ConnReqType? {
    switch connectionPlan {
    case .invitationLink: .invitation
    case .contactAddress: .contact
    case .groupLink: .groupLink
    case .error: nil
    }
}

func connReqSentAlert(_ type: ConnReqType) -> Alert {
    return mkAlert(
        title: "Connection request sent!",
        message: type.connReqSentText
    )
}

struct NewChatView_Previews: PreviewProvider {
    static var previews: some View {
        @State var parentAlert: SomeAlert?
        @State var contactConnection: PendingContactConnection? = nil

        NewChatView(
            selection: .invite
        )
    }
}
