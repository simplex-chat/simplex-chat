//
//  NewChatView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

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
    case planAndConnectAlert(alert: PlanAndConnectAlert)
    case newChatSomeAlert(alert: SomeAlert)
    var id: String {
        switch self {
        case let .planAndConnectAlert(alert): return "planAndConnectAlert \(alert.id)"
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
            case let .planAndConnectAlert(alert):
                return planAndConnectAlert(alert, dismiss: true, cleanup: { pastedLink = "" })
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
                    .sorted { u, _ in u.activeUser }
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
                        if let contactConn = contactConnection {
                            let conn = try await apiChangeConnectionUser(connId: contactConn.pccConnId, userId: profile.userId)
                            await MainActor.run {
                                contactConnection = conn
                                connLinkInvitation = conn.connLinkInv ?? CreatedConnLink(connFullLink: "", connShortLink: nil)
                                incognitoEnabled = false
                                chatModel.updateContactConnection(conn)
                            }
                            do {
                                try await changeActiveUserAsync_(profile.userId, viewPwd: profile.hidden ? trimmedSearchTextOrPassword : nil )
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
                                            message: Text("Your connection was moved to \(profile.chatViewName) but an unexpected error occurred while redirecting you to the profile.")
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
                let otherProfiles = filteredProfiles.filter { u in u.userId != activeProfile?.userId }
                
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
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var showQRCodeScanner: Bool
    @Binding var pastedLink: String
    @Binding var alert: NewChatViewAlert?
    @State private var sheet: PlanAndConnectActionSheet?
    @State private var pasteboardHasStrings = UIPasteboard.general.hasStrings

    var body: some View {
        List {
            Section(header: Text("Paste the link you received").foregroundColor(theme.colors.secondary)) {
                pasteLinkView()
            }
            Section(header: Text("Or scan QR code").foregroundColor(theme.colors.secondary)) {
                ScannerInView(showQRCodeScanner: $showQRCodeScanner, processQRCode: processQRCode)
            }
        }
        .actionSheet(item: $sheet) { s in
            planAndConnectActionSheet(s, dismiss: true, cleanup: { pastedLink = "" })
        }
    }

    @ViewBuilder private func pasteLinkView() -> some View {
        if pastedLink == "" {
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
        } else {
            linkTextView(pastedLink)
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
        planAndConnect(
            link,
            showAlert: { alert = .planAndConnectAlert(alert: $0) },
            showActionSheet: { sheet = $0 },
            dismiss: true,
            incognito: nil
        )
    }
}

struct ScannerInView: View {
    @Binding var showQRCodeScanner: Bool
    let processQRCode: (_ resp: Result<ScanResult, ScanError>) -> Void
    @State private var cameraAuthorizationStatus: AVAuthorizationStatus?
    var scanMode: ScanMode = .continuous

    var body: some View {
        Group {
            if showQRCodeScanner, case .authorized = cameraAuthorizationStatus {
                CodeScannerView(codeTypes: [.qr], scanMode: scanMode, completion: processQRCode)
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

enum PlanAndConnectAlert: Identifiable {
    case ownInvitationLinkConfirmConnect(connectionLink: CreatedConnLink, connectionPlan: ConnectionPlan, incognito: Bool)
    case invitationLinkConnecting(connectionLink: CreatedConnLink)
    case ownContactAddressConfirmConnect(connectionLink: CreatedConnLink, connectionPlan: ConnectionPlan, incognito: Bool)
    case contactAddressConnectingConfirmReconnect(connectionLink: CreatedConnLink, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConfirmConnect(connectionLink: CreatedConnLink, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConnectingConfirmReconnect(connectionLink: CreatedConnLink, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConnecting(connectionLink: CreatedConnLink, groupInfo: GroupInfo?)
    case error(shortOrFullLink: String, alert: Alert)

    var id: String {
        switch self {
        case let .ownInvitationLinkConfirmConnect(connectionLink, _, _): return "ownInvitationLinkConfirmConnect \(connectionLink.connFullLink)"
        case let .invitationLinkConnecting(connectionLink): return "invitationLinkConnecting \(connectionLink.connFullLink)"
        case let .ownContactAddressConfirmConnect(connectionLink, _, _): return "ownContactAddressConfirmConnect \(connectionLink.connFullLink)"
        case let .contactAddressConnectingConfirmReconnect(connectionLink, _, _): return "contactAddressConnectingConfirmReconnect \(connectionLink.connFullLink)"
        case let .groupLinkConfirmConnect(connectionLink, _, _): return "groupLinkConfirmConnect \(connectionLink.connFullLink)"
        case let .groupLinkConnectingConfirmReconnect(connectionLink, _, _): return "groupLinkConnectingConfirmReconnect \(connectionLink.connFullLink)"
        case let .groupLinkConnecting(connectionLink, _): return "groupLinkConnecting \(connectionLink.connFullLink)"
        case let .error(shortOrFullLink, alert): return "error \(shortOrFullLink)"
        }
    }
}

func planAndConnectAlert(_ alert: PlanAndConnectAlert, dismiss: Bool, cleanup: (() -> Void)? = nil) -> Alert {
    switch alert {
    case let .ownInvitationLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Connect to yourself?"),
            message: Text("This is your own one-time link!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case .invitationLinkConnecting:
        return Alert(
            title: Text("Already connecting!"),
            message: Text("You are already connecting via this one-time link!"),
            dismissButton: .default(Text("OK")) { cleanup?() }
        )
    case let .ownContactAddressConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Connect to yourself?"),
            message: Text("This is your own SimpleX address!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case let .contactAddressConnectingConfirmReconnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Repeat connection request?"),
            message: Text("You have already requested connection via this address!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case let .groupLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Join group?"),
            message: Text("You will connect to all group members."),
            primaryButton: .default(
                Text(incognito ? "Join incognito" : "Join"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case let .groupLinkConnectingConfirmReconnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Repeat join request?"),
            message: Text("You are already joining the group via this link!"),
            primaryButton: .destructive(
                Text(incognito ? "Join incognito" : "Join"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case let .groupLinkConnecting(_, groupInfo):
        if let groupInfo = groupInfo {
            return groupInfo.businessChat == nil
            ? Alert(
                title: Text("Group already exists!"),
                message: Text("You are already joining the group \(groupInfo.displayName)."),
                dismissButton: .default(Text("OK")) { cleanup?() }
            )
            : Alert(
                title: Text("Chat already exists!"),
                message: Text("You are already connecting to \(groupInfo.displayName)."),
                dismissButton: .default(Text("OK")) { cleanup?() }
            )
        } else {
            return Alert(
                title: Text("Already joining the group!"),
                message: Text("You are already joining the group via this link."),
                dismissButton: .default(Text("OK")) { cleanup?() }
            )
        }
    case let .error(_, alert): return alert
    }
}

enum PlanAndConnectActionSheet: Identifiable {
    case askCurrentOrIncognitoProfile(connectionLink: CreatedConnLink, connectionPlan: ConnectionPlan?, title: LocalizedStringKey)
    case askCurrentOrIncognitoProfileDestructive(connectionLink: CreatedConnLink, connectionPlan: ConnectionPlan, title: LocalizedStringKey)
    case askCurrentOrIncognitoProfileConnectContactViaAddress(contact: Contact)
    case ownGroupLinkConfirmConnect(connectionLink: CreatedConnLink, connectionPlan: ConnectionPlan, incognito: Bool?, groupInfo: GroupInfo)

    var id: String {
        switch self {
        case let .askCurrentOrIncognitoProfile(connectionLink, _, _): return "askCurrentOrIncognitoProfile \(connectionLink.connFullLink)"
        case let .askCurrentOrIncognitoProfileDestructive(connectionLink, _, _): return "askCurrentOrIncognitoProfileDestructive \(connectionLink.connFullLink)"
        case let .askCurrentOrIncognitoProfileConnectContactViaAddress(contact): return "askCurrentOrIncognitoProfileConnectContactViaAddress \(contact.contactId)"
        case let .ownGroupLinkConfirmConnect(connectionLink, _, _, _): return "ownGroupLinkConfirmConnect \(connectionLink.connFullLink)"
        }
    }
}

func planAndConnectActionSheet(_ sheet: PlanAndConnectActionSheet, dismiss: Bool, cleanup: (() -> Void)? = nil) -> ActionSheet {
    switch sheet {
    case let .askCurrentOrIncognitoProfile(connectionLink, connectionPlan, title):
        return ActionSheet(
            title: Text(title),
            buttons: [
                .default(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false, cleanup: cleanup) },
                .default(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true, cleanup: cleanup) },
                .cancel() { cleanup?() }
            ]
        )
    case let .askCurrentOrIncognitoProfileDestructive(connectionLink, connectionPlan, title):
        return ActionSheet(
            title: Text(title),
            buttons: [
                .destructive(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false, cleanup: cleanup) },
                .destructive(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true, cleanup: cleanup) },
                .cancel() { cleanup?() }
            ]
        )
    case let .askCurrentOrIncognitoProfileConnectContactViaAddress(contact):
        return ActionSheet(
            title: Text("Connect with \(contact.chatViewName)"),
            buttons: [
                .default(Text("Use current profile")) { connectContactViaAddress_(contact, dismiss: dismiss, incognito: false, cleanup: cleanup) },
                .default(Text("Use new incognito profile")) { connectContactViaAddress_(contact, dismiss: dismiss, incognito: true, cleanup: cleanup) },
                .cancel() { cleanup?() }
            ]
        )
    case let .ownGroupLinkConfirmConnect(connectionLink, connectionPlan, incognito, groupInfo):
        if let incognito = incognito {
            return ActionSheet(
                title: Text("Join your group?\nThis is your link for group \(groupInfo.displayName)!"),
                buttons: [
                    .default(Text("Open group")) { openKnownGroup(groupInfo, dismiss: dismiss, showAlreadyExistsAlert: nil) },
                    .destructive(Text(incognito ? "Join incognito" : "Join with current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) },
                    .cancel() { cleanup?() }
                ]
            )
        } else {
            return ActionSheet(
                title: Text("Join your group?\nThis is your link for group \(groupInfo.displayName)!"),
                buttons: [
                    .default(Text("Open group")) { openKnownGroup(groupInfo, dismiss: dismiss, showAlreadyExistsAlert: nil) },
                    .destructive(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false, cleanup: cleanup) },
                    .destructive(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true, cleanup: cleanup) },
                    .cancel() { cleanup?() }
                ]
            )
        }
    }
}

func planAndConnect(
    _ shortOrFullLink: String,
    showAlert: @escaping (PlanAndConnectAlert) -> Void,
    showActionSheet: @escaping (PlanAndConnectActionSheet) -> Void,
    dismiss: Bool,
    incognito: Bool?,
    cleanup: (() -> Void)? = nil,
    filterKnownContact: ((Contact) -> Void)? = nil,
    filterKnownGroup: ((GroupInfo) -> Void)? = nil
) {
    Task {
        let (result, alert) = await apiConnectPlan(connLink: shortOrFullLink)
        if let (connectionLink, connectionPlan) = result {
            switch connectionPlan {
            case let .invitationLink(ilp):
                switch ilp {
                case .ok:
                    logger.debug("planAndConnect, .invitationLink, .ok, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup)
                    } else {
                        await MainActor.run {
                            showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect via one-time link"))
                        }
                    }
                case .ownLink:
                    logger.debug("planAndConnect, .invitationLink, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let incognito = incognito {
                            showAlert(.ownInvitationLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                        } else {
                            showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect to yourself?\nThis is your own one-time link!"))
                        }
                    }
                case let .connecting(contact_):
                    logger.debug("planAndConnect, .invitationLink, .connecting, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let contact = contact_ {
                            if let f = filterKnownContact {
                                f(contact)
                            } else {
                                openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyConnectingAlert(contact)) }
                            }
                        } else {
                            showAlert(.invitationLinkConnecting(connectionLink: connectionLink))
                        }
                    }
                case let .known(contact):
                    logger.debug("planAndConnect, .invitationLink, .known, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let f = filterKnownContact {
                            f(contact)
                        } else {
                            openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyExistsAlert(contact)) }
                        }
                    }
                }
            case let .contactAddress(cap):
                switch cap {
                case .ok:
                    logger.debug("planAndConnect, .contactAddress, .ok, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup)
                    } else {
                        await MainActor.run {
                            showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect via contact address"))
                        }
                    }
                case .ownLink:
                    logger.debug("planAndConnect, .contactAddress, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let incognito = incognito {
                            showAlert(.ownContactAddressConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                        } else {
                            showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect to yourself?\nThis is your own SimpleX address!"))
                        }
                    }
                case .connectingConfirmReconnect:
                    logger.debug("planAndConnect, .contactAddress, .connectingConfirmReconnect, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let incognito = incognito {
                            showAlert(.contactAddressConnectingConfirmReconnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                        } else {
                            showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "You have already requested connection!\nRepeat connection request?"))
                        }
                    }
                case let .connectingProhibit(contact):
                    logger.debug("planAndConnect, .contactAddress, .connectingProhibit, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let f = filterKnownContact {
                            f(contact)
                        } else {
                            openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyConnectingAlert(contact)) }
                        }
                    }
                case let .known(contact):
                    logger.debug("planAndConnect, .contactAddress, .known, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let f = filterKnownContact {
                            f(contact)
                        } else {
                            openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyExistsAlert(contact)) }
                        }
                    }
                case let .contactViaAddress(contact):
                    logger.debug("planAndConnect, .contactAddress, .contactViaAddress, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectContactViaAddress_(contact, dismiss: dismiss, incognito: incognito, cleanup: cleanup)
                    } else {
                        await MainActor.run {
                            showActionSheet(.askCurrentOrIncognitoProfileConnectContactViaAddress(contact: contact))
                        }
                    }
                }
            case let .groupLink(glp):
                switch glp {
                case .ok:
                    await MainActor.run {
                        if let incognito = incognito {
                            showAlert(.groupLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                        } else {
                            showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Join group"))
                        }
                    }
                case let .ownLink(groupInfo):
                    logger.debug("planAndConnect, .groupLink, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let f = filterKnownGroup {
                            f(groupInfo)
                        }
                        showActionSheet(.ownGroupLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito, groupInfo: groupInfo))
                    }
                case .connectingConfirmReconnect:
                    logger.debug("planAndConnect, .groupLink, .connectingConfirmReconnect, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let incognito = incognito {
                            showAlert(.groupLinkConnectingConfirmReconnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                        } else {
                            showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "You are already joining the group!\nRepeat join request?"))
                        }
                    }
                case let .connectingProhibit(groupInfo_):
                    logger.debug("planAndConnect, .groupLink, .connectingProhibit, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        showAlert(.groupLinkConnecting(connectionLink: connectionLink, groupInfo: groupInfo_))
                    }
                case let .known(groupInfo):
                    logger.debug("planAndConnect, .groupLink, .known, incognito=\(incognito?.description ?? "nil")")
                    await MainActor.run {
                        if let f = filterKnownGroup {
                            f(groupInfo)
                        } else {
                            openKnownGroup(groupInfo, dismiss: dismiss) { AlertManager.shared.showAlert(groupAlreadyExistsAlert(groupInfo)) }
                        }
                    }
                }
            case let .error(chatError):
                logger.debug("planAndConnect, .error \(chatErrorString(chatError))")
                if let incognito = incognito {
                    connectViaLink(connectionLink, connectionPlan: nil, dismiss: dismiss, incognito: incognito, cleanup: cleanup)
                } else {
                    showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: nil, title: "Connect via link"))
                }
            }
        } else if let alert {
            await MainActor.run {
                showAlert(.error(shortOrFullLink: shortOrFullLink, alert: alert))
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

func openKnownContact(_ contact: Contact, dismiss: Bool, showAlreadyExistsAlert: (() -> Void)?) {
    let m = ChatModel.shared
    if let c = m.getContactChat(contact.contactId) {
        if dismiss {
            dismissAllSheets(animated: true) {
                ItemsModel.shared.loadOpenChat(c.id) {
                    showAlreadyExistsAlert?()
                }
            }
        } else {
            ItemsModel.shared.loadOpenChat(c.id) {
                showAlreadyExistsAlert?()
            }
        }
    }
}

func openKnownGroup(_ groupInfo: GroupInfo, dismiss: Bool, showAlreadyExistsAlert: (() -> Void)?) {
    let m = ChatModel.shared
    if let g = m.getGroupChat(groupInfo.groupId) {
        if dismiss {
            dismissAllSheets(animated: true) {
                ItemsModel.shared.loadOpenChat(g.id) {
                    showAlreadyExistsAlert?()
                }
            }
        } else {
            ItemsModel.shared.loadOpenChat(g.id) {
                showAlreadyExistsAlert?()
            }
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
