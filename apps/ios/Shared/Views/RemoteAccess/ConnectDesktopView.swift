//
//  ConnectDesktopView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 13/10/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import CodeScanner

struct ConnectDesktopView: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    var viaSettings = false
    @AppStorage(DEFAULT_DEVICE_NAME_FOR_REMOTE_ACCESS) private var deviceName = UIDevice.current.name
    @AppStorage(DEFAULT_CONFIRM_REMOTE_SESSIONS) private var confirmRemoteSessions = false
    @AppStorage(DEFAULT_CONNECT_REMOTE_VIA_MULTICAST) private var connectRemoteViaMulticast = true
    @AppStorage(DEFAULT_CONNECT_REMOTE_VIA_MULTICAST_AUTO) private var connectRemoteViaMulticastAuto = true
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var sessionAddress: String = ""
    @State private var remoteCtrls: [RemoteCtrlInfo] = []
    @State private var alert: ConnectDesktopAlert?
    @State private var showConnectScreen = true
    @State private var showQRCodeScanner = true
    @State private var firstAppearance = true

    private var useMulticast: Bool {
        connectRemoteViaMulticast && !remoteCtrls.isEmpty
    }

    private enum ConnectDesktopAlert: Identifiable {
        case unlinkDesktop(rc: RemoteCtrlInfo)
        case disconnectDesktop(action: UserDisconnectAction)
        case badInvitationError
        case badVersionError(version: String?)
        case desktopDisconnectedError
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case let .unlinkDesktop(rc): "unlinkDesktop \(rc.remoteCtrlId)"
            case let .disconnectDesktop(action): "disconnectDecktop \(action)"
            case .badInvitationError: "badInvitationError"
            case let .badVersionError(v): "badVersionError \(v ?? "")"
            case .desktopDisconnectedError: "desktopDisconnectedError"
            case let .error(title, _): "error \(title)"
            }
        }
    }

    private enum UserDisconnectAction: String {
        case back
        case dismiss // TODO dismiss settings after confirmation
    }

    var body: some View {
        if viaSettings {
            viewBody
                .modifier(BackButton(label: "Back") {
                    if m.activeRemoteCtrl {
                        alert = .disconnectDesktop(action: .back)
                    } else {
                        dismiss()
                    }
                })
        } else {
            NavigationView {
                viewBody
            }
        }
    }

    var viewBody: some View {
        Group {
            let discovery = m.remoteCtrlSession?.discovery
            if discovery == true || (discovery == nil && !showConnectScreen) {
                searchingDesktopView()
            } else if let session = m.remoteCtrlSession {
                switch session.sessionState {
                case .starting: connectingDesktopView(session, nil)
                case .searching: searchingDesktopView()
                case let .found(rc, compatible): foundDesktopView(session, rc, compatible)
                case let .connecting(rc_): connectingDesktopView(session, rc_)
                case let .pendingConfirmation(rc_, sessCode):
                    if confirmRemoteSessions || rc_ == nil {
                        verifySessionView(session, rc_, sessCode)
                    } else {
                        connectingDesktopView(session, rc_).onAppear {
                            verifyDesktopSessionCode(sessCode)
                        }
                    }
                case let .connected(rc, _): activeSessionView(session, rc)
                }
            // The hack below prevents camera freezing when exiting linked devices view.
            // Using showQRCodeScanner inside connectDesktopView or passing it as parameter still results in freezing.
            } else if showQRCodeScanner || firstAppearance {
                connectDesktopView()
            } else {
                connectDesktopView(showScanner: false)
            }
        }
        .onAppear {
            setDeviceName(deviceName)
            updateRemoteCtrls()
            showConnectScreen = !useMulticast
            if m.remoteCtrlSession != nil {
                disconnectDesktop()
            } else if useMulticast {
                findKnownDesktop()
            }
            // The hack below prevents camera freezing when exiting linked devices view.
            // `firstAppearance` prevents camera flicker when the view first opens.
            // moving `showQRCodeScanner = false` to `onDisappear` (to avoid `firstAppearance`) does not prevent freeze.
            showQRCodeScanner = false
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
                firstAppearance = false
                showQRCodeScanner = true
            }
        }
        .onDisappear {
            if m.remoteCtrlSession != nil {
                showConnectScreen = false
                disconnectDesktop()
            }
        }
        .onChange(of: deviceName) {
            setDeviceName($0)
        }
        .onChange(of: m.activeRemoteCtrl) {
            UIApplication.shared.isIdleTimerDisabled = $0
        }
        .alert(item: $alert) { a in
            switch a {
            case let .unlinkDesktop(rc):
                Alert(
                    title: Text("Unlink desktop?"),
                    primaryButton: .destructive(Text("Unlink")) {
                        unlinkDesktop(rc)
                    },
                    secondaryButton: .cancel()
                )
            case let .disconnectDesktop(action):
                Alert(
                    title: Text("Disconnect desktop?"),
                    primaryButton: .destructive(Text("Disconnect")) {
                        disconnectDesktop(action)
                    },
                    secondaryButton: .cancel()
                )
            case .badInvitationError:
                Alert(title: Text("Bad desktop address"))
            case let .badVersionError(v):
                Alert(
                    title: Text("Incompatible version"),
                    message: Text("Desktop app version \(v ?? "") is not compatible with this app.")
                )
            case .desktopDisconnectedError:
                Alert(title: Text("Connection terminated"))
            case let .error(title, error):
                Alert(title: Text(title), message: Text(error))
            }
        }
        .interactiveDismissDisabled(m.activeRemoteCtrl)
    }

    private func connectDesktopView(showScanner: Bool = true) -> some View {
        List {
            Section("This device name") {
                devicesView()
            }
            if showScanner {
                scanDesctopAddressView()
            }
            if developerTools {
                desktopAddressView()
            }
        }
        .navigationTitle("Connect to desktop")
    }

    private func connectingDesktopView(_ session: RemoteCtrlSession, _ rc: RemoteCtrlInfo?) -> some View {
        List {
            Section("Connecting to desktop") {
                ctrlDeviceNameText(session, rc)
                ctrlDeviceVersionText(session)
            }

            if let sessCode = session.sessionCode {
                Section("Session code") {
                    sessionCodeText(sessCode)
                }
            }

            Section {
                disconnectButton()
            }
        }
        .navigationTitle("Connecting to desktop")
    }

    private func searchingDesktopView() -> some View {
        List {
            Section("This device name") {
                devicesView()
            }
            Section("Found desktop") {
                Text("Waiting for desktop...").italic()
                Button {
                    disconnectDesktop()
                } label: {
                    Label("Scan QR code", systemImage: "qrcode")
                }
            }
        }
        .navigationTitle("Connecting to desktop")
    }

    @ViewBuilder private func foundDesktopView(_ session: RemoteCtrlSession, _ rc: RemoteCtrlInfo, _ compatible: Bool) -> some View {
        let v = List {
            Section("This device name") {
                devicesView()
            }
            Section("Found desktop") {
                ctrlDeviceNameText(session, rc)
                ctrlDeviceVersionText(session)
                if !compatible {
                    Text("Not compatible!").foregroundColor(.red)
                } else if !connectRemoteViaMulticastAuto {
                    Button {
                        confirmKnownDesktop(rc)
                    } label: {
                        Label("Connect", systemImage: "checkmark")
                    }
                }
            }
            if !compatible && !connectRemoteViaMulticastAuto {
                Section {
                    disconnectButton("Cancel")
                }
            }
        }
        .navigationTitle("Found desktop")

        if compatible && connectRemoteViaMulticastAuto {
            v.onAppear { confirmKnownDesktop(rc) }
        } else {
            v
        }
    }

    private func verifySessionView(_ session: RemoteCtrlSession, _ rc: RemoteCtrlInfo?, _ sessCode: String) -> some View {
        List {
            Section("Connected to desktop") {
                ctrlDeviceNameText(session, rc)
                ctrlDeviceVersionText(session)
            }

            Section("Verify code with desktop") {
                sessionCodeText(sessCode)
                Button {
                    verifyDesktopSessionCode(sessCode)
                } label: {
                    Label("Confirm", systemImage: "checkmark")
                }
            }

            Section {
                disconnectButton()
            }
        }
        .navigationTitle("Verify connection")
    }

    private func ctrlDeviceNameText(_ session: RemoteCtrlSession, _ rc: RemoteCtrlInfo?) -> Text {
        var t = Text(rc?.deviceViewName ?? session.ctrlAppInfo?.deviceName ?? "")
        if (rc == nil) {
            t = t + Text(" ") + Text("(new)").italic()
        }
        return t
    }

    private func ctrlDeviceVersionText(_ session: RemoteCtrlSession) -> Text {
        let v = session.ctrlAppInfo?.appVersionRange.maxVersion
        var t = Text("v\(v ?? "")")
        if v != session.appVersion {
            t = t + Text(" ") + Text("(this device v\(session.appVersion))").italic()
        }
        return t
    }

    private func activeSessionView(_ session: RemoteCtrlSession, _ rc: RemoteCtrlInfo) -> some View {
        List {
            Section("Connected desktop") {
                Text(rc.deviceViewName)
                ctrlDeviceVersionText(session)
            }

            if let sessCode = session.sessionCode {
                Section("Session code") {
                    sessionCodeText(sessCode)
                }
            }

            Section {
                disconnectButton()
            } footer: {
                // This is specific to iOS
                Text("Keep the app open to use it from desktop")
            }
        }
        .navigationTitle("Connected to desktop")
    }

    private func sessionCodeText(_ code: String) -> some View {
        Text(code.prefix(23))
    }

    private func devicesView() -> some View {
        Group {
            TextField("Enter this device name…", text: $deviceName)
            if !remoteCtrls.isEmpty {
                NavigationLink {
                    linkedDesktopsView()
                } label: {
                    Text("Linked desktops")
                }
            }
        }
    }

    private func scanDesctopAddressView() -> some View {
        Section("Scan QR code from desktop") {
            CodeScannerView(codeTypes: [.qr], scanMode: .oncePerCode, completion: processDesktopQRCode)
                .aspectRatio(1, contentMode: .fit)
                .cornerRadius(12)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .padding(.horizontal)
        }
    }

    private func desktopAddressView() -> some View {
        Section("Desktop address") {
            if sessionAddress.isEmpty {
                Button {
                    sessionAddress = UIPasteboard.general.string ?? ""
                } label: {
                    Label("Paste desktop address", systemImage: "doc.plaintext")
                }
                .disabled(!UIPasteboard.general.hasStrings)
            } else {
                HStack {
                    Text(sessionAddress).lineLimit(1)
                    Spacer()
                    Image(systemName: "multiply.circle.fill")
                        .foregroundColor(.secondary)
                        .onTapGesture { sessionAddress = "" }
                }
            }
            Button {
                connectDesktopAddress(sessionAddress)
            } label: {
                Label("Connect to desktop", systemImage: "rectangle.connected.to.line.below")
            }
            .disabled(sessionAddress.isEmpty)
        }
    }

    private func linkedDesktopsView() -> some View {
        List {
            Section("Desktop devices") {
                ForEach(remoteCtrls, id: \.remoteCtrlId) { rc in
                    remoteCtrlView(rc)
                }
                .onDelete { indexSet in
                    if let i = indexSet.first, i < remoteCtrls.count {
                        alert = .unlinkDesktop(rc: remoteCtrls[i])
                    }
                }
            }

            Section("Linked desktop options") {
                Toggle("Verify connections", isOn: $confirmRemoteSessions)
                Toggle("Discover via local network", isOn: $connectRemoteViaMulticast)
                if connectRemoteViaMulticast {
                    Toggle("Connect automatically", isOn: $connectRemoteViaMulticastAuto)
                }
            }
        }
        .navigationTitle("Linked desktops")
    }

    private func remoteCtrlView(_ rc: RemoteCtrlInfo) -> some View {
        Text(rc.deviceViewName)
    }


    private func setDeviceName(_ name: String) {
        do {
            try setLocalDeviceName(deviceName)
        } catch let e {
            errorAlert(e)
        }
    }

    private func updateRemoteCtrls() {
        do {
            remoteCtrls = try listRemoteCtrls()
        } catch let e {
            errorAlert(e)
        }
    }

    private func processDesktopQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r): connectDesktopAddress(r.string)
        case let .failure(e): errorAlert(e)
        }
    }

    private func findKnownDesktop() {
        Task {
            do {
                try await findKnownRemoteCtrl()
                await MainActor.run {
                    m.remoteCtrlSession = RemoteCtrlSession(
                        ctrlAppInfo: nil,
                        appVersion: "",
                        sessionState: .searching
                    )
                    showConnectScreen = true
                }
            } catch let e {
                await MainActor.run {
                    errorAlert(e)
                }
            }
        }
    }

    private func confirmKnownDesktop(_ rc: RemoteCtrlInfo) {
        connectDesktop_ {
            try await confirmRemoteCtrl(rc.remoteCtrlId)
        }
    }

    private func connectDesktopAddress(_ addr: String) {
        connectDesktop_ {
            try await connectRemoteCtrl(desktopAddress: addr)
        }
    }

    private func connectDesktop_(_ connect: @escaping () async throws -> (RemoteCtrlInfo?, CtrlAppInfo, String)) {
        Task {
            do {
                let (rc_, ctrlAppInfo, v) = try await connect()
                await MainActor.run {
                    sessionAddress = ""
                    m.remoteCtrlSession = RemoteCtrlSession(
                        ctrlAppInfo: ctrlAppInfo,
                        appVersion: v,
                        sessionState: .connecting(remoteCtrl_: rc_)
                    )
                }
            } catch let e {
                await MainActor.run {
                    switch e as? ChatResponse {
                    case .chatCmdError(_, .errorRemoteCtrl(.badInvitation)): alert = .badInvitationError
                    case .chatCmdError(_, .error(.commandError)): alert = .badInvitationError
                    case let .chatCmdError(_, .errorRemoteCtrl(.badVersion(v))): alert = .badVersionError(version: v)
                    case .chatCmdError(_, .errorAgent(.RCP(.version))): alert = .badVersionError(version: nil)
                    case .chatCmdError(_, .errorAgent(.RCP(.ctrlAuth))): alert = .desktopDisconnectedError
                    default: errorAlert(e)
                    }
                }
            }
        }
    }

    private func verifyDesktopSessionCode(_ sessCode: String) {
        Task {
            do {
                let rc = try await verifyRemoteCtrlSession(sessCode)
                await MainActor.run {
                    m.remoteCtrlSession = m.remoteCtrlSession?.updateState(.connected(remoteCtrl: rc, sessionCode: sessCode))
                }
                await MainActor.run {
                    updateRemoteCtrls()
                }
            } catch let error {
                await MainActor.run {
                    errorAlert(error)
                }
            }
        }
    }

    private func disconnectButton(_ label: LocalizedStringKey = "Disconnect") -> some View {
        Button {
            disconnectDesktop(.dismiss)
        } label: {
            Label(label, systemImage: "multiply")
        }
    }

    private func disconnectDesktop(_ action: UserDisconnectAction? = nil) {
        Task {
            do {
                try await stopRemoteCtrl()
                await MainActor.run {
                    if case .connected = m.remoteCtrlSession?.sessionState {
                        switchToLocalSession()
                    } else {
                        m.remoteCtrlSession = nil
                    }
                    switch action {
                    case .back: dismiss()
                    case .dismiss: dismiss()
                    case .none: ()
                    }
                }
            } catch let e {
                await MainActor.run {
                    errorAlert(e)
                }
            }
        }
    }

    private func unlinkDesktop(_ rc: RemoteCtrlInfo) {
        Task {
            do {
                try await deleteRemoteCtrl(rc.remoteCtrlId)
                await MainActor.run {
                    remoteCtrls.removeAll(where: { $0.remoteCtrlId == rc.remoteCtrlId })
                }
            } catch let e {
                await MainActor.run {
                    errorAlert(e)
                }
            }
        }
    }

    private func errorAlert(_ error: Error) {
        let a = getErrorAlert(error, "Error")
        alert = .error(title: a.title, error: a.message)
    }
}

#Preview {
    ConnectDesktopView()
}
