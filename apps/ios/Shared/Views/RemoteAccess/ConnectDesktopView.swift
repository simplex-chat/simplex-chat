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
    @AppStorage(DEFAULT_DEVICE_NAME_FOR_REMOTE_ACCESS) private var deviceName = UIDevice.current.name
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var sessionAddress: String = ""
    @State private var remoteCtrls: [RemoteCtrlInfo] = []
    @State private var alert: ConnectDesktopAlert?

    private enum ConnectDesktopAlert: Identifiable {
        case deletePairedDesktop(rc: RemoteCtrlInfo)
        case disconnectDesktop(action: UserDisconnectAction)
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case let .deletePairedDesktop(rc): "deleteRemoteCtrl \(rc.remoteCtrlId)"
            case let .disconnectDesktop(action): "disconnectDecktop \(action)"
            case let .error(title, _): "error \(title)"
            }
        }
    }

    private enum UserDisconnectAction: String {
        case back
        case dismiss // TODO dismiss settings after confirmation
    }

    var body: some View {
        Group {
            if let session = m.remoteCtrlSession {
                switch session.sessionState {
                case .starting: connectingDesktopView(session, nil)
                case let .connecting(rc_): connectingDesktopView(session, rc_)
                case let .pendingConfirmation(rc_, sessCode): verifySessionView(session, rc_, sessCode)
                case let .connected(rc, _): activeSessionView(session, rc)
                }
            } else {
                connectDesktopView()
            }
        }
        .onAppear {
            setDeviceName(deviceName)
            Task {
                do {
                    let rcs = try await listRemoteCtrls()
                    if !rcs.isEmpty {
                        await MainActor.run {
                            remoteCtrls = rcs
                        }
                    }
                } catch let e {
                    errorAlert(e)
                }
            }
        }
        .onDisappear {
            if m.activeRemoteCtrl {
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
            case let .deletePairedDesktop(rc):
                Alert(
                    title: Text("Forget linked desktop?"),
                    primaryButton: .destructive(Text("Delete")) {
                        deleteDesktop(rc)
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
            case let .error(title, error):
                Alert(title: Text(title), message: Text(error))
            }
        }
        .modifier(BackButton(label: "Back") {
            if m.activeRemoteCtrl {
                alert = .disconnectDesktop(action: .back)
            } else {
                dismiss()
            }
        })
        .interactiveDismissDisabled(m.activeRemoteCtrl)
    }

    private func connectDesktopView() -> some View {
        List {
            Section("This device name") {
                devicesView()
            }
            scanDesctopAddressView()
            if developerTools {
                desktopAddressView()
            }
        }
        .navigationTitle("Connect to desktop")
    }

    private func connectingDesktopView(_ session: RemoteCtrlSession, _ rc_: RemoteCtrlInfo?) -> some View {
        List {
            Section("This device name") {
                devicesView()
                .disabled(true)
                disconnectButton()
            }

            scanDesctopAddressView()
            .disabled(true)

            if developerTools {
                desktopAddressView()
                .disabled(true)
            }
        }
        .navigationTitle("Connecting to desktop")
    }

    private func verifySessionView(_ session: RemoteCtrlSession, _ rc: RemoteCtrlInfo?, _ sessCode: String) -> some View {
        List {
            Section("Connected to desktop") {
                if let rc = rc {
                    Text(rc.deviceViewName)
                } else {
                    Text("New desktop device").italic()
                }
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

    private func activeSessionView(_ session: RemoteCtrlSession, _ rc: RemoteCtrlInfo) -> some View {
        List {
            Section("Connected desktop") {
                Text(rc.deviceViewName)
            }

            if let sessCode = session.sessionCode {
                Section("Session code") {
                    sessionCodeText(sessCode)
                }
            }

            Section {
                disconnectButton()
            } footer: {
                Text("Please keep this screen open to use the app from desktop")
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
                    pairedDesktopsView()
                } label: {
                    Text("Linked desktops")
                }
            }
        }
    }

    private func scanDesctopAddressView() -> some View {
        Section("Scan QR code from desktop") {
            CodeScannerView(codeTypes: [.qr], completion: processDesktopQRCode)
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

    private func pairedDesktopsView() -> some View {
        List {
            Section("Swipe to forget") {
                ForEach(remoteCtrls, id: \.remoteCtrlId) { rc in
                    remoteCtrlView(rc)
                }
                .onDelete { indexSet in
                    if let i = indexSet.first {
                        alert = .deletePairedDesktop(rc: remoteCtrls[i])
                    }
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

    private func processDesktopQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r): connectDesktopAddress(r.string)
        case let .failure(e): errorAlert(e)
        }
    }

    private func connectDesktopAddress(_ addr: String) {
        Task {
            do {
                let (rc_, ctrlAppInfo, v) = try await connectRemoteCtrl(desktopAddress: addr)
                await MainActor.run {
                    sessionAddress = ""
                    m.remoteCtrlSession = RemoteCtrlSession(
                        ctrlAppInfo: ctrlAppInfo,
                        appVersion: v,
                        sessionState: .connecting(remoteCtrl_: rc_)
                    )
                }
            } catch let e {
                errorAlert(e)
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
            } catch let error {
                errorAlert(error)
            }
        }
    }

    private func disconnectButton() -> some View {
        Button {
            disconnectDesktop()
        } label: {
            Label("Disconnect", systemImage: "multiply")
        }
    }

    private func disconnectDesktop(_ action: UserDisconnectAction? = nil) {
        Task {
            do {
                try await stopRemoteCtrl()
                await MainActor.run {
                    m.remoteCtrlSession = nil
                    switch action {
                    case .back: dismiss()
                    case .dismiss: dismiss()
                    case .none: ()
                    }
                }
            } catch let e {
                errorAlert(e)
            }
        }
    }

    private func deleteDesktop(_ rc: RemoteCtrlInfo) {
        Task {
            do {
                try await deleteRemoteCtrl(rc.remoteCtrlId)
                await MainActor.run {
                    remoteCtrls.removeAll(where: { $0.remoteCtrlId == rc.remoteCtrlId })
                }
            } catch let e {
                errorAlert(e)
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
