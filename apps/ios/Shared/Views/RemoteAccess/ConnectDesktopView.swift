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
    @AppStorage(DEFAULT_DEVICE_NAME_FOR_REMOTE_ACCESS) private var deviceName = UIDevice.current.name
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var sessionAddress: String = ""

    var body: some View {
        switch m.remoteCtrlSession {
        case .none: connectDesktopView()
        case .starting: Text("Starting...")
        case .connecting: Text("Connecting...")
        case let .pendingConfirmation(rc_, sessCode): verifySessionView(rc_, sessCode)
        case let .connected(rc): activeSessionView(rc)
        }
    }

    func connectDesktopView() -> some View {
        List {
            Section("This device name") {
                TextField("Enter this device name…", text: $deviceName)
            }

            Section("Scan QR code from desktop") {
                CodeScannerView(codeTypes: [.qr], completion: processDesktopQRCode)
                    .aspectRatio(1, contentMode: .fit)
                    .cornerRadius(12)
                    .listRowBackground(Color.clear)
                    .listRowSeparator(.hidden)
                    .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                    .padding(.horizontal)
            }

            if developerTools {
                Section("Desktop address") {
                    if sessionAddress.isEmpty {
                        Group {
                            if #available(iOS 16.0, *) {
                                PasteButton(payloadType: String.self) { strings in
                                    sessionAddress = strings.first ?? ""
                                }
                            } else {
                                Button {
                                    sessionAddress = UIPasteboard.general.string ?? ""
                                } label: {
                                    Label("Paste desktop address", systemImage: "doc.plaintext")
                                }
                            }
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
        }
        .onAppear {
            setDeviceName(deviceName)
        }
        .onChange(of: deviceName) {
            setDeviceName($0)
        }
        .navigationTitle("Connect to desktop")
    }

    private func verifySessionView(_ rc: RemoteCtrlInfo?, _ sessCode: String) -> some View {
        List {
            Section("Connected desktop") {
                if let rc = rc {
                    Text(rc.ctrlName)
                } else {
                    Text("New device").italic()
                }
            }

            Section("Confirm code with desktop") {
                Text(sessCode)
                Button {
                    verifyDesktopSessionCode(sessCode)
                } label: {
                    Label("Confirm", systemImage: "checkmark")
                }
            }
        }
        .navigationTitle("Verify connection")
    }

    private func activeSessionView(_ rc: RemoteCtrlInfo) -> some View {
        List {
            Section {
                Text(rc.ctrlName)
                Button {
                    disconnectDesktop()
                } label: {
                    Label("Disconnect", systemImage: "multiply")
                }
            } header: {
                Text("Connected desktop")
            } footer: {
                Text("Please keep this screen open to use the app from desktop")
            }
        }
        .navigationTitle("Connected")
    }

    private func setDeviceName(_ name: String) {
        do {
            try setLocalDeviceName(deviceName)
        } catch let error {
            logger.error("setLocalDeviceName \(responseError(error))")
        }
    }

    private func processDesktopQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r): connectDesktopAddress(r.string)
        case let .failure(e):
            logger.error("processDesktopQRCode QR code error: \(e.localizedDescription)")
        }
    }

    private func connectDesktopAddress(_ addr: String) {
        Task {
            do {
                try await connectRemoteCtrl(desktopAddress: addr)
                await MainActor.run {
                    m.remoteCtrlSession = .starting
                }
            } catch let error {
                logger.error("connectRemoteCtrl \(responseError(error))")
            }
        }
    }

    private func verifyDesktopSessionCode(_ sessCode: String) {
        Task {
            do {
                let rc = try await verifyRemoteCtrlSession(sessCode)
                await MainActor.run {
                    m.remoteCtrlSession = .connected(remoteCtrl: rc)
                }
            } catch let error {
                logger.error("verifyRemoteCtrlSession \(responseError(error))")
            }
        }
    }

    private func disconnectDesktop() {
        Task {
            do {
                try await stopRemoteCtrl()
                await MainActor.run {
                    m.remoteCtrlSession = nil
                }
            } catch let error {
                logger.error("verifyRemoteCtrlSession \(responseError(error))")
            }
        }
    }
}

#Preview {
    ConnectDesktopView()
}
