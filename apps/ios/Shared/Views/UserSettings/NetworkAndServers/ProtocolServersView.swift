//
//  ProtocolServersView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let howToUrl = URL(string: "https://simplex.chat/docs/server.html")!

enum ServerAlert: Identifiable {
    case testsFailed(failures: [String: ProtocolTestFailure])
    case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

    var id: String {
        switch self {
        case .testsFailed: return "testsFailed"
        case let .error(title, _): return "error \(title)"
        }
    }
}

struct YourServersView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject private var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.editMode) private var editMode
    @State private var currSMPServers: [UserServer] = []
    @State private var smpServers: [UserServer] = []
    @State private var currXFTPServers: [UserServer] = []
    @State private var xftpServers: [UserServer] = []
    @State private var selectedServer: String? = nil
    @State private var showAddServer = false
    @State private var showScanProtoServer = false
    @State private var justOpened = true
    @State private var testing = false
    @State private var alert: ServerAlert? = nil
    @State private var showSaveDialog = false

    let smpProto: String = ServerProtocol.smp.rawValue.uppercased()
    let xftpProto: String = ServerProtocol.xftp.rawValue.uppercased()

    var body: some View {
        ZStack {
            protocolServersView()
            if testing {
                ProgressView().scaleEffect(2)
            }
        }
    }

    private func protocolServersView() -> some View {
        List {
            if !smpServers.isEmpty {
                Section {
                    ForEach($smpServers) { srv in
                        ProtocolServerViewLink(
                            server: srv,
                            serverProtocol: .smp,
                            preset: false,
                            backLabel: "Your servers",
                            selectedServer: $selectedServer
                        )
                    }
                    .onMove { indexSet, offset in
                        smpServers.move(fromOffsets: indexSet, toOffset: offset)
                    }
                    .onDelete { indexSet in
                        smpServers.remove(atOffsets: indexSet)
                    }
                } header: {
                    Text("\(smpProto) servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    Text("The servers for new connections of your current chat profile **\(m.currentUser?.displayName ?? "")**.")
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(10)
                }
            }

            if !xftpServers.isEmpty {
                Section {
                    ForEach($xftpServers) { srv in
                        ProtocolServerViewLink(
                            server: srv,
                            serverProtocol: .xftp,
                            preset: false,
                            backLabel: "Your servers",
                            selectedServer: $selectedServer
                        )
                    }
                    .onMove { indexSet, offset in
                        xftpServers.move(fromOffsets: indexSet, toOffset: offset)
                    }
                    .onDelete { indexSet in
                        xftpServers.remove(atOffsets: indexSet)
                    }
                } header: {
                    Text("\(xftpProto) servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    Text("The servers for new files of your current chat profile **\(m.currentUser?.displayName ?? "")**.")
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(10)
                }
            }

            Section {
                Button("Add server") {
                    showAddServer = true
                }
            }

            Section {
                Button("Reset") {
                    smpServers = currSMPServers
                    xftpServers = currXFTPServers
                }
                .disabled((smpServers == currSMPServers && xftpServers == currXFTPServers) || testing)
                TestServersButton(
                    smpServers: $smpServers,
                    xftpServers: $xftpServers,
                    testing: $testing,
                    alert: $alert
                )
                Button("Save servers", action: saveServers)
                    .disabled(saveDisabled)
                howToButton()
            }
        }
        .toolbar { EditButton() }
        .confirmationDialog("Add server", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Enter SMP server manually") {
                smpServers.append(UserServer.empty)
                selectedServer = smpServers.last?.id
            }
            Button("Enter XFTP server manually") {
                xftpServers.append(UserServer.empty)
                selectedServer = xftpServers.last?.id
            }
            Button("Scan server QR code") { showScanProtoServer = true }
        }
        .sheet(isPresented: $showScanProtoServer) {
            ScanProtocolServer(smpServers: $smpServers, xftpServers: $xftpServers)
                .modifier(ThemedBackground(grouped: true))
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            if saveDisabled {
                dismiss()
                justOpened = false
            } else {
                showSaveDialog = true
            }
        })
        .confirmationDialog("Save servers?", isPresented: $showSaveDialog, titleVisibility: .visible) {
            Button("Save") {
                saveServers()
                dismiss()
                justOpened = false
            }
            Button("Exit without saving") { dismiss() }
        }
        .alert(item: $alert) { a in
            switch a {
            case let .testsFailed(fs):
                let msg = fs.map { (srv, f) in
                    "\(srv): \(f.localizedDescription)"
                }.joined(separator: "\n")
                return Alert(
                    title: Text("Tests failed!"),
                    message: Text("Some servers failed the test:\n" + msg)
                )
            case .error:
                return Alert(
                    title: Text("Error")
                )
            }
        }
        .onAppear {
            // this condition is needed to prevent re-setting the servers when exiting single server view
            if justOpened {
                do {
                    // TODO single getUserServers api call for both SMP and XFTP;
                    //      api call would be made in parent view, with servers returned per operator,
                    //      this view will take servers of "null operator" (no operator)
                    let rSMP = try getUserProtoServers(.smp)
                    currSMPServers = rSMP.protoServers
                    smpServers = currSMPServers

                    let rXFTP = try getUserProtoServers(.xftp)
                    currXFTPServers = rXFTP.protoServers
                    xftpServers = currXFTPServers
                } catch let error {
                    alert = .error(
                        title: "Error loading servers",
                        error: "Error: \(responseError(error))"
                    )
                }
                justOpened = false
            }
        }
    }

    private var saveDisabled: Bool {
        (smpServers == currSMPServers && xftpServers == currXFTPServers) ||
        testing
    }

    func howToButton() -> some View {
        Button {
            DispatchQueue.main.async {
                UIApplication.shared.open(howToUrl)
            }
        } label: {
            HStack {
                Text("How to use your servers")
                Image(systemName: "arrow.up.right.circle")
            }
        }
    }

    func saveServers() {
        Task {
            do {
                // TODO Single api call - setUserServers
                try await setUserProtoServers(.smp, servers: smpServers)
                try await setUserProtoServers(.xftp, servers: xftpServers)
                await MainActor.run {
                    currSMPServers = smpServers
                    currXFTPServers = xftpServers
                    editMode?.wrappedValue = .inactive
                }
            } catch let error {
                let err = responseError(error)
                logger.error("saveServers setUserProtocolServers error: \(err)")
                await MainActor.run {
                    alert = .error(
                        title: "Error saving servers",
                        error: "Make sure server addresses are in correct format, line separated and are not duplicated (\(responseError(error)))."
                    )
                }
            }
        }
    }
}

struct ProtocolServerViewLink: View {
    @EnvironmentObject var theme: AppTheme
    @Binding var server: UserServer
    var serverProtocol: ServerProtocol
    var preset: Bool
    var backLabel: LocalizedStringKey
    @Binding var selectedServer: String?

    var body: some View {
        let proto = serverProtocol.rawValue.uppercased()

        NavigationLink(tag: server.id, selection: $selectedServer) {
            ProtocolServerView(
                serverProtocol: serverProtocol,
                server: $server,
                serverToEdit: server,
                preset: preset,
                backLabel: backLabel
            )
            .navigationBarTitle("\(proto) server")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            let address = parseServerAddress(server.server)
            HStack {
                Group {
                    if let address = address {
                        if !address.valid || address.serverProtocol != serverProtocol {
                            invalidServer()
                            // TODO Show based on validateServers
                            // } else if !uniqueAddress(srv, address) {
                            //     Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        } else if !server.enabled {
                            Image(systemName: "slash.circle").foregroundColor(theme.colors.secondary)
                        } else {
                            showTestStatus(server: server)
                        }
                    } else {
                        invalidServer()
                    }
                }
                .frame(width: 16, alignment: .center)
                .padding(.trailing, 4)

                let v = Text(address?.hostnames.first ?? server.server).lineLimit(1)
                if server.enabled {
                    v
                } else {
                    v.foregroundColor(theme.colors.secondary)
                }
            }
        }
    }

    private func invalidServer() -> some View {
        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
    }
}

struct TestServersButton: View {
    @Binding var smpServers: [UserServer]
    @Binding var xftpServers: [UserServer]
    @Binding var testing: Bool
    @Binding var alert: ServerAlert?

    var body: some View {
        Button("Test servers", action: testServers)
            .disabled(testing || allServersDisabled)
    }

    private var allServersDisabled: Bool {
        smpServers.allSatisfy { !$0.enabled } && xftpServers.allSatisfy { !$0.enabled }
    }

    private func testServers() {
        resetTestStatus()
        testing = true
        Task {
            let fs = await runServersTest()
            await MainActor.run {
                testing = false
                if !fs.isEmpty {
                    alert = .testsFailed(failures: fs)
                }
            }
        }
    }

    private func resetTestStatus() {
        for i in 0..<smpServers.count {
            if smpServers[i].enabled {
                smpServers[i].tested = nil
            }
        }

        for i in 0..<xftpServers.count {
            if xftpServers[i].enabled {
                xftpServers[i].tested = nil
            }
        }
    }

    private func runServersTest() async -> [String: ProtocolTestFailure] {
        var fs: [String: ProtocolTestFailure] = [:]
        for i in 0..<smpServers.count {
            if smpServers[i].enabled {
                if let f = await testServerConnection(server: $smpServers[i]) {
                    fs[serverHostname(smpServers[i].server)] = f
                }
            }
        }

        for i in 0..<xftpServers.count {
            if xftpServers[i].enabled {
                if let f = await testServerConnection(server: $xftpServers[i]) {
                    fs[serverHostname(xftpServers[i].server)] = f
                }
            }
        }
        return fs
    }
}

struct YourServersView_Previews: PreviewProvider {
    static var previews: some View {
        YourServersView()
    }
}
