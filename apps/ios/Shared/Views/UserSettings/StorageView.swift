//
//  StorageView.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 13.01.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct StorageView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State var appGroupFiles: [String: Int64] = [:]
    @State var documentsFiles: [String: Int64] = [:]

    var body: some View {
        ScrollView {
            VStack(alignment: .leading) {
                directoryView("App group:", getGroupContainerDirectory(), appGroupFiles)
                if !documentsFiles.isEmpty {
                    directoryView("Documents:", getDocumentsDirectory(), documentsFiles)
                }
            }
        }
        .padding()
        .onAppear(perform: loadFiles)
    }

    private func loadFiles() {
        appGroupFiles = traverseFiles(in: getGroupContainerDirectory())
        documentsFiles = traverseFiles(in: getDocumentsDirectory())
    }

    @ViewBuilder
    private func directoryView(_ name: LocalizedStringKey, _ dir: URL, _ contents: [String: Int64]) -> some View {
        Text(name).font(.headline)
        ForEach(Array(contents), id: \.key) { (key, value) in
            let sizeText = Text(key).bold() + Text(verbatim: "   ") + Text((ByteCountFormatter.string(fromByteCount: value, countStyle: .binary)))
            if dir.appendingPathComponent(key).path == getTempFilesDirectory().path {
                let stopped = m.chatRunning == false
                HStack {
                    sizeText
                    Button("Delete temp data", role: .destructive, action: confirmDeleteTempFiles)
                        .disabled(!stopped)
                }
                if !stopped {
                    Text("Stop chat in Database settings to delete temp data.")
                        .font(.footnote)
                        .foregroundColor(theme.colors.secondary)
                }
            } else {
                sizeText
            }
        }
    }

    private func confirmDeleteTempFiles() {
        AlertManager.shared.showAlert(Alert(
            title: Text("Delete temp data?"),
            message: Text("File transfers in progress will fail."),
            primaryButton: .destructive(Text("Delete")) {
                deleteTempFiles()
                loadFiles()
            },
            secondaryButton: .cancel()
        ))
    }

    private func traverseFiles(in dir: URL) -> [String: Int64] {
        var res: [String: Int64] = [:]
        let fm = FileManager.default
        do {
            if let enumerator = fm.enumerator(at: dir, includingPropertiesForKeys: [.isDirectoryKey, .fileSizeKey, .fileAllocatedSizeKey]) {
                for case let url as URL in enumerator {
                    let attrs = try url.resourceValues(forKeys: [/*.isDirectoryKey, .fileSizeKey,*/ .fileAllocatedSizeKey])
                    let root = String(url.absoluteString.replacingOccurrences(of: dir.absoluteString, with: "").split(separator: "/")[0])
                    res[root] = (res[root] ?? 0) + Int64(attrs.fileAllocatedSize ?? 0)
                }
            }
        } catch {
            logger.error("Error traversing files: \(error)")
        }
        return res
    }
}
