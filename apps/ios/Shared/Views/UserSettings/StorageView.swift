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
    @State var appGroupFiles: [String: Int64] = [:]
    @State var documentsFiles: [String: Int64] = [:]

    var body: some View {
        ScrollView {
            VStack(alignment: .leading) {
                directoryView("App group:", appGroupFiles)
                if !documentsFiles.isEmpty {
                    directoryView("Documents:", documentsFiles)
                }
            }
        }
        .padding()
        .onAppear {
            appGroupFiles = traverseFiles(in: getGroupContainerDirectory())
            documentsFiles = traverseFiles(in: getDocumentsDirectory())
        }
    }

    @ViewBuilder
    private func directoryView(_ name: LocalizedStringKey, _ contents: [String: Int64]) -> some View {
        Text(name).font(.headline)
        ForEach(Array(contents), id: \.key) { (key, value) in
            Text(key).bold() + Text(verbatim: "   ") + Text((ByteCountFormatter.string(fromByteCount: value, countStyle: .binary)))
        }
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
