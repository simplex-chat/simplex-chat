import SwiftUI
import SimpleXChat
import Ink

func isMarkdownFile(_ file: CIFile) -> Bool {
    let ext = (file.fileName as NSString).pathExtension.lowercased()
    return file.fileSize <= MAX_MARKDOWN_VIEW_SIZE && (ext == "md" || ext == "markdown")
}

func showMarkdownFile(_ file: CIFile) {
    guard let fileSource = getLoadedFileSource(file) else { return }
    Task {
        do {
            let data = try getFileData(getAppFilePath(fileSource.filePath), fileSource.cryptoArgs)
            guard let text = textFileContent(data) else {
                await MainActor.run {
                    showAlert(
                        title: NSLocalizedString("Not a text file", comment: "alert title"),
                        buttonTitle: NSLocalizedString("Save", comment: "alert button"),
                        buttonAction: { saveCryptoFile(fileSource) },
                        cancelButton: true
                    )
                }
                return
            }
            let lines = text.components(separatedBy: "\n")
            let html = MarkdownParser().html(from: text)
            await MainActor.run {
                showAppSheet(fullScreen: true) {
                    MarkdownFileView(file: file, fileSource: fileSource, lines: lines, html: html)
                }
            }
        } catch {
            await MainActor.run {
                showAlert(NSLocalizedString("Error reading file", comment: "alert title"), message: error.localizedDescription)
            }
        }
    }
}

private func textFileContent(_ data: Data) -> String? {
    guard let text = String(data: data, encoding: .utf8),
          !text.unicodeScalars.contains(where: { $0.value < 0x20 && $0 != "\t" && $0 != "\n" && $0 != "\r" })
    else { return nil }
    return text
}

struct MarkdownFileView: View {
    @AppStorage(DEFAULT_MARKDOWN_FILE_TEXT_VIEW) private var textView = false
    let file: CIFile
    let fileSource: CryptoFile
    let lines: [String]
    let html: String

    var body: some View {
        NavigationView {
            Group {
                if textView {
                    ScrollView {
                        LazyVStack(alignment: .leading, spacing: 0) {
                            ForEach(lines.indices, id: \.self) { i in
                                Text(lines[i])
                                    .font(.system(.body, design: .monospaced))
                                    .textSelection(.enabled)
                            }
                        }
                        .padding()
                    }
                } else {
                    ConditionsWebView(html: html)
                        .padding(.horizontal)
                }
            }
            .modifier(ThemedBackground())
            .navigationTitle(file.fileName)
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    Button {
                        getTopViewController()?.dismiss(animated: true)
                    } label: {
                        Image(systemName: "xmark")
                    }
                }
                ToolbarItemGroup(placement: .navigationBarTrailing) {
                    Button {
                        textView.toggle()
                    } label: {
                        Image(systemName: textView ? "doc.richtext" : "chevron.left.forwardslash.chevron.right")
                    }
                    Button {
                        saveCryptoFile(fileSource)
                    } label: {
                        Image(systemName: "square.and.arrow.up")
                    }
                }
            }
        }
        .navigationViewStyle(.stack)
    }
}
