import SwiftUI
import SimpleXChat
import Ink

private let MAX_MARKDOWN_VIEW_SIZE: Int64 = 1_048_576
private let MAX_MARKDOWN_LINE_LENGTH = 15_000
private let MAX_MARKDOWN_NESTING = 16
private let MAX_MARKDOWN_LINK_NESTING = 4
private let MAX_MARKDOWN_TABLE_ROWS = 200
private let MAX_MARKDOWN_IMAGE_DIMENSION: CGFloat = 4320
private let MAX_MARKDOWN_IMAGE_PIXELS: CGFloat = 2160 * 4320

func isMarkdownFile(_ file: CIFile) -> Bool {
    let ext = (file.fileName as NSString).pathExtension.lowercased()
    return file.fileSize <= MAX_MARKDOWN_VIEW_SIZE && (ext == "md" || ext == "markdown")
}

func showMarkdownFile(_ file: CIFile) {
    guard let fileSource = getLoadedFileSource(file) else { return }
    Task {
        do {
            let data = try getFileData(getAppFilePath(fileSource.filePath), fileSource.cryptoArgs)
            let text = decodedText(data)
            let lines = text.components(separatedBy: "\n")
            let html = withinMarkdownLimits(lines)
                ? MarkdownParser(modifiers: [Modifier(target: .html) { escapedHTML($0.markdown) }, imagesWithinBudget()]).html(from: text)
                : nil
            let shownLines = lines.flatMap(chunkedLine)
            let hasBidiControls = text.unicodeScalars.contains(where: isBidiControl)
            await MainActor.run {
                showAppSheet {
                    MarkdownFileView(file: file, fileSource: fileSource, lines: shownLines, html: html, hasBidiControls: hasBidiControls)
                }
            }
        } catch {
            await MainActor.run {
                showAlert(NSLocalizedString("Error reading file", comment: "alert title"), message: error.localizedDescription)
            }
        }
    }
}

private func decodedText(_ data: Data) -> String {
    var text = String.UnicodeScalarView()
    var parser = Unicode.UTF8.ForwardParser()
    var bytes = data.makeIterator()
    while true {
        switch parser.parseScalar(from: &bytes) {
        case let .valid(encoded): text.append(replacingControlChar(Unicode.UTF8.decode(encoded)))
        case let .error(length): text.append(contentsOf: repeatElement("?" as Unicode.Scalar, count: length))
        case .emptyInput: return String(text).replacingOccurrences(of: "\r\n", with: "\n").replacingOccurrences(of: "\r", with: "\n")
        }
    }
}

private func replacingControlChar(_ s: Unicode.Scalar) -> Unicode.Scalar {
    s.value < 0x20 && s != "\t" && s != "\n" && s != "\r" ? "?" : s
}

private func withinMarkdownLimits(_ lines: [String]) -> Bool {
    var tableRows = 0
    var linkNesting = 0
    for line in lines {
        tableRows = line.contains("|") ? tableRows + 1 : 0
        if line.count > MAX_MARKDOWN_LINE_LENGTH || nestingLevel(line) > MAX_MARKDOWN_NESTING || tableRows > MAX_MARKDOWN_TABLE_ROWS || !linkNestingWithinLimit(line, &linkNesting) {
            return false
        }
    }
    return true
}

private func linkNestingWithinLimit(_ line: String, _ nesting: inout Int) -> Bool {
    if line.isEmpty { nesting = 0 }
    for c in line {
        if c == "[" {
            nesting += 1
            if nesting > MAX_MARKDOWN_LINK_NESTING { return false }
        } else if c == "]" && nesting > 0 {
            nesting -= 1
        }
    }
    return true
}

private func nestingLevel(_ line: String) -> Int {
    let content = line.drop(while: { $0 == " " || $0 == "\t" || $0 == ">" })
    let prefix = line[..<content.startIndex]
    let quotes = prefix.filter { $0 == ">" }.count
    let listItem = content.first.map { "-*+".contains($0) || $0.isNumber } ?? false
    return quotes + (listItem ? (prefix.count - quotes) / 2 : 0)
}

private func chunkedLine(_ line: String) -> [String] {
    let chars = Array(line)
    return stride(from: 0, to: max(chars.count, 1), by: MAX_MARKDOWN_LINE_LENGTH).map {
        String(chars[$0 ..< min($0 + MAX_MARKDOWN_LINE_LENGTH, chars.count)])
    }
}

private func escapedHTML(_ s: Substring) -> String {
    s.replacingOccurrences(of: "&", with: "&amp;")
        .replacingOccurrences(of: "<", with: "&lt;")
        .replacingOccurrences(of: ">", with: "&gt;")
}

private func imagesWithinBudget() -> Modifier {
    var pixels = MAX_MARKDOWN_IMAGE_PIXELS
    return Modifier(target: .images) { image in
        let src = image.html.dropFirst("<img src=\"".count).prefix(while: { $0 != "\"" })
        guard src.hasPrefix("data:image/") else { return escapedHTML(image.markdown) }
        let parts = src.split(separator: ",", maxSplits: 1)
        guard parts.count == 2,
              parts[0].hasSuffix(";base64"),
              let data = Data(base64Encoded: String(parts[1])),
              ImageFileType.jpeg.matches(data) || ImageFileType.png.matches(data),
              let size = UIImage(data: data)?.size,
              size.width <= MAX_MARKDOWN_IMAGE_DIMENSION,
              size.height <= MAX_MARKDOWN_IMAGE_DIMENSION,
              size.width * size.height <= pixels
        else { return "" }
        pixels -= size.width * size.height
        return "<img src=\"\(src)\"/>"
    }
}

private func isBidiControl(_ s: Unicode.Scalar) -> Bool {
    [0x061C, 0x200E, 0x200F].contains(s.value) || (0x202A...0x202E).contains(s.value) || (0x2066...0x2069).contains(s.value)
}

private func withInvisibleCharMarkers(_ line: String) -> Text {
    var text = Text(verbatim: "")
    var plain = ""
    for s in line.unicodeScalars {
        if s.properties.generalCategory == .format {
            text = text + Text(verbatim: plain) + Text(verbatim: unicodeMarker(s)).foregroundColor(.orange)
            plain = ""
        } else {
            plain.unicodeScalars.append(s)
        }
    }
    return text + Text(verbatim: plain)
}

private func openMarkdownFileLink(_ url: URL) {
    let uri = url.absoluteString
    let parsed = parseSimpleXMarkdown(uri)
    let format = parsed?.count == 1 ? parsed?.first?.format : nil
    switch format {
    case let .simplexLink(_, _, simplexUri, _)?:
        if let simplexUrl = URL(string: simplexUri) {
            ChatModel.shared.appOpenUrl = simplexUrl
        } else {
            showInvalidLinkAlert(uri)
        }
    case .uri? where uri.hasPrefix("https://") || uri.hasPrefix("http://"):
        openBrowserAlert(uri: uri)
    default:
        showInvalidLinkAlert(uri)
    }
}

struct MarkdownFileView: View {
    @AppStorage(DEFAULT_MARKDOWN_FILE_TEXT_VIEW) private var textView = false
    let file: CIFile
    let fileSource: CryptoFile
    let lines: [String]
    let html: String?
    let hasBidiControls: Bool

    var body: some View {
        NavigationView {
            VStack(spacing: 0) {
                if hasBidiControls {
                    Text("This file has text direction characters. The text can be shown in a different order than it is written.")
                        .foregroundColor(.orange)
                        .padding()
                }
                if let html, !textView {
                    ConditionsWebView(html: html, openLink: openMarkdownFileLink)
                        .padding(.horizontal)
                } else {
                    ScrollView {
                        LazyVStack(alignment: .leading, spacing: 0) {
                            ForEach(lines.indices, id: \.self) { i in
                                withInvisibleCharMarkers(lines[i])
                                    .font(.system(.body, design: .monospaced))
                                    .textSelection(.enabled)
                            }
                        }
                        .padding()
                    }
                }
            }
            .modifier(ThemedBackground())
            .navigationTitle(file.fileName)
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItemGroup(placement: .navigationBarTrailing) {
                    if html != nil {
                        Button {
                            textView.toggle()
                        } label: {
                            Image(systemName: textView ? "doc.richtext" : "chevron.left.forwardslash.chevron.right")
                        }
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
