//
//  DirectorySearchView.swift
//  SimpleX
//
//  Created by spaced4ndy on 13.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum DirectoryFooter {
    case retry, loadMore
}

@MainActor
class OnlineSearchModel: ObservableObject {
    @Published private(set) var directory: [DirectoryResult] = []
    @Published private(set) var moreCount: Int? = nil
    @Published private(set) var loading = false { didSet { updateSpinner() } }
    @Published private(set) var failed = false
    @Published private(set) var searched = false

    private var cursor: JSONValue? = nil
    private var searchedText = ""
    private var generation = 0
    private var spinnerShown = false

    var showResults: Bool { loading || searched }
    var footer: DirectoryFooter? { failed ? .retry : cursor != nil ? .loadMore : nil }

    func reset() {
        generation += 1
        directory = []
        moreCount = nil
        cursor = nil
        loading = false
        failed = false
        searched = false
        searchedText = ""
    }

    func cancelRequest() {
        generation += 1
        loading = false
    }

    func search(_ text: String) {
        let text = text.trimmingCharacters(in: .whitespaces)
        if !text.isEmpty {
            confirmDirectorySearch { Task { await self.searchDirectory(text) } }
        }
    }

    private func searchDirectory(_ text: String) async {
        guard !text.isEmpty, !(loading && text == searchedText) else { return }
        reset()
        searchedText = text
        await request()
    }

    func loadMore() {
        confirmDirectorySearch { Task { await self.requestNextPage() } }
    }

    private func requestNextPage() async {
        guard cursor != nil, !loading else { return }
        await request()
    }

    func retry() {
        confirmDirectorySearch { Task { await self.retryRequest() } }
    }

    private func retryRequest() async {
        if cursor != nil {
            await requestNextPage()
        } else {
            await searchDirectory(searchedText)
        }
    }

    func rows(shownChatIds: Set<ChatId>) -> OnlineSearchRows {
        var yourChats: [ChatId] = []
        var results: [DirectoryResult] = []
        for result in directory {
            switch result.state {
            case let .yourChat(chatId) where ChatModel.shared.getChat(chatId) != nil:
                if !shownChatIds.contains(chatId) && !yourChats.contains(chatId) {
                    yourChats.append(chatId)
                }
            case .yourChat:
                results.append(DirectoryResult(entry: result.entry, state: .open))
            case .joining, .open:
                results.append(result)
            }
        }
        return OnlineSearchRows(yourChats: yourChats, results: results, footer: footer)
    }

    func refreshStates() {
        Task {
            let gen = generation
            var states: [String: DirectoryResultState] = [:]
            for result in directory {
                if case .yourChat = result.state { continue }
                states[result.id] = await resultState(result.entry.link)
            }
            guard gen == generation else { return }
            directory = directory.map { result in
                var r = result
                r.state = states[r.id] ?? r.state
                return r
            }
        }
    }

    private func resultState(_ link: String) async -> DirectoryResultState {
        let plan = await apiConnectPlan(connLink: link, resolveMode: .never, inProgress: BoxedValue(false))
        return directoryResultState(plan?.connectionPlan)
    }

    private func request() async {
        let gen = generation
        loading = true
        failed = false
        let r = await apiSearchDirectory(searchedText, cursor: cursor)
        var page: [DirectoryResult] = []
        if let r {
            var seen = Set(directory.map { $0.id })
            for entry in r.entries {
                guard seen.insert(entry.id).inserted else { continue }
                page.append(DirectoryResult(entry: entry, state: await resultState(entry.link)))
            }
        }
        guard gen == generation else { return }
        loading = false
        searched = true
        guard let r else {
            failed = true
            return
        }
        cursor = r.cursor
        moreCount = r.moreCount
        directory += page
    }

    private func updateSpinner() {
        guard loading != spinnerShown else { return }
        spinnerShown = loading
        if spinnerShown {
            ConnectProgressManager.shared.startConnectProgress(
                NSLocalizedString("Searching directory…", comment: "in progress text"),
                owner: .directorySearch
            ) { [weak self] in
                Task { @MainActor in self?.cancelRequest() }
            }
        } else {
            ConnectProgressManager.shared.stopConnectProgress(.directorySearch)
        }
    }
}

struct SearchInDirectoryRow: View {
    @EnvironmentObject var theme: AppTheme
    @FocusState.Binding var searchFocussed: Bool
    var onSearch: () -> Void

    var body: some View {
        HStack(spacing: 4) {
            Image(systemName: "magnifyingglass")
                .foregroundColor(theme.colors.primary)
            Text("Search in Directory")
                .foregroundColor(theme.colors.primary)
            Spacer()
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
        .onTapGesture {
            searchFocussed = false
            onSearch()
        }
    }
}

struct DirectorySearchRow: View {
    var entry: DirectorySearchEntry
    var imageSize: CGFloat
    var state: DirectoryResultState
    var showsOpen = true

    var body: some View {
        DirectorySearchRowLayout(
            entryType: entry.entryType,
            name: entry.displayName,
            simplexName: entry.simplexName,
            shortDescr: entry.shortDescr,
            image: entry.image,
            imageSize: imageSize,
            state: state,
            showsOpen: showsOpen
        )
    }
}

private struct DirectorySearchRowLayout: View {
    @EnvironmentObject var theme: AppTheme
    var entryType: DirectoryEntryType
    var name: String
    var simplexName: String?
    var shortDescr: String?
    var image: String?
    var imageSize: CGFloat
    var state: DirectoryResultState
    var showsOpen: Bool

    var body: some View {
        HStack(spacing: 8) {
            ProfileImage(
                imageStr: image,
                icon: Image(systemName: isChannel ? "antenna.radiowaves.left.and.right.circle.fill" : "person.2.circle.fill"),
                size: imageSize
            )
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 4) {
                    Text(displayName).fontWeight(.bold).lineLimit(1)
                    if let simplexName {
                        Text(simplexName)
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(1)
                    }
                }
                if let descr = shortDescr, !descr.isEmpty {
                    Text(descr)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(2)
                }
                Text(joining ? NSLocalizedString("Already joining", comment: "directory search result") : membersText)
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
            Spacer()
            if showsOpen && !joining {
                Text("Open")
                    .foregroundColor(theme.colors.primary)
                    .lineLimit(1)
                    .fixedSize()
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
    }

    private var displayName: String { "#" + name }

    private var isChannel: Bool { entryType.groupType == .channel }

    private var joining: Bool {
        if case .joining = state { true } else { false }
    }

    private var membersText: String {
        let summary = entryType.summary
        let count = summary.publicMemberCount ?? summary.currentMembers
        return isChannel
            ? subscriberCountStr(count)
            : String.localizedStringWithFormat(NSLocalizedString("%d members", comment: "directory search result"), Int(count))
    }
}

struct DirectoryLoadMoreRow: View {
    @ObservedObject var onlineSearch: OnlineSearchModel

    var body: some View {
        DirectoryLoadMoreRowLayout(moreCount: onlineSearch.moreCount, loading: onlineSearch.loading) {
            onlineSearch.loadMore()
        }
    }
}

private struct DirectoryLoadMoreRowLayout: View {
    @EnvironmentObject var theme: AppTheme
    var moreCount: Int?
    var loading: Bool
    var onLoadMore: () -> Void

    var body: some View {
        Button {
            onLoadMore()
        } label: {
            HStack(spacing: 4) {
                Image(systemName: "plus.magnifyingglass")
                Text(loadMoreText)
            }
            .foregroundColor(loading ? theme.colors.secondary : theme.colors.primary)
        }
        .disabled(loading)
    }

    private var loadMoreText: String {
        if let n = moreCount, n > 0 {
            String.localizedStringWithFormat(NSLocalizedString("Load more · %d left", comment: "directory search results"), n)
        } else {
            NSLocalizedString("Load more", comment: "directory search results")
        }
    }
}

struct DirectoryRetryRow: View {
    var onlineSearch: OnlineSearchModel

    var body: some View {
        DirectoryRetryRowLayout {
            onlineSearch.retry()
        }
    }
}

private struct DirectoryRetryRowLayout: View {
    @EnvironmentObject var theme: AppTheme
    var onRetry: () -> Void

    var body: some View {
        Button {
            onRetry()
        } label: {
            HStack(spacing: 4) {
                Image(systemName: "arrow.clockwise")
                Text("Search failed, tap to retry")
            }
            .foregroundColor(theme.colors.primary)
        }
    }
}

struct DirectoryResultMenu: ViewModifier {
    @EnvironmentObject var theme: AppTheme
    var result: DirectoryResult
    var imageSize: CGFloat
    var onOpen: () -> Void

    func body(content: Content) -> some View {
        if #available(iOS 16.0, *) {
            content.contextMenu {
                Button(action: onOpen) {
                    Label("Open", systemImage: "message")
                }
            } preview: {
                DirectorySearchRow(entry: result.entry, imageSize: imageSize, state: result.state, showsOpen: false)
                    .padding()
                    .frame(width: UIScreen.main.bounds.width - 32)
                    .background(theme.colors.background)
                    .environmentObject(theme)
            }
        } else {
            content
        }
    }
}

func openDirectoryResult(_ result: DirectoryResult, theme: AppTheme, dismiss: Bool, cleanup: (() -> Void)?) {
    let prepareGroup: PrepareGroupMode = if case .joining = result.state { .confirm } else { .open }
    planAndConnect(result.entry.link, theme: theme, dismiss: dismiss, cleanup: cleanup, prepareGroup: prepareGroup)
}

func confirmDirectorySearch(_ search: @escaping () -> Void) {
    if UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_DIRECTORY_SEARCH) {
        search()
    } else {
        showDirectorySearchAlert(onSearch: search)
    }
}

private func showDirectorySearchAlert(onSearch: @escaping () -> Void) {
    showAlert(
        NSLocalizedString("Enable Directory search?", comment: "alert title"),
        message: NSLocalizedString("Search text is sent to SimpleX Directory, each search separately and without your profile.", comment: "alert message"),
        actions: {[
            UIAlertAction(title: NSLocalizedString("Cancel", comment: "alert action"), style: .cancel),
            UIAlertAction(
                title: NSLocalizedString("Enable", comment: "alert action"),
                style: .default,
                handler: { _ in
                    UserDefaults.standard.set(true, forKey: DEFAULT_PRIVACY_DIRECTORY_SEARCH)
                    onSearch()
                }
            )
        ]}
    )
}

private let DIRECTORY_PREVIEW_IMAGE = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAAT0lEQVR42u3V0QkAIAhFURdqxJZsodsCRUUqRO/i//kIzUptoWMC3gaAEIBRbgDzHABWXQHs9S3ASXqDKCB8DzI2OeMWZVxT/WgCBAjwng4ufNBwaLZEbgAAAABJRU5ErkJggg=="

private struct DirectorySearchRowsPreview: View {
    @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
    @FocusState private var searchFocussed: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            results
            SearchInDirectoryRow(searchFocussed: $searchFocussed, onSearch: {})
            DirectoryLoadMoreRowLayout(moreCount: 12, loading: false, onLoadMore: {})
            DirectoryLoadMoreRowLayout(moreCount: nil, loading: false, onLoadMore: {})
            DirectoryLoadMoreRowLayout(moreCount: 12, loading: true, onLoadMore: {})
            DirectoryRetryRowLayout(onRetry: {})
        }
        .padding()
    }

    @ViewBuilder private var results: some View {
        result("Privacy")
        result(
            "SimpleX news",
            entryType: DirectoryEntryType(groupType: .channel, summary: GroupSummary(currentMembers: 3, publicMemberCount: 2048)),
            shortDescr: "News about the app",
            image: DIRECTORY_PREVIEW_IMAGE
        )
        result("a")
        result(
            "The longest channel name that anyone could give to a group about private messaging",
            simplexName: "#private-messaging.simplex.chat",
            shortDescr: "Private messaging without user IDs: news, releases and help with servers, notifications and groups, with answers that never fit in the two lines of a row."
        )
        result("No description", shortDescr: nil)
        result("Joining", state: .joining)
        result("Long press", showsOpen: false)
    }

    private func result(
        _ name: String,
        entryType: DirectoryEntryType = DirectoryEntryType(summary: GroupSummary(currentMembers: 128)),
        simplexName: String? = nil,
        shortDescr: String? = "Private chats",
        image: String? = nil,
        state: DirectoryResultState = .open,
        showsOpen: Bool = true
    ) -> some View {
        DirectorySearchRowLayout(
            entryType: entryType,
            name: name,
            simplexName: simplexName,
            shortDescr: shortDescr,
            image: image,
            imageSize: dynamicSize(userFont).profileImageSize,
            state: state,
            showsOpen: showsOpen
        )
    }
}

struct DirectorySearchView_Previews: PreviewProvider {
    static var previews: some View {
        DirectorySearchRowsPreview()
    }
}
