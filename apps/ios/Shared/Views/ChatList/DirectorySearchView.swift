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

// Online results are not chats and are never saved: they live as long as the search text does.
@MainActor
class OnlineSearchModel: ObservableObject {
    @Published private(set) var directory: [DirectoryResult] = []
    @Published private(set) var moreCount: Int? = nil
    @Published private(set) var loading = false { didSet { updateSpinner() } }
    @Published private(set) var failed = false
    // tells "nothing searched yet" apart from "searched and found nothing"
    @Published private(set) var searched = false

    private var cursor: JSONValue? = nil
    private var searchedText = ""
    // bumped on every reset, so a reply that arrives too late cannot refill a list already cleared
    private var generation = 0
    private var spinnerShown = false

    var searching: Bool { loading }
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

    // a way out of the wait, not out of the results: what is already shown stays
    func cancelRequest() {
        generation += 1
        loading = false
    }

    func searchDirectory(_ text: String) async {
        let text = text.trimmingCharacters(in: .whitespaces)
        guard !text.isEmpty, !(loading && text == searchedText) else { return }
        if text == searchedText {
            directory = []
            moreCount = nil
            cursor = nil
            failed = false
            searched = false
        } else {
            reset()
            searchedText = text
        }
        await request(append: false)
    }

    func loadMore() async {
        guard cursor != nil, !loading else { return }
        await request(append: true)
    }

    // a failed "Load more" keeps its cursor, so the retry resumes the page rather than starting over
    func retry() async {
        if cursor != nil {
            await loadMore()
        } else if !loading {
            await request(append: false)
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

    // a chat opened from a result exists only once its connection flow ends, so results look again then
    func refreshStates() async {
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

    private func resultState(_ link: String) async -> DirectoryResultState {
        let plan = await apiConnectPlan(connLink: link, resolveMode: .never, inProgress: BoxedValue(false), onFailure: .log)
        return directoryResultState(plan?.connectionPlan)
    }

    private func request(append: Bool) async {
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
        directory = append ? directory + page : page
    }

    private func updateSpinner() {
        guard searching != spinnerShown else { return }
        spinnerShown = searching
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
    @EnvironmentObject var theme: AppTheme
    var entry: DirectorySearchEntry
    var imageSize: CGFloat
    var state: DirectoryResultState
    var showsOpen = true

    var body: some View {
        HStack(spacing: 8) {
            ProfileImage(
                imageStr: entry.image,
                icon: Image(systemName: isChannel ? "antenna.radiowaves.left.and.right.circle.fill" : "person.2.circle.fill"),
                size: imageSize
            )
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 4) {
                    Text(displayName).fontWeight(.bold).lineLimit(1)
                    if let simplexName = entry.simplexName {
                        // the directory's claim, shown as plain text: connecting uses the link
                        Text(simplexName)
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(1)
                    }
                }
                if let descr = entry.shortDescr, !descr.isEmpty {
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

    private var displayName: String { "#" + entry.displayName }

    private var isChannel: Bool { entry.entryType.groupType == .channel }

    private var joining: Bool {
        if case .joining = state { true } else { false }
    }

    private var membersText: String {
        let summary = entry.entryType.summary
        let count = summary.publicMemberCount ?? summary.currentMembers
        return isChannel
            ? subscriberCountStr(count)
            : String.localizedStringWithFormat(NSLocalizedString("%d members", comment: "directory search result"), Int(count))
    }
}

struct DirectoryLoadMoreRow: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var onlineSearch: OnlineSearchModel

    var body: some View {
        Button {
            confirmDirectorySearch { Task { await onlineSearch.loadMore() } }
        } label: {
            HStack(spacing: 4) {
                Image(systemName: "plus.magnifyingglass")
                Text(loadMoreText)
            }
            .foregroundColor(onlineSearch.loading ? theme.colors.secondary : theme.colors.primary)
        }
        .disabled(onlineSearch.loading)
    }

    private var loadMoreText: String {
        if let n = onlineSearch.moreCount, n > 0 {
            String.localizedStringWithFormat(NSLocalizedString("Load more · %d left", comment: "directory search results"), n)
        } else {
            NSLocalizedString("Load more", comment: "directory search results")
        }
    }
}

struct DirectoryRetryRow: View {
    @EnvironmentObject var theme: AppTheme
    var onlineSearch: OnlineSearchModel

    var body: some View {
        Button {
            confirmDirectorySearch { Task { await onlineSearch.retry() } }
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

// a pending join keeps the sheet or alert the connection flow shows for it
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
