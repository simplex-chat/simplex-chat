//
//  DirectorySearch.swift
//  SimpleX
//
//  Created by spaced4ndy on 12.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

// Must be the short link form: only that one carries the keys a service request needs.
let DIRECTORY_SERVICE_LINK = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

let DIRECTORY_SEARCH_TIMEOUT_SEC: Double = 10

// The directory refuses longer search text, counting Unicode scalars rather than characters.
let DIRECTORY_SEARCH_MAX_TEXT_LENGTH = 100

struct DirectoryPublicLink: Decodable, Hashable {
    var connFullLink: String? = nil
    var connShortLink: String? = nil
}

struct DirectoryEntryType: Decodable, Hashable {
    var groupType: GroupType? = nil
    var summary: GroupSummary
}

struct DirectorySearchEntry: Decodable, Hashable, Identifiable {
    var entryType: DirectoryEntryType
    var displayName: String
    var simplexName: String? = nil
    let link: String
    var shortDescr: String? = nil
    var image: String? = nil

    var id: String { link }

    enum CodingKeys: String, CodingKey {
        case entryType, displayName, simplexName, groupLink, shortDescr, image
    }

    init(from decoder: Decoder) throws {
        let c = try decoder.container(keyedBy: CodingKeys.self)
        entryType = try c.decode(DirectoryEntryType.self, forKey: .entryType)
        displayName = try c.decode(String.self, forKey: .displayName)
        simplexName = try c.decodeIfPresent(String.self, forKey: .simplexName)
        shortDescr = try c.decodeIfPresent(String.self, forKey: .shortDescr)
        image = try c.decodeIfPresent(String.self, forKey: .image).flatMap { isSafeImageData($0) ? $0 : nil }
        let groupLink = try c.decode(DirectoryPublicLink.self, forKey: .groupLink)
        // untrusted input: only group and channel links are kept
        guard let link = groupLink.connShortLink ?? groupLink.connFullLink,
              let linkType = simplexLinkType(link),
              linkType == .group || linkType == .channel else {
            throw DecodingError.dataCorruptedError(forKey: .groupLink, in: c, debugDescription: "not a group or channel link")
        }
        self.link = link
    }
}

func simplexLinkType(_ s: String) -> SimplexLinkType? {
    if case let .link(text, linkType, _) = strConnectTarget(s), text == s { linkType } else { nil }
}

private func isSafeImageData(_ s: String) -> Bool {
    s.hasPrefix("data:image/jpg;base64,/9j/") || s.hasPrefix("data:image/png;base64,iVBORw0KGgo")
}

// entries stay undecoded here so they can be decoded one at a time: one entry the app does not
// understand, because a newer directory added a field, must not discard the whole response
private struct DirectorySearchResponse: Decodable {
    var type: String
    var entries: [JSONValue]?
    var searchCursor: JSONValue?
    var moreCount: JSONValue?
}

struct DirectorySearchResults {
    var entries: [DirectorySearchEntry]
    var cursor: JSONValue?
    var moreCount: Int?
}

func directorySearchRequestJSON(_ text: String, _ cursor: JSONValue?) -> String {
    let searchText = String(String.UnicodeScalarView(text.unicodeScalars.prefix(DIRECTORY_SEARCH_MAX_TEXT_LENGTH)))
    var req: [String: JSONValue] = ["type": .string("search"), "searchText": .string(searchText)]
    if let cursor { req["searchCursor"] = cursor }
    return encodeJSON(JSONValue.object(req))
}

func parseDirectorySearchResponse(_ resp: JSONValue) -> DirectorySearchResults? {
    guard let r: DirectorySearchResponse = decodeJSONValue(resp), r.type == "searchResults" else {
        return nil
    }
    let entries = (r.entries ?? []).compactMap { (e: JSONValue) -> DirectorySearchEntry? in
        decodeJSONValue(e)
    }
    let moreCount: Int? = if case let .number(n)? = r.moreCount, n >= 0, n <= Double(Int32.max), n == n.rounded() { Int(n) } else { nil }
    return DirectorySearchResults(entries: entries, cursor: r.searchCursor, moreCount: moreCount)
}

func planLocalChatId(_ plan: ConnectionPlan) -> ChatId? {
    switch plan {
    case let .contactAddress(contactAddressPlan):
        switch contactAddressPlan {
        case let .known(contact), let .connectingProhibit(contact), let .contactViaAddress(contact): return contact.id
        case .ok, .ownLink, .connectingConfirmReconnect: return nil
        }
    case let .groupLink(groupLinkPlan):
        switch groupLinkPlan {
        case let .known(groupInfo), let .ownLink(groupInfo): return groupInfo.id
        case let .connectingProhibit(groupInfo_): return groupInfo_?.id
        case .ok, .connectingConfirmReconnect, .noRelays, .updateRequired: return nil
        }
    case let .invitationLink(invitationLinkPlan):
        switch invitationLinkPlan {
        case let .known(contact): return contact.id
        case let .connecting(contact_): return contact_?.id
        case .ok, .ownLink: return nil
        }
    case .error: return nil
    }
}

enum DirectoryResultState {
    case yourChat(ChatId)
    case joining
    case open
}

func directoryResultState(_ plan: ConnectionPlan?) -> DirectoryResultState {
    if let chatId = plan.flatMap(planLocalChatId) { return .yourChat(chatId) }
    switch plan {
    case .groupLink(.connectingConfirmReconnect)?, .groupLink(.connectingProhibit(nil))?: return .joining
    default: return .open
    }
}

struct DirectoryResult: Identifiable {
    let entry: DirectorySearchEntry
    var state: DirectoryResultState

    var id: String { entry.id }
}

struct OnlineSearchRows {
    var yourChats: [ChatId]
    var results: [DirectoryResult]
    var footer: DirectoryFooter?

    var isEmpty: Bool { yourChats.isEmpty && results.isEmpty && footer == nil }
}
