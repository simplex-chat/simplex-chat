//
//  DirectorySearch.swift
//  SimpleX
//
//  Created by spaced4ndy on 12.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

// The directory's address, as published in docs/DIRECTORY.md. It must be the short link form -
// only that one carries the keys a service request needs; a full link fails every time.
let DIRECTORY_SERVICE_LINK = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

// A search sets up an encrypted connection, so it takes seconds, not milliseconds.
let DIRECTORY_SEARCH_TIMEOUT_SEC: Double = 10

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
    var activeAt: Date? = nil
    var createdAt: Date? = nil

    // the link is stable and unique per entry, so it also de-duplicates across pages
    var id: String { link }

    enum CodingKeys: String, CodingKey {
        case entryType, displayName, simplexName, groupLink, shortDescr, image, activeAt, createdAt
    }

    init(from decoder: Decoder) throws {
        let c = try decoder.container(keyedBy: CodingKeys.self)
        entryType = try c.decode(DirectoryEntryType.self, forKey: .entryType)
        displayName = try c.decode(String.self, forKey: .displayName)
        simplexName = try c.decodeIfPresent(String.self, forKey: .simplexName)
        shortDescr = try c.decodeIfPresent(String.self, forKey: .shortDescr)
        image = try c.decodeIfPresent(String.self, forKey: .image)
        activeAt = try c.decodeIfPresent(Date.self, forKey: .activeAt)
        createdAt = try c.decodeIfPresent(Date.self, forKey: .createdAt)
        let groupLink = try c.decode(DirectoryPublicLink.self, forKey: .groupLink)
        // untrusted input: only group and channel links are kept, as the core fetches an invitation link even in a store-only plan
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
    // opaque: stored and echoed back on the next request, never inspected
    var cursor: JSONValue?
    var moreCount: Int?
}

func directorySearchRequestJSON(_ text: String, _ cursor: JSONValue?) -> String {
    var req: [String: JSONValue] = ["type": .string("search"), "searchText": .string(text)]
    if let cursor { req["searchCursor"] = cursor }
    return encodeJSON(JSONValue.object(req))
}

// Anything but a well-formed results response is a failure, not something to salvage: it comes
// from another party, not from our own core.
func parseDirectorySearchResponse(_ resp: JSONValue) -> DirectorySearchResults? {
    guard let r: DirectorySearchResponse = decodeJSONValue(resp), r.type == "searchResults" else {
        return nil
    }
    let entries = (r.entries ?? []).compactMap { (e: JSONValue) -> DirectorySearchEntry? in
        decodeJSONValue(e)
    }
    // only shown as text, so a count out of range is dropped rather than failing the response
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

// no plan means the store doesn't know the link, or the plan failed and Open's online plan will show why
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
    var pageInChats: Bool
    var footer: DirectoryFooter?

    var isEmpty: Bool { yourChats.isEmpty && results.isEmpty && !pageInChats && footer == nil }
}
