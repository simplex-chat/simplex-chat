import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class ModerationTests: RecordedAPITestCase {
    private let contactDeletion = privacyContactDeletionDefault.get()
    private let showDeleteContactNotice = showDeleteContactNoticeDefault.get()
    private let showDeleteConversationNotice = showDeleteConversationNoticeDefault.get()
    private static let until = ISO8601DateFormatter().date(from: "2099-01-01T12:00:00Z")!.formatted(date: .abbreviated, time: .shortened)

    @MainActor
    override func tearDown() async throws {
        privacyContactDeletionDefault.set(contactDeletion)
        showDeleteContactNoticeDefault.set(showDeleteContactNotice)
        showDeleteConversationNoticeDefault.set(showDeleteConversationNotice)
        ChatModel.shared.groupMembers = []
        ChatModel.shared.groupMembersIndexes = [:]
        ChatModel.shared.userAddress = nil
        AlertManager.shared.presentAlert = false
        try await super.tearDown()
    }

    @MainActor
    func testContactNoticePlan() async throws {
        try recorded("bans/contact-notices")
        connect("ALICE_LINK")
        try await assertTexts(["Not allowed", "Blocked by this contact until \(Self.until).", "Ok", "How it works"])
        try await press("Ok")
        try await assertTexts([])
    }

    @MainActor
    func testContactNoticeForeverPlan() async throws {
        try recorded("bans/contact-notices")
        connect("CATH_LINK")
        try await assertTexts(["Not allowed", "Blocked by this contact.", "Ok", "How it works"])
    }

    @MainActor
    func testContactNoticeConnect() async throws {
        try recorded("bans/contact-notices")
        _ = await apiConnect(incognito: false, connLink: CreatedConnLink(connFullLink: "ALICE_FULL_LINK", connShortLink: "ALICE_LINK"))
        try await assertTexts(["Not allowed", "Blocked by this contact until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testPreparedContactNotice() async throws {
        try recorded("bans/connect-contact-notice")
        _ = await apiConnectPreparedContact(contactId: 5, incognito: false, msg: nil)
        try await assertTexts(["Not allowed", "Blocked by this contact until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testContactCardNotice() async throws {
        try recorded("bans/connect-contact-notice")
        _ = await apiConnectContactViaAddress(incognito: false, contactId: 6)
        try await assertTexts(["Not allowed", "Blocked by this contact until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testGroupNoticePlan() async throws {
        try recorded("bans/plan-group-notice")
        connect("GROUP_LINK")
        try await assertTexts(["Not allowed", "Blocked by the admins of this group until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testChannelNoticePlan() async throws {
        try recorded("bans/plan-channel-notice")
        connect("CHANNEL_LINK")
        try await assertTexts(["Not allowed", "Blocked by the owners of this channel until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testChannelNameNoticePlan() async throws {
        try recorded("names/plan-channel-notice")
        connect("team.simplex")
        try await assertTexts(["Not allowed", "Blocked by the owners of this channel until \(Self.until).", "Ok", "Connect to @team.simplex", "How it works"])
        try await press("Connect to @team.simplex")
        try await waitFor { self.presented() != nil && self.shownTexts().contains("alice") }
    }

    @MainActor
    func testBusinessNoticePlan() async throws {
        try recorded("bans/plan-business-notice")
        connect("BUSINESS_LINK")
        try await assertTexts(["Not allowed", "Blocked by this business until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testPreparedGroupNotice() async throws {
        try recorded("bans/connect-group-notice")
        try await connectPreparedGroup("/_get chat #2 count=0")
        try await assertTexts(["Not allowed", "Blocked by the admins of this group until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testPreparedChannelNotice() async throws {
        try recorded("bans/connect-channel-notice")
        try await connectPreparedGroup("/_get chat #1 count=0")
        try await assertTexts(["Not allowed", "Blocked by the owners of this channel until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testPreparedBusinessNotice() async throws {
        try recorded("bans/connect-business-notice")
        try await connectPreparedGroup("/_get chat #2 count=0")
        try await assertTexts(["Not allowed", "Blocked by this business until \(Self.until).", "Ok", "How it works"])
    }

    @MainActor
    func testBlockMemberForDay() async throws {
        try recorded("bans/block-members")
        let (groupInfo, members) = try openGroup()
        showBanMemberSheet(groupInfo, try member(members, "bob"))
        let dialog = try await sheet(BanDialog.self)
        XCTAssertEqual(dialog.title, "Block member for all?")
        XCTAssertEqual(dialog.actions.map(\.title), ["Block", "Block and remove", "Block, remove and delete messages"])
        XCTAssertEqual(shownText(dialog.footer), "bob won't be notified, and if removed, can't rejoin while blocked.")
        dialog.close { dialog.actions[0].action(.day) }
        try await awaitMember("bob") { $0.blockedByAdmin && $0.blockedUntil != nil }
    }

    @MainActor
    func testBlockMemberForWeek() async throws {
        try recorded("bans/block-members")
        let (groupInfo, members) = try openGroup()
        showBanMemberSheet(groupInfo, try member(members, "cath"))
        let dialog = try await sheet(BanDialog.self)
        dialog.close { dialog.actions[0].action(.week) }
        try await awaitMember("cath") { $0.blockedByAdmin && $0.blockedUntil != nil }
    }

    @MainActor
    func testBlockMemberForever() async throws {
        try recorded("bans/block-members")
        let (groupInfo, members) = try openGroup()
        showBanMemberSheet(groupInfo, try member(members, "dan"))
        let dialog = try await sheet(BanDialog.self)
        dialog.close { dialog.actions[0].action(.forever) }
        try await awaitMember("dan") { $0.blockedByAdmin && $0.blockedUntil == nil }
    }

    @MainActor
    func testUnblockMember() async throws {
        try recorded("bans/block-members")
        let (groupInfo, _) = try openGroup()
        let bob = try member(try blockedMembers("/_block #1 2 blocked=on ttl=86400"), "bob")
        replaceMembers([bob])
        AlertManager.shared.showAlert(unblockForAllAlert(groupInfo, bob))
        try await assertTexts(["Unblock member for all?", "Messages from bob will be shown for all members.", "Unblock for all", "Cancel"])
        try await press("Unblock for all")
        try await awaitMember("bob") { !$0.blockedByAdmin }
    }

    @MainActor
    func testMemberInfoBlockedText() async throws {
        try recorded("bans/block-members")
        let (groupInfo, _) = try openGroup()
        let bob = try member(try blockedMembers("/_block #1 2 blocked=on ttl=86400"), "bob")
        let dan = try member(try blockedMembers("/_block #1 4 blocked=on"), "dan")
        let info = GroupMemberInfoView(groupInfo: groupInfo, chat: groupChat(groupInfo), groupMember: GMember(bob), scrollToItemId: .constant(nil))
        XCTAssertEqual(info.bannedText(bob), "Blocked until \(Self.until)")
        XCTAssertEqual(info.bannedText(dan), "Blocked by admin")
    }

    @MainActor
    func testBlockAndRemoveMember() async throws {
        try recorded("bans/block-members")
        let (groupInfo, members) = try openGroup()
        showBanMemberSheet(groupInfo, try member(members, "bob"))
        let dialog = try await sheet(BanDialog.self)
        dialog.close { dialog.actions[1].action(.day) }
        try await awaitMember("bob") { $0.memberStatus == .memRemoved }
    }

    @MainActor
    func testRemoveBlockedMemberForever() async throws {
        try recorded("bans/block-members")
        let (groupInfo, _) = try openGroup()
        let cath = try member(try blockedMembers("/_block #1 2,3 blocked=on ttl=86400"), "cath")
        showRemoveMemberAlert(groupInfo, cath)
        let dialog = try await sheet(BanDialog.self)
        XCTAssertEqual(dialog.title, "Remove member?")
        XCTAssertEqual(dialog.actions.map(\.title), ["Remove", "Remove and delete messages"])
        XCTAssertEqual(shownText(dialog.footer), "cath won't be notified, and can't rejoin while blocked.")
        dialog.close { dialog.actions[1].action(.forever) }
        try await assertTexts(["Remove forever?", "cath can't rejoin via the group link again, except from another app.", "Cancel", "Remove"])
        let alert = try XCTUnwrap(presented() as? UIAlertController)
        let confirm = try XCTUnwrap(alert.actions.first { $0.title == "Remove" })
        XCTAssertFalse(confirm.isEnabled)
        let field = try XCTUnwrap(alert.textFields?.first)
        XCTAssertEqual(field.placeholder, "Type yes")
        field.text = "yes"
        field.sendActions(for: .editingChanged)
        XCTAssertTrue(confirm.isEnabled)
        try await press("Remove")
        try await awaitMember("cath") { $0.memberStatus == .memRemoved }
    }

    @MainActor
    func testRemoveMemberNotBlocked() async throws {
        try recorded("bans/block-members")
        let (groupInfo, members) = try openGroup()
        showRemoveMemberAlert(groupInfo, try member(members, "dan"))
        try await assertTexts(["Remove member?", "Member will be removed from group - this cannot be undone!", "Remove", "Remove and delete messages", "Cancel"])
        try await press("Remove")
        try await awaitMember("dan") { $0.memberStatus == .memRemoved }
    }

    @MainActor
    func testHideMemberMessages() async throws {
        try recorded("bans/hide-messages")
        let (groupInfo, members) = try openGroup()
        AlertManager.shared.showAlert(blockMemberAlert(groupInfo, try member(members, "cath")))
        try await assertTexts(["Hide messages?", "All new messages from cath will be hidden!", "Hide", "Cancel"])
        try await press("Hide")
        try await awaitMember("cath") { !$0.memberSettings.showMessages }
    }

    @MainActor
    func testShowMemberMessages() async throws {
        try recorded("bans/hide-messages")
        let (groupInfo, members) = try openGroup()
        var cath = try member(members, "cath")
        cath.memberSettings.showMessages = false
        replaceMembers([cath])
        AlertManager.shared.showAlert(unblockMemberAlert(groupInfo, cath))
        try await assertTexts(["Show messages?", "Messages from cath will be shown!", "Show", "Cancel"])
        try await press("Show")
        try await awaitMember("cath") { $0.memberSettings.showMessages }
    }

    @MainActor
    func testBlockMemberOfPreviousVersion() async throws {
        try recorded("bans/previous-version")
        let (groupInfo, members) = try openGroup()
        showBanMemberSheet(groupInfo, try member(members, "cath"))
        let dialog = try await sheet(BanDialog.self)
        XCTAssertEqual(dialog.actions.map(\.title), ["Block"])
        XCTAssertEqual(shownText(dialog.footer), "cath won't be notified, and the messages will be hidden for all.")
    }

    @MainActor
    func testBlockPendingMember() async throws {
        try recorded("bans/pending-member")
        let (groupInfo, members) = try openGroup()
        let bob = try member(members, "bob")
        XCTAssertTrue(bob.canBlockForAll(groupInfo: groupInfo))
        showBanMemberSheet(groupInfo, bob)
        let dialog = try await sheet(BanDialog.self)
        dialog.close { dialog.actions[0].action(.day) }
        try await awaitMember("bob") { $0.blockedByAdmin }
    }

    @MainActor
    func testAcceptBlockedPendingMember() async throws {
        try recorded("bans/pending-member")
        let (groupInfo, _) = try openGroup()
        let bob = try member(try blockedMembers("/_block #1 2 blocked=on ttl=86400"), "bob")
        replaceMembers([bob])
        showAcceptMemberAlert(groupInfo, bob)
        try await assertTexts(["Accept member", "Member will be unblocked and will join the group, accept member?", "Accept as member", "Accept as observer", "Cancel"])
        try await press("Accept as member")
        try await awaitMember("bob") { !$0.memberPending && !$0.blockedByAdmin }
    }

    @MainActor
    func testAcceptBlockedPendingMemberUnblockFails() async throws {
        try recorded("bans/pending-member")
        let (groupInfo, members) = try openGroup()
        var other = try member(members, "bob")
        other.groupMemberId = 3
        other.blockedByAdmin = true
        showAcceptMemberAlert(groupInfo, other)
        try await press("Accept as member")
        try await assertRequested { $0 == "/_block #1 3 blocked=off" }
        try await waitFor { self.shownTexts().contains("Error blocking member for all") }
    }

    @MainActor
    func testAcceptPendingMember() async throws {
        try recorded("bans/pending-member")
        let (groupInfo, members) = try openGroup()
        showAcceptMemberAlert(groupInfo, try member(members, "bob"))
        try await assertTexts(["Accept member", "Member will join the group, accept member?", "Accept as member", "Accept as observer", "Cancel"])
        try await press("Accept as member")
        try await awaitMember("bob") { !$0.memberPending }
    }

    @MainActor
    func testBlockSubscriber() async throws {
        try recorded("bans/channel")
        let (groupInfo, members) = try openGroup()
        showBanMemberSheet(groupInfo, try member(members, "cath"))
        let dialog = try await sheet(BanDialog.self)
        XCTAssertEqual(dialog.title, "Block subscriber for all?")
        XCTAssertEqual(dialog.actions.map(\.title), ["Block", "Block and remove", "Block, remove and delete messages"])
        dialog.close { dialog.actions[0].action(.day) }
        try await awaitMember("cath") { $0.blockedByAdmin }
    }

    @MainActor
    func testUnblockSubscriber() async throws {
        try recorded("bans/channel")
        let (groupInfo, _) = try openGroup()
        let cath = try member(try blockedMembers("/_block #1 3 blocked=on ttl=86400"), "cath")
        replaceMembers([cath])
        AlertManager.shared.showAlert(unblockForAllAlert(groupInfo, cath))
        try await assertTexts(["Unblock subscriber for all?", "Messages from cath will be shown for all subscribers.", "Unblock for all", "Cancel"])
        try await press("Unblock for all")
        try await awaitMember("cath") { !$0.blockedByAdmin }
    }

    @MainActor
    func testRemoveSubscriberForever() async throws {
        try recorded("bans/channel")
        let (groupInfo, members) = try openGroup()
        showBanMemberSheet(groupInfo, try member(members, "dan"))
        let dialog = try await sheet(BanDialog.self)
        dialog.close { dialog.actions[1].action(.forever) }
        try await assertTexts(["Remove forever?", "dan can't rejoin via the channel link again, except from another app.", "Cancel", "Remove"])
        let field = try XCTUnwrap((presented() as? UIAlertController)?.textFields?.first)
        field.text = "yes"
        field.sendActions(for: .editingChanged)
        try await press("Remove")
        try await awaitMember("dan") { $0.memberStatus == .memRemoved }
    }

    @MainActor
    func testDeleteChannelKeepingConversation() async throws {
        try recorded("bans/channel")
        let (groupInfo, _) = try openGroup()
        AlertManager.shared.showAlert(GroupChatInfoView.deleteGroupAlert(groupChat(groupInfo), groupInfo))
        try await assertTexts(["Delete channel?", "team\n\nChannel will be deleted for all subscribers - this cannot be undone!", "Delete", "Cancel"])
        try await press("Delete")
        let dialog = try await sheet(KeepConversationDialog.self)
        XCTAssertNil(dialog.footer)
        dialog.close { dialog.delete(true) }
        try await waitFor { self.groupInfo(groupInfo.id)?.membership.memberStatus == .memGroupDeleted }
    }

    @MainActor
    func testDeleteOwnGroup() async throws {
        try recorded("bans/group-deletion")
        let (groupInfo, _) = try openGroup()
        AlertManager.shared.showAlert(GroupChatInfoView.deleteGroupAlert(groupChat(groupInfo), groupInfo))
        try await assertTexts(["Delete group?", "team\n\nGroup will be deleted for all members - this cannot be undone!", "Delete", "Cancel"])
        try await press("Delete")
        let dialog = try await sheet(KeepConversationDialog.self)
        XCTAssertEqual(dialog.actionTitle, "Delete")
        XCTAssertNil(dialog.footer)
        XCTAssertNil(dialog.confirmWord)
        dialog.close { dialog.delete(false) }
        try await waitFor { ChatModel.shared.getChat(groupInfo.id) == nil }
    }

    @MainActor
    func testDeleteOwnGroupKeepingConversation() async throws {
        try recorded("bans/group-deletion")
        let (groupInfo, _) = try openGroup("/_members #2")
        AlertManager.shared.showAlert(GroupChatInfoView.deleteGroupAlert(groupChat(groupInfo), groupInfo))
        try await press("Delete")
        let dialog = try await sheet(KeepConversationDialog.self)
        dialog.close { dialog.delete(true) }
        try await waitFor { self.groupInfo(groupInfo.id)?.membership.memberStatus == .memGroupDeleted }
    }

    @MainActor
    func testDeleteGroupAsMember() async throws {
        try recorded("bans/group-deletion-member")
        let (groupInfo, _) = try openGroup()
        AlertManager.shared.showAlert(GroupChatInfoView.deleteGroupAlert(groupChat(groupInfo), groupInfo))
        try await assertTexts(["Delete group?", "team\n\nGroup will be deleted for you - this cannot be undone!", "Delete", "Cancel"])
        try await press("Delete")
        try await waitFor { ChatModel.shared.getChat(groupInfo.id) == nil }
    }

    @MainActor
    func testDeleteBusinessChatKeepingConversation() async throws {
        try recorded("bans/business")
        try loadAddress()
        let (groupInfo, _) = try openGroup("/_members #2")
        AlertManager.shared.showAlert(GroupChatInfoView.deleteGroupAlert(groupChat(groupInfo), groupInfo))
        try await assertTexts(["Delete chat?", "cath\n\nChat will be deleted for all members - this cannot be undone!", "Delete", "Cancel"])
        try await press("Delete")
        let dialog = try await sheet(KeepConversationDialog.self)
        XCTAssertEqual(shownText(dialog.footer), "cath will be notified, and can reconnect via your address.")
        dialog.close { dialog.delete(true) }
        try await waitFor { self.groupInfo(groupInfo.id)?.membership.memberStatus == .memGroupDeleted }
    }

    @MainActor
    func testDeleteAndBlockCustomer() async throws {
        try recorded("bans/business")
        let (groupInfo, members) = try openGroup()
        XCTAssertTrue(groupInfo.canDeleteAndBanCustomer(members))
        deleteAndBanCustomerDialog(groupChat(groupInfo), groupInfo)
        let dialog = try await sheet(BanDialog.self)
        XCTAssertEqual(dialog.title, "Delete and block?")
        XCTAssertEqual(dialog.actions.map(\.title), ["Delete and block"])
        XCTAssertEqual(shownText(dialog.footer), "Chat with bob will be deleted for all members - this cannot be undone!")
        dialog.close { dialog.actions[0].action(.day) }
        let keep = try await sheet(KeepConversationDialog.self)
        XCTAssertEqual(keep.title, "Delete and block?")
        XCTAssertEqual(shownText(keep.footer), "bob won't be notified, and can't connect to you for 1 day.")
        keep.close { keep.delete(false) }
        try await waitFor { ChatModel.shared.getChat(groupInfo.id) == nil }
    }

    @MainActor
    func testCantBlockCustomerWhoLeft() async throws {
        try recorded("bans/business")
        let (groupInfo, members) = try openGroup("/_members #3")
        XCTAssertFalse(groupInfo.canDeleteAndBanCustomer(members))
        AlertManager.shared.showAlert(deleteAndBanUnavailableAlert(groupInfo))
        try await assertTexts(["Can't block", "dan left this chat or uses an older app version.", "OK"])
    }

    @MainActor
    func testDeleteContactNotifying() async throws {
        try recorded("bans/contact-deletion")
        privacyContactDeletionDefault.set(.notify)
        try loadAddress()
        let (chat, contact) = try openContact("/_get chat @5 count=0")
        deleteContactDialog(chat, contact, dismissToChatList: false, showAlert: { AlertManager.shared.showAlert($0.alert) }, showActionSheet: { _ in XCTFail("action sheet in Notify mode") })
        try await assertTexts(["Delete contact?", "Contact with eve will be deleted - this cannot be undone!", "Delete", "Cancel"])
        try await press("Delete")
        let dialog = try await sheet(KeepConversationDialog.self)
        XCTAssertEqual(shownText(dialog.footer), "eve will be notified, and can reconnect via your address.")
        dialog.close { dialog.delete(false) }
        try await waitFor { ChatModel.shared.getChat(chat.id) == nil }
    }

    @MainActor
    func testDeleteContactSilentlyKeepingConversation() async throws {
        try recorded("bans/contact-deletion")
        privacyContactDeletionDefault.set(.ask)
        try loadAddress()
        let (chat, contact) = try openContact("/_get chat @5 count=0")
        try await withActionSheetHost { host in
            deleteContactDialog(chat, contact, dismissToChatList: false, showAlert: { AlertManager.shared.showAlert($0.alert) }, showActionSheet: { host.sheet = $0 })
            try await assertTexts(["Delete contact?", "Contact with eve will be deleted - this cannot be undone!", "Delete and notify", "Delete silently", "Cancel"])
            try await press("Delete silently")
            let dialog = try await sheet(KeepConversationDialog.self)
            XCTAssertEqual(shownText(dialog.footer), "eve won't be notified, and can reconnect via your address.")
            dialog.close { dialog.delete(true) }
            try await assertRequested { $0 == "/_delete @5 entity notify=off" }
            try await waitFor { AlertManager.shared.presentAlert }
        }
    }

    @MainActor
    func testDeleteContactWithoutAddress() async throws {
        try recorded("bans/contact-notices")
        privacyContactDeletionDefault.set(.ask)
        let (chat, contact) = try openContact("/_get chat @2 count=0")
        try await withActionSheetHost { host in
            deleteContactDialog(chat, contact, dismissToChatList: false, showAlert: { AlertManager.shared.showAlert($0.alert) }, showActionSheet: { host.sheet = $0 })
            try await press("Delete silently")
            let dialog = try await sheet(KeepConversationDialog.self)
            XCTAssertEqual(shownText(dialog.footer), "alice won't be notified.")
            dialog.close { dialog.delete(false) }
            try await waitFor { ChatModel.shared.getChat(chat.id) == nil }
        }
    }

    @MainActor
    func testArchiveContactFails() async throws {
        try recorded("bans/contact-deletion")
        let (chat, contact) = try openContact("/_get chat @3 count=0")
        archiveContact(chat, contact)
        try await assertRequested { $0 == "/_delete @3 archive" }
        try await waitFor { self.shownTexts().contains("Error archiving contact") }
    }

    @MainActor
    func testCustomerLoadFailureShowsError() async throws {
        try recorded("bans/business")
        var (groupInfo, _) = try openGroup()
        groupInfo.groupId = 9
        ChatListNavLink(chat: groupChat(groupInfo), parentSheet: .constant(nil)).deleteAndBanCustomerIfAvailable(groupInfo)
        try await assertRequested { $0 == "/_members #9" }
        try await assertTexts(["Error loading members", "Ok"])
    }

    @MainActor
    func testBlockMemberFails() async throws {
        try recorded("bans/block-members")
        let (groupInfo, members) = try openGroup()
        showBanMemberSheet(groupInfo, try member(members, "dan"))
        let dialog = try await sheet(BanDialog.self)
        dialog.close { dialog.actions[0].action(.week) }
        try await assertRequested { $0 == "/_block #1 4 blocked=on ttl=604800" }
        try await waitFor { self.shownTexts().contains("Error blocking member for all") }
    }

    @MainActor
    func testArchiveContact() async throws {
        try recorded("bans/contact-deletion")
        showDeleteConversationNoticeDefault.set(true)
        let (chat, contact) = try openContact("/_get chat @2 count=0")
        archiveContact(chat, contact)
        try await assertTexts(["Contact archived!", "You can send messages to bob from Archived contacts.", "Don't show again", "Ok"])
        guard case let .direct(archived)? = ChatModel.shared.getChat(chat.id)?.chatInfo else { return XCTFail("contact removed") }
        XCTAssertTrue(archived.chatDeleted)
    }

    @MainActor
    func testDeleteArchivedContactKeepingConversation() async throws {
        try recorded("bans/contact-deletion")
        privacyContactDeletionDefault.set(.notify)
        showDeleteContactNoticeDefault.set(true)
        let r: ChatResponse1 = try recordedResult("/_delete @2 archive")
        guard case let .contactDeleted(_, archived) = r else { return XCTFail("archive not recorded") }
        let chat = openChat(Chat(chatInfo: .direct(contact: archived)))
        deleteContactDialog(chat, archived, dismissToChatList: false, showAlert: { AlertManager.shared.showAlert($0.alert) }, showActionSheet: { _ in XCTFail("action sheet in Notify mode") })
        try await press("Delete")
        let dialog = try await sheet(KeepConversationDialog.self)
        dialog.close { dialog.delete(true) }
        try await assertTexts(["Contact deleted!", "You can still view conversation with bob in Archived contacts.", "Don't show again", "Ok"])
    }

    @MainActor
    func testDeleteAndBlockContactForDay() async throws {
        try recorded("bans/contact-deletion")
        showDeleteContactNoticeDefault.set(true)
        let (chat, contact) = try openContact("/_get chat @3 count=0")
        XCTAssertTrue(contact.canDeleteAndBan)
        deleteAndBanContactDialog(chat, contact, dismissToChatList: false, showAlert: { AlertManager.shared.showAlert($0.alert) })
        let dialog = try await sheet(BanDialog.self)
        XCTAssertEqual(dialog.title, "Delete and block?")
        XCTAssertEqual(dialog.actions.map(\.title), ["Delete and block"])
        XCTAssertEqual(shownText(dialog.footer), "Contact with cath will be deleted - this cannot be undone!")
        dialog.close { dialog.actions[0].action(.day) }
        let keep = try await sheet(KeepConversationDialog.self)
        XCTAssertEqual(keep.title, "Delete and block?")
        XCTAssertEqual(keep.actionTitle, "Delete and block")
        XCTAssertEqual(shownText(keep.footer), "cath won't be notified, and can't connect to you for 1 day.")
        XCTAssertNil(keep.confirmWord)
        keep.close { keep.delete(true) }
        try await assertTexts(["Contact deleted!", "You can still view conversation with cath in the list of chats.", "Don't show again", "Ok"])
    }

    @MainActor
    func testDeleteAndBlockContactForWeek() async throws {
        try recorded("bans/contact-deletion")
        let (chat, contact) = try openContact("/_get chat @3 count=0")
        deleteAndBanContactDialog(chat, contact, dismissToChatList: false, showAlert: { AlertManager.shared.showAlert($0.alert) })
        let dialog = try await sheet(BanDialog.self)
        dialog.close { dialog.actions[0].action(.week) }
        let keep = try await sheet(KeepConversationDialog.self)
        XCTAssertEqual(shownText(keep.footer), "cath won't be notified, and can't connect to you for 1 week.")
        keep.close { keep.delete(false) }
        try await assertRequested { $0 == "/_delete @3 full notify=off notice={\"ttl\":604800}" }
        try await waitFor { AlertManager.shared.presentAlert }
    }

    @MainActor
    func testDeleteAndBlockContactForever() async throws {
        try recorded("bans/contact-deletion")
        let (chat, contact) = try openContact("/_get chat @4 count=0")
        deleteAndBanContactDialog(chat, contact, dismissToChatList: false, showAlert: { AlertManager.shared.showAlert($0.alert) })
        let dialog = try await sheet(BanDialog.self)
        dialog.close { dialog.actions[0].action(.forever) }
        let keep = try await sheet(KeepConversationDialog.self)
        XCTAssertEqual(keep.title, "Delete and block forever?")
        XCTAssertEqual(shownText(keep.footer), "dan won't be notified, and can't connect via your address again, except from another app.")
        XCTAssertEqual(keep.confirmWord, "yes")
        keep.close { keep.delete(false) }
        try await waitFor { ChatModel.shared.getChat(chat.id) == nil }
    }

    func testTypedWordConfirms() {
        XCTAssertTrue(typedWordConfirms(" Yes ", "yes"))
        XCTAssertFalse(typedWordConfirms("ye", "yes"))
        XCTAssertFalse(typedWordConfirms(nil, "yes"))
    }

    @MainActor
    func testContactNotViaAddressCantBeBlocked() async throws {
        try recorded("bans/contact-deletion")
        let (chat, contact) = try openContact("/_get chat @5 count=0")
        XCTAssertFalse(contact.canDeleteAndBan)
        XCTAssertEqual(ChatListActions.contact(chat, contact).shown.map { $0.label.title }, ["Delete", "Archive"])
        let (cathChat, cath) = try openContact("/_get chat @3 count=0")
        XCTAssertEqual(ChatListActions.contact(cathChat, cath).shown.map { $0.label.title }, ["Delete", "Block", "Archive"])
    }

    @MainActor
    func testDeleteContactThatDeletedYou() async throws {
        try recorded("bans/contact-notices")
        let (chat, contact) = try openContact("/_get chat @4 count=0")
        try await withActionSheetHost { host in
            deleteContactDialog(chat, contact, dismissToChatList: false, showAlert: { AlertManager.shared.showAlert($0.alert) }, showActionSheet: { host.sheet = $0 })
            try await assertTexts(["Confirm contact deletion?", "dan", "Confirm", "Cancel"])
            try await press("Confirm")
            try await waitFor { ChatModel.shared.getChat(chat.id) == nil }
        }
    }

    @MainActor
    func testBusinessChatListActions() async throws {
        try recorded("bans/business")
        let (groupInfo, _) = try openGroup()
        XCTAssertEqual(ChatListActions.group(groupChat(groupInfo), groupInfo).shown.map { $0.label.title }, ["Delete", "Block", "Leave"])
    }

    func testReportReasonIsContent() {
        XCTAssertEqual(ChatCommand.apiReportMessage(groupId: 1, chatItemId: 2, reportReason: .content, reportText: "spam").cmdString, "/_report #1 2 reason=content spam")
    }

    private func shownText(_ text: Text?) -> String? {
        guard let text else { return nil }
        let description = String(describing: text)
        guard let start = description.range(of: ">: \""), let end = description.range(of: "\"), modifiers", options: .backwards) else { return description }
        return String(description[start.upperBound..<end.lowerBound])
    }

    @MainActor
    private func connectPreparedGroup(_ chatCmd: String) async throws {
        let r: ChatResponse0 = try recordedResult(chatCmd)
        guard case let .apiChat(_, chat, _) = r, case let .group(groupInfo, _) = chat.chatInfo else { return XCTFail("no group in \(chatCmd)") }
        _ = await apiConnectPreparedGroup(groupId: groupInfo.groupId, noticeTarget: LinkNoticeTarget(groupInfo), incognito: false, msg: nil)
    }

    @MainActor
    private func openGroup(_ cmd: String = "/_members #1") throws -> (GroupInfo, [GroupMember]) {
        let r: ChatResponse2 = try recordedResult(cmd)
        guard case let .groupMembers(_, group) = r else { throw UnexpectedRecording(cmd: cmd) }
        _ = openChat(groupChat(group.groupInfo))
        ChatModel.shared.groupMembers = group.members.map { GMember($0) }
        ChatModel.shared.populateGroupMembersIndexes()
        return (group.groupInfo, group.members)
    }

    @MainActor
    private func loadAddress() throws {
        let r: ChatResponse1 = try recordedResult("/_show_address 1")
        guard case let .userContactLink(_, contactLink) = r else { throw UnexpectedRecording(cmd: "/_show_address 1") }
        ChatModel.shared.userAddress = contactLink
    }

    @MainActor
    private func replaceMembers(_ members: [GroupMember]) {
        for m in members {
            if let i = ChatModel.shared.groupMembersIndexes[m.groupMemberId] { ChatModel.shared.groupMembers[i].wrapped = m }
        }
    }

    @MainActor
    private func openContact(_ cmd: String) throws -> (Chat, Contact) {
        let r: ChatResponse0 = try recordedResult(cmd)
        guard case let .apiChat(_, chatData, _) = r, case let .direct(contact) = chatData.chatInfo else { throw UnexpectedRecording(cmd: cmd) }
        return (openChat(Chat(chatData)), contact)
    }

    @MainActor
    private func openChat(_ chat: Chat) -> Chat {
        ChatModel.shared.addChat(chat)
        ChatModel.shared.chatId = chat.id
        return chat
    }

    private func groupChat(_ groupInfo: GroupInfo) -> Chat {
        Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: nil))
    }

    @MainActor
    private func groupInfo(_ id: ChatId) -> GroupInfo? {
        if case let .group(groupInfo, _)? = ChatModel.shared.getChat(id)?.chatInfo { groupInfo } else { nil }
    }

    private func blockedMembers(_ cmd: String) throws -> [GroupMember] {
        let r: ChatResponse2 = try recordedResult(cmd)
        guard case let .membersBlockedForAllUser(_, _, members, _) = r else { throw UnexpectedRecording(cmd: cmd) }
        return members
    }

    private func member(_ members: [GroupMember], _ name: String) throws -> GroupMember {
        try XCTUnwrap(members.first { $0.displayName == name }, "no member \(name)")
    }

    @MainActor
    private func awaitMember(_ name: String, _ condition: @escaping (GroupMember) -> Bool) async throws {
        try await waitFor { ChatModel.shared.groupMembers.contains { $0.wrapped.displayName == name && condition($0.wrapped) } }
    }

    @MainActor
    private func sheet<V>(_ type: V.Type, file: StaticString = #filePath, line: UInt = #line) async throws -> V {
        var found: V?
        try? await waitFor {
            found = self.sheetContent(V.self)
            return found != nil
        }
        return try XCTUnwrap(found, "\(V.self) not presented", file: file, line: line)
    }

    @MainActor
    private func sheetContent<V>(_ type: V.Type) -> V? {
        guard var value = (presented() as? HostingRoot)?.anyRootView else { return nil }
        for _ in 0..<10 {
            if let found = value as? V { return found }
            guard let content = Mirror(reflecting: value).descendant("content") else { return nil }
            value = content
        }
        return nil
    }

    @MainActor
    private func withActionSheetHost(_ test: (ActionSheetHostState) async throws -> Void) async throws {
        let state = ActionSheetHostState()
        let scene = try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first)
        let keyWindow = scene.windows.first { $0.isKeyWindow }
        let window = UIWindow(windowScene: scene)
        window.rootViewController = UIHostingController(rootView: ActionSheetHost(state: state))
        window.makeKeyAndVisible()
        defer {
            window.isHidden = true
            keyWindow?.makeKey()
        }
        try await Task.sleep(nanoseconds: 100_000_000)
        try await test(state)
    }
}

private struct UnexpectedRecording: Error {
    var cmd: String
}

@MainActor
private protocol HostingRoot {
    var anyRootView: Any { get }
}

extension UIHostingController: HostingRoot {
    fileprivate var anyRootView: Any { rootView }
}

private final class ActionSheetHostState: ObservableObject {
    @Published var sheet: SomeActionSheet?
}

private struct ActionSheetHost: View {
    @ObservedObject var state: ActionSheetHostState

    var body: some View {
        Color.clear.actionSheet(item: $state.sheet) { $0.actionSheet }
    }
}
