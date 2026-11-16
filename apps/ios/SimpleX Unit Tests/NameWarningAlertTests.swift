import XCTest
import SimpleXChat

final class NameWarningAlertTests: XCTestCase {
    private let expiredAt = badgeDateText(Date(timeIntervalSince1970: 1767268800))
    private let graceUntil = badgeDateText(Date(timeIntervalSince1970: 4070952000))
    private let price = "$20 for 2 years"

    private func fixture(_ name: String) throws -> APIResult<ChatResponse1> {
        decodeAPIResult(try Data(contentsOf: URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("names/\(name).json")))
    }

    private func plan(_ name: String) throws -> (SimplexNameInfo?, ConnectionPlan) {
        let r = try fixture(name)
        let plan: (SimplexNameInfo?, ConnectionPlan)? = if case let .result(.connectionPlan(_, _, planSimplexName, _, connectionPlan)) = r { (planSimplexName, connectionPlan) } else { nil }
        return try XCTUnwrap(plan, r.responseType)
    }

    private func planAlert(_ name: String) throws -> NameWarningAlert {
        let (planSimplexName, connectionPlan) = try plan(name)
        return try XCTUnwrap(planNameWarningAlert(planSimplexName, connectionPlan))
    }

    private func errorAlert(_ name: String) throws -> NameWarningAlert {
        let r = try fixture(name)
        let domainWarning: (SimplexDomain, NameWarning)? = if case let .error(.error(.simplexDomainNotReady(domain, .nameWarning(warning)))) = r { (domain, warning) } else { nil }
        let (domain, warning) = try XCTUnwrap(domainWarning, r.responseType)
        return nameWarningAlert(domain: domain, warning: warning, own: false, chat: nil)
    }

    private func assertAlert(_ alert: NameWarningAlert, _ title: String, _ message: String, _ action: NameWarningAction?, _ chat: String?) {
        XCTAssertEqual(alert.title, title)
        XCTAssertEqual(alert.message, message)
        XCTAssertEqual(alert.action, action)
        XCTAssertEqual(alert.chat?.localDisplayName, chat)
    }

    func testKnownChatNameExpired() throws {
        assertAlert(try planAlert("plan-known-expired"), "Name expired", "alice.simplex expired on \(expiredAt). Its owner can renew it until \(graceUntil).", nil, "alice")
    }

    func testOwnNameExpired() throws {
        assertAlert(try planAlert("plan-own-expired"), "Your name has expired", "Your name alice.simplex expired on \(expiredAt). Renew it before \(graceUntil).", .renew, nil)
    }

    func testKnownChatNameAvailable() throws {
        assertAlert(try planAlert("plan-known-available"), "Name no longer registered", "alice.simplex is available for registration for \(price).", .register, "alice")
    }

    func testOwnNameAvailable() throws {
        assertAlert(try planAlert("plan-own-available"), "Your name has expired", "Your name alice.simplex is no longer registered. It is available for registration for \(price).", .reRegister, nil)
    }

    func testKnownChatNameReserved() throws {
        assertAlert(try planAlert("plan-known-reserved"), "Name not registered", "alice.simplex is reserved for community. Please contact SimpleX Chat if you are interested in using it.", .connectToTeam, "alice")
    }

    func testKnownChatNameMoved() throws {
        let (planSimplexName, connectionPlan) = try plan("plan-known-moved")
        XCTAssertNil(planNameWarningAlert(planSimplexName, connectionPlan))
        XCTAssertEqual(connectionPlan.localChats.map(\.localDisplayName), ["alice"])
    }

    func testNameAvailable() throws {
        assertAlert(try errorAlert("error-available"), "Name not registered", "nobody.simplex is available for registration for \(price).", .register, nil)
    }

    func testNameNotRegistered() throws {
        assertAlert(try errorAlert("error-not-registered"), "Name not registered", "This SimpleX name is not registered. Please check the name.", nil, nil)
    }
}
