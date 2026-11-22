import XCTest
@testable import SimpleX

final class ConnectionLinkTests: XCTestCase {
    func testSlashAfterSchemeIsOptional() throws {
        let link = "c#grcfG3ulVI4Sh6ow33qBsmSk7uEy3gRSl2KkJ5ER6tA?h=smp18.simplex.im"
        XCTAssertEqual(connectionLinkPath(try XCTUnwrap(URL(string: "simplex:" + link))), "/c")
        XCTAssertEqual(connectionLinkPath(try XCTUnwrap(URL(string: "simplex:/" + link))), "/c")
    }
}
