//
//  Tests_iOS.swift
//  Tests iOS
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import XCTest

class Tests_iOS: XCTestCase {

    override func setUpWithError() throws {
        // Put setup code here. This method is called before the invocation of each test method in the class.

        // In UI tests it is usually best to stop immediately when a failure occurs.
        continueAfterFailure = false

        // In UI tests it’s important to set the initial state - such as interface orientation - required for your tests before they run. The setUp method is a good place to do this.
    }

    override func tearDownWithError() throws {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
    }

    func testExample() throws {
        // UI tests must launch the application that they test.
        let app = XCUIApplication()
        app.launch()

        // Use recording to get started writing UI tests.
        // Use XCTAssert and related functions to verify your tests produce the correct results.
    }

    func testLaunchPerformance() throws {
        if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 7.0, *) {
            // This measures how long it takes to launch your application.
            measure(metrics: [XCTApplicationLaunchMetric()]) {
                XCUIApplication().launch()
            }
        }
    }
}

class ArrayMapTests: XCTestCase {
    private struct Element: Identifiable, ExpressibleByStringLiteral, Equatable {
        var id: String
        init(stringLiteral: String) { id = stringLiteral }
    }

    private var arrayMap: ArrayMap<Element> { ["a", "b", "c", "d"] }

    func testAccess() {
        XCTAssertEqual(arrayMap["a"], "a")
        XCTAssertEqual(arrayMap[1], "b")
    }

    func testAppend() {
        var am = arrayMap
        am.append("e")
        XCTAssertEqual(am.elements, ["a", "b", "c", "d", "e"])
        XCTAssertEqual(am.hashMap, ["a": 0, "b": 1, "c": 2, "d": 3, "e": 4])
    }

    func testInsert() {
        var am = arrayMap
        am.insert("c1", at: 3)
        XCTAssertEqual(am.elements, ["a", "b", "c", "c1", "d"])
        XCTAssertEqual(am.hashMap, ["a": 0, "b": 1, "c": 2, "c1": 3, "d": 4])
    }

    func testMove() {
        var am = arrayMap
        // Move to start
        am.move("c")
        XCTAssertEqual(am.elements, ["c", "a", "b", "d"])
        XCTAssertEqual(am.hashMap, ["c": 0, "a": 1, "b": 2, "d": 3])
        // Move to the end
        am.move("c", to: am.elements.count)
        XCTAssertEqual(am.elements, ["a", "b", "d", "c"])
        XCTAssertEqual(am.hashMap, ["a": 0, "b": 1, "d": 2, "c": 3])
        // Move to the middle
        am.move("c", to: 2)
        XCTAssertEqual(am.elements, ["a", "b", "c", "d"])
        XCTAssertEqual(am.hashMap, ["a": 0, "b": 1, "c": 2, "d": 3])
    }

    func testRemoveById() {
        var am = arrayMap
        am.remove(id: "b")
        XCTAssertEqual(am.elements, ["a", "c", "d"])
        XCTAssertEqual(am.hashMap, ["a": 0, "c": 1, "d": 2])
    }

    func testRemoveByIndex() {
        var am = arrayMap
        am.remove(at: 1)
        XCTAssertEqual(am.elements, ["a", "c", "d"])
        XCTAssertEqual(am.hashMap, ["a": 0, "c": 1, "d": 2])
    }
}
