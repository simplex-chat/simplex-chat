//
//  Tests_iOS.swift
//  Tests iOS
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import XCTest

final class Tests_iOS: XCTestCase {

    private struct Element: Identifiable, ExpressibleByStringLiteral, Equatable {
        var id: String
        init(stringLiteral: String) { id = stringLiteral }
    }

    private var arrayMap: ArrayMap<Element> { ["a", "b", "c", "d"] }

    func testArrayMapAccess() {
        XCTAssertEqual(arrayMap["a"], "a")
        XCTAssertEqual(arrayMap[1], "b")
    }

    func testArrayMapAppend() {
        var am = arrayMap
        am.append("e")
        XCTAssertEqual(am.elements, ["a", "b", "c", "d", "e"])
        XCTAssertEqual(am.hashMap, ["a": 0, "b": 1, "c": 2, "d": 3, "e": 4])
    }

    func testArrayMapInsert() {
        var am = arrayMap
        am.insert("c1", at: 3)
        XCTAssertEqual(am.elements, ["a", "b", "c", "c1", "d"])
        XCTAssertEqual(am.hashMap, ["a": 0, "b": 1, "c": 2, "c1": 3, "d": 4])
    }

    func testArrayMapMove() {
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

    func testArrayMapRemoveById() {
        var am = arrayMap
        am.remove(id: "b")
        XCTAssertEqual(am.elements, ["a", "c", "d"])
        XCTAssertEqual(am.hashMap, ["a": 0, "c": 1, "d": 2])
    }

    func testArrayMapRemoveByIndex() {
        var am = arrayMap
        am.remove(at: 1)
        XCTAssertEqual(am.elements, ["a", "c", "d"])
        XCTAssertEqual(am.hashMap, ["a": 0, "c": 1, "d": 2])
    }
}
