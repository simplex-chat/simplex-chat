//
//  ArrayMap.swift
//  SimpleX (iOS)
//
//  Created by User on 19/08/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

public struct ArrayMap<Element: Identifiable>: ExpressibleByArrayLiteral {
    private(set) var elements: [Element]
    private(set) var hashMap: [Element.ID: Int]

    public init(_ elements: [Element] = []) {
        self.elements = elements
        self.hashMap = elements
            .enumerated()
            .reduce(into: [:]) { dict, enumerated in
                dict[enumerated.element.id] = enumerated.offset
            }
    }

    public init(arrayLiteral elements: Element...) {
        self.elements = elements
        self.hashMap = elements
            .enumerated()
            .reduce(into: [:]) { dict, enumerated in
                dict[enumerated.element.id] = enumerated.offset
            }
    }

    public subscript(index: Int) -> Element {
        get { elements[index] }
        set {
            let oldElement = elements[index]
            elements[index] = newValue
            hashMap[oldElement.id] = nil
            hashMap[newValue.id] = index
        }
    }

    public subscript(id: Element.ID) -> Element? {
        get {
            hashMap[id].map { elements[$0] }
        }
    }

    public mutating func append(_ element: Element) {
        elements.append(element)
        updateMap(in: [elements.count - 1])
    }

    public mutating func insert(_ element: Element, at index: Int) {
        elements.insert(element, at: index)
        hashMap[element.id] = index
        updateMap(in: index..<elements.count)
    }

    public mutating func move(_ id: Element.ID, to index: Int = 0) {
        if let offset = hashMap[id] {
            elements.move(fromOffsets: [offset], toOffset: index)
            updateMap(
                in: min(index, offset)...min(max(index, offset), elements.count - 1)
            )
        }
    }

    public mutating func remove(at index: Int) {
        hashMap[elements[index].id] = nil
        elements.remove(at: index)
        updateMap(in: index..<elements.count)
    }

    public mutating func remove(id: Element.ID) {
        if let index = hashMap[id] { remove(at: index) }
    }

    private mutating func updateMap<S: Sequence>(in sequence: S) where S.Element == Int {
        for i in sequence { hashMap[elements[i].id] = i }
    }
}
