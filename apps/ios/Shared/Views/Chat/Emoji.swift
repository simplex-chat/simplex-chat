//
//  Emoji.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

private func isSimpleEmoji(_ c: Character) -> Bool {
    guard let firstScalar = c.unicodeScalars.first else { return false }
    return firstScalar.properties.isEmoji && firstScalar.value > 0x238C
}

private func isCombinedIntoEmoji(_ c: Character) -> Bool {
    c.unicodeScalars.count > 1 && c.unicodeScalars.first?.properties.isEmoji ?? false
}

func isEmoji(_ c: Character) -> Bool {
    isSimpleEmoji(c) || isCombinedIntoEmoji(c)
}

func isShortEmoji(_ str: String) -> Bool {
    let s = str.trimmingCharacters(in: .whitespaces)
    return s.count <= 3 && s.allSatisfy(isEmoji)
}
