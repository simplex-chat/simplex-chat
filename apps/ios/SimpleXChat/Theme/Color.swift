//
//  Color.swift
//  SimpleX (iOS)
//
//  Created by Avently on 05.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

let Purple200 = Color(0xFFBB86FC)
let Purple500 = Color(0xFF6200EE)
let Purple700 = Color(0xFF3700B3)
let Teal200 = Color(0xFF03DAC5)
let Gray = Color(0x22222222)
let Indigo = Color(0xFF9966FF)
let SimplexBlue = Color(0, 136, 255, a: 255)
let SimplexGreen = Color(77, 218, 103, a: 255)
let SecretColor = Color(0x40808080)
let LightGray = Color(241, 242, 246, a: 255)
let DarkGray = Color(43, 44, 46, a: 255)
let HighOrLowlight = Color(139, 135, 134, a: 255)
let MessagePreviewDark = Color(179, 175, 174, a: 255)
let MessagePreviewLight = Color(49, 45, 44, a: 255)
let ToolbarLight = Color(220, 220, 220, a: 12)
let ToolbarDark = Color(80, 80, 80, a: 12)
let SettingsSecondaryLight = Color(200, 196, 195, a: 90)
let GroupDark = Color(80, 80, 80, a: 60)
let IncomingCallLight = Color(239, 237, 236, a: 255)
let WarningOrange = Color(255, 127, 0, a: 255)
let WarningYellow = Color(255, 192, 0, a: 255)
let FileLight = Color(183, 190, 199, a: 255)
let FileDark = Color(101, 101, 106, a: 255)

public let LightThemeBackgroundColor = Color(0xfff2f2f7)

extension Color {
    public init(_ argb: Int64) {
        let a = Double((argb & 0xFF000000) >> 24) / 255.0
        let r = Double((argb & 0xFF0000) >> 16) / 255.0
        let g = Double((argb & 0xFF00) >> 8) / 255.0
        let b = Double((argb & 0xFF)) / 255.0
        self.init(.sRGB, red: r, green: g, blue: b, opacity: a)
    }

    public init(_ r: Int, _ g: Int, _ b: Int, a: Int) {
        self.init(.sRGB, red: Double(r) / 255.0, green: Double(g) / 255.0, blue: Double(b) / 255.0, opacity: Double(a) / 255.0)
    }

    public func toReadableHex() -> String {
        let uiColor: UIColor = .init(self)
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        uiColor.getRed(&r, green: &g, blue: &b, alpha: &a)
        return String(format: "#%02x%02x%02x%02x",
                      Int((a * 255).rounded()),
                      Int((r * 255).rounded()),
                      Int((g * 255).rounded()),
                      Int((b * 255).rounded())
        )
    }

    public func darker(_ factor: CGFloat = 0.1) -> Color {
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        UIColor(self).getRed(&r, green: &g, blue: &b, alpha: &a)
        return Color(.sRGB, red: max(r * (1 - factor), 0), green: max(g * (1 - factor), 0), blue: max(b * (1 - factor), 0), opacity: a)
    }

    public func lighter(_ factor: CGFloat = 0.1) -> Color {
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        UIColor(self).getRed(&r, green: &g, blue: &b, alpha: &a)
        return Color(.sRGB, red: min(r * (1 + factor), 1), green: min(g * (1 + factor), 1), blue: min(b * (1 + factor), 1), opacity: a)
    }

    public func asGroupedBackground(_ mode: DefaultThemeMode) -> Color {
        let uiColor: UIColor = .init(self)
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        uiColor.getRed(&r, green: &g, blue: &b, alpha: &a)
        return mode == DefaultThemeMode.light
        ? Color(.sRGB, red: max(0, r - 0.052), green: max(0, g - 0.051), blue: max(0, b - 0.032), opacity: a)
        : Color(.sRGB, red: min(1, r + 0.11), green: min(1, g + 0.11), blue: min(1, b + 0.115), opacity: a)
    }

}

extension String {
    func colorFromReadableHex() -> Color {
        // https://stackoverflow.com/a/56874327
        let hex = self.trimmingCharacters(in: ["#", " "])
        var int: UInt64 = 0
        Scanner(string: hex).scanHexInt64(&int)
        let a, r, g, b: UInt64
        switch hex.count {
        case 3: // RGB (12-bit)
            (a, r, g, b) = (255, (int >> 8) * 17, (int >> 4 & 0xF) * 17, (int & 0xF) * 17)
        case 6: // RGB (24-bit)
            (a, r, g, b) = (255, int >> 16, int >> 8 & 0xFF, int & 0xFF)
        case 8: // ARGB (32-bit)
            (a, r, g, b) = (int >> 24, int >> 16 & 0xFF, int >> 8 & 0xFF, int & 0xFF)
        default:
            (a, r, g, b) = (1, 1, 1, 0)
        }

        return Color(
            .sRGB,
            red: Double(r) / 255,
            green: Double(g) / 255,
            blue:  Double(b) / 255,
            opacity: Double(a) / 255
        )
    }
}
