//
//  Color.swift
//  SimpleX (iOS)
//
//  Created by Avently on 05.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

//let Purple200 = Color(0xFFBB86FC)
//let Purple500 = Color(0xFF6200EE)
//let Purple700 = Color(0xFF3700B3)
//let Teal200 = Color(0xFF03DAC5)
//let Gray = Color(0x22222222)
//let Indigo = Color(0xFF9966FF)
let SimplexBlue = Color(0, 136, 255, a: 255)
//let SimplexGreen = Color(77, 218, 103, a: 255)
//let SecretColor = Color(0x40808080)
let LightGray = Color(241, 242, 246, a: 255)
let DarkGray = Color(43, 44, 46, a: 255)
let HighOrLowlight = Color(139, 135, 134, a: 255)
//let MessagePreviewDark = Color(179, 175, 174, a: 255)
//let MessagePreviewLight = Color(49, 45, 44, a: 255)
//let ToolbarLight = Color(220, 220, 220, a: 12)
//let ToolbarDark = Color(80, 80, 80, a: 12)
//let SettingsSecondaryLight = Color(200, 196, 195, a: 90)
//let GroupDark = Color(80, 80, 80, a: 60)
//let IncomingCallLight = Color(239, 237, 236, a: 255)
//let WarningOrange = Color(255, 127, 0, a: 255)
//let WarningYellow = Color(255, 192, 0, a: 255)
//let FileLight = Color(183, 190, 199, a: 255)
//let FileDark = Color(101, 101, 106, a: 255)

private let displayP3ColorSpace = CGColorSpace(name: CGColorSpace.displayP3)!
private let sRGBColorSpace = CGColorSpace(name: CGColorSpace.sRGB)!

private func srgbToP3(red r: Double, green g: Double, blue b: Double, opacity a: Double) -> Color {
    let srgb = UIColor(red: r, green: g, blue: b, alpha: a)
    if let p3 = srgb.cgColor.converted(to: displayP3ColorSpace, intent: .defaultIntent, options: nil) {
        return Color(p3)
    }
    return Color(.sRGB, red: r, green: g, blue: b, opacity: a)
}

/// Create a Display P3 Color from oklch components. H in degrees.
func oklch(_ L: Double, _ C: Double, _ H: Double, alpha: Double = 1.0) -> Color {
    let hRad = H * .pi / 180.0
    let a = C * cos(hRad)
    let b = C * sin(hRad)
    // oklab → LMS (Ottosson 2021)
    let l_ = L + 0.3963377774 * a + 0.2158037573 * b
    let m_ = L - 0.1055613458 * a - 0.0638541728 * b
    let s_ = L - 0.0894841775 * a - 1.2914855480 * b
    let l = l_ * l_ * l_
    let m = m_ * m_ * m_
    let s = s_ * s_ * s_
    // LMS → linear Display P3 (direct, no sRGB clamping)
    let lr =  3.1281105148 * l - 2.2570749853 * m + 0.1293047593 * s
    let lg = -1.0911282009 * l + 2.4132668169 * m - 0.3221681599 * s
    let lb = -0.0260136845 * l - 0.5080276339 * m + 1.5333166364 * s
    // linear P3 → gamma-encoded P3 (same transfer function as sRGB)
    func gammaEncode(_ x: Double) -> Double { x >= 0.0031308 ? 1.055 * pow(x, 1.0 / 2.4) - 0.055 : 12.92 * x }
    return Color(.displayP3,
        red: gammaEncode(min(max(lr, 0), 1)),
        green: gammaEncode(min(max(lg, 0), 1)),
        blue: gammaEncode(min(max(lb, 0), 1)),
        opacity: alpha
    )
}

extension Color {
    public init(_ argb: Int64) {
        let a = Double((argb & 0xFF000000) >> 24) / 255.0
        let r = Double((argb & 0xFF0000) >> 16) / 255.0
        let g = Double((argb & 0xFF00) >> 8) / 255.0
        let b = Double((argb & 0xFF)) / 255.0
        self = srgbToP3(red: r, green: g, blue: b, opacity: a)
    }

    public init(_ r: Int, _ g: Int, _ b: Int, a: Int) {
        self = srgbToP3(red: Double(r) / 255.0, green: Double(g) / 255.0, blue: Double(b) / 255.0, opacity: Double(a) / 255.0)
    }

    public func toReadableHex() -> String {
        let uiColor: UIColor = .init(self)
        // Convert to sRGB explicitly since internal colors may be P3
        if let srgbCG = uiColor.cgColor.converted(to: sRGBColorSpace, intent: .defaultIntent, options: nil),
           let comps = srgbCG.components, comps.count >= 3 {
            let r = min(1, max(0, comps[0]))
            let g = min(1, max(0, comps[1]))
            let b = min(1, max(0, comps[2]))
            let a = comps.count >= 4 ? min(1, max(0, comps[3])) : 1.0
            return String(format: "#%02x%02x%02x%02x",
                          Int((a * 255).rounded()),
                          Int((r * 255).rounded()),
                          Int((g * 255).rounded()),
                          Int((b * 255).rounded()))
        }
        // Fallback
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        uiColor.getRed(&r, green: &g, blue: &b, alpha: &a)
        r = min(1, max(0, r)); g = min(1, max(0, g)); b = min(1, max(0, b)); a = min(1, max(0, a))
        return String(format: "#%02x%02x%02x%02x",
                      Int((a * 255).rounded()),
                      Int((r * 255).rounded()),
                      Int((g * 255).rounded()),
                      Int((b * 255).rounded()))
    }

    public func toHTMLHex() -> String {
        let uiColor: UIColor = .init(self)
        if let srgbCG = uiColor.cgColor.converted(to: sRGBColorSpace, intent: .defaultIntent, options: nil),
           let comps = srgbCG.components, comps.count >= 3 {
            let r = min(1, max(0, comps[0]))
            let g = min(1, max(0, comps[1]))
            let b = min(1, max(0, comps[2]))
            let a = comps.count >= 4 ? min(1, max(0, comps[3])) : 1.0
            return String(format: "#%02x%02x%02x%02x",
                          Int((r * 255).rounded()),
                          Int((g * 255).rounded()),
                          Int((b * 255).rounded()),
                          Int((a * 255).rounded()))
        }
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        uiColor.getRed(&r, green: &g, blue: &b, alpha: &a)
        r = min(1, max(0, r)); g = min(1, max(0, g)); b = min(1, max(0, b)); a = min(1, max(0, a))
        return String(format: "#%02x%02x%02x%02x",
                      Int((r * 255).rounded()),
                      Int((g * 255).rounded()),
                      Int((b * 255).rounded()),
                      Int((a * 255).rounded()))
    }

    public func darker(_ factor: CGFloat = 0.1) -> Color {
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        UIColor(self).getRed(&r, green: &g, blue: &b, alpha: &a)
        return Color(.displayP3, red: max(r * (1 - factor), 0), green: max(g * (1 - factor), 0), blue: max(b * (1 - factor), 0), opacity: a)
    }

    public func lighter(_ factor: CGFloat = 0.1) -> Color {
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        UIColor(self).getRed(&r, green: &g, blue: &b, alpha: &a)
        return Color(.displayP3, red: min(r * (1 + factor), 1), green: min(g * (1 + factor), 1), blue: min(b * (1 + factor), 1), opacity: a)
    }

    public func asGroupedBackground(_ mode: DefaultThemeMode) -> Color {
        let uiColor: UIColor = .init(self)
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        uiColor.getRed(&r, green: &g, blue: &b, alpha: &a)
        return mode == DefaultThemeMode.light
        ? Color(.displayP3, red: max(0, r - 0.052), green: max(0, g - 0.051), blue: max(0, b - 0.032), opacity: a)
        : Color(.displayP3, red: min(1, r + 0.11), green: min(1, g + 0.11), blue: min(1, b + 0.115), opacity: a)
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

        return srgbToP3(
            red: Double(r) / 255,
            green: Double(g) / 255,
            blue: Double(b) / 255,
            opacity: Double(a) / 255
        )
    }
}
