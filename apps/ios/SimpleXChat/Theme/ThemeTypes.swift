//
//  Theme.swift
//  SimpleX (iOS)
//
//  Created by Avently on 03.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

public enum DefaultTheme: String, Codable, Equatable {
    case LIGHT
    case DARK
    case SIMPLEX
    case BLACK

    public static let SYSTEM_THEME_NAME: String = "SYSTEM"

    public var themeName: String { self.rawValue }

    public var mode: DefaultThemeMode {
        self == .LIGHT
        ? DefaultThemeMode.light
        : DefaultThemeMode.dark
    }

    public func hasChangedAnyColor(_ overrides: ThemeOverrides?) -> Bool {
        if let overrides {
            overrides.colors != ThemeColors() || (overrides.wallpaper != nil && (overrides.wallpaper?.background != nil || overrides.wallpaper?.tint != nil))
        } else {
            false
        }
    }
}

public enum DefaultThemeMode: String, Codable {
    case light
    case dark
}

public class Colors: ObservableObject, NSCopying, Equatable {
    @Published public var primary: Color
    @Published public var primaryVariant: Color
    @Published public var secondary: Color
    @Published public var secondaryVariant: Color
    @Published public var background: Color
    @Published public var surface: Color
    @Published public var error: Color
    @Published public var onBackground: Color
    @Published public var onSurface: Color
    @Published public var isLight: Bool

    public init(primary: Color, primaryVariant: Color, secondary: Color, secondaryVariant: Color, background: Color, surface: Color, error: Color, onBackground: Color, onSurface: Color, isLight: Bool) {
        self.primary = primary
        self.primaryVariant = primaryVariant
        self.secondary = secondary
        self.secondaryVariant = secondaryVariant
        self.background = background
        self.surface = surface
        self.error = error
        self.onBackground = onBackground
        self.onSurface = onSurface
        self.isLight = isLight
    }

    public static func == (lhs: Colors, rhs: Colors) -> Bool {
        lhs.primary == rhs.primary &&
        lhs.primaryVariant == rhs.primaryVariant &&
        lhs.secondary == rhs.secondary &&
        lhs.secondaryVariant == rhs.secondaryVariant &&
        lhs.background == rhs.background &&
        lhs.surface == rhs.surface &&
        lhs.error == rhs.error &&
        lhs.onBackground == rhs.onBackground &&
        lhs.onSurface == rhs.onSurface &&
        lhs.isLight == rhs.isLight
    }

    public func copy(with zone: NSZone? = nil) -> Any {
        Colors(primary: self.primary, primaryVariant: self.primaryVariant, secondary: self.secondary, secondaryVariant: self.secondaryVariant, background: self.background, surface: self.surface, error: self.error, onBackground: self.onBackground, onSurface: self.onSurface, isLight: self.isLight)
    }

    public func clone() -> Colors { copy() as! Colors }
}

public class AppColors: ObservableObject, NSCopying, Equatable {
    @Published public var title: Color
    @Published public var primaryVariant2: Color
    @Published public var sentMessage: Color
    @Published public var sentQuote: Color
    @Published public var receivedMessage: Color
    @Published public var receivedQuote: Color

    public init(title: Color, primaryVariant2: Color, sentMessage: Color, sentQuote: Color, receivedMessage: Color, receivedQuote: Color) {
        self.title = title
        self.primaryVariant2 = primaryVariant2
        self.sentMessage = sentMessage
        self.sentQuote = sentQuote
        self.receivedMessage = receivedMessage
        self.receivedQuote = receivedQuote
    }

    public static func == (lhs: AppColors, rhs: AppColors) -> Bool {
        lhs.title == rhs.title &&
        lhs.primaryVariant2 == rhs.primaryVariant2 &&
        lhs.sentMessage == rhs.sentMessage &&
        lhs.sentQuote == rhs.sentQuote &&
        lhs.receivedQuote == rhs.receivedMessage &&
        lhs.receivedQuote == rhs.receivedQuote
    }

    public func copy(with zone: NSZone? = nil) -> Any {
        AppColors(title: self.title, primaryVariant2: self.primaryVariant2, sentMessage: self.sentMessage, sentQuote: self.sentQuote, receivedMessage: self.receivedMessage, receivedQuote: self.receivedQuote)
    }

    public func clone() -> AppColors { copy() as! AppColors }

    public func copy(
        title: Color?,
        primaryVariant2: Color?,
        sentMessage: Color?,
        sentQuote: Color?,
        receivedMessage: Color?,
        receivedQuote: Color?
    ) -> AppColors {
        AppColors(
            title: title ?? self.title,
            primaryVariant2: primaryVariant2 ?? self.primaryVariant2,
            sentMessage: sentMessage ?? self.sentMessage,
            sentQuote: sentQuote ?? self.sentQuote,
            receivedMessage: receivedMessage ?? self.receivedMessage,
            receivedQuote: receivedQuote ?? self.receivedQuote
        )
    }
}

public class AppWallpaper: ObservableObject, NSCopying, Equatable {
    public static func == (lhs: AppWallpaper, rhs: AppWallpaper) -> Bool {
        lhs.background == rhs.background &&
        lhs.tint == rhs.tint &&
        lhs.type == rhs.type
    }
    
    @Published public var background: Color? = nil
    @Published public var tint: Color? = nil
    @Published public var type: WallpaperType = WallpaperType.Empty

    public init(background: Color?, tint: Color?, type: WallpaperType) {
        self.background = background
        self.tint = tint
        self.type = type
    }

    public func copy(with zone: NSZone? = nil) -> Any {
        AppWallpaper(background: self.background, tint: self.tint, type: self.type)
    }

    public func clone() -> AppWallpaper { copy() as! AppWallpaper }

    public func copyWithoutDefault(_ background: Color?, _ tint: Color?, _ type: WallpaperType) -> AppWallpaper {
        AppWallpaper(
            background: background,
            tint: tint,
            type: type
        )
    }
}

public enum ThemeColor {
    case PRIMARY
    case PRIMARY_VARIANT
    case SECONDARY
    case SECONDARY_VARIANT
    case BACKGROUND
    case SURFACE
    case TITLE
    case SENT_MESSAGE
    case SENT_QUOTE
    case RECEIVED_MESSAGE
    case RECEIVED_QUOTE
    case PRIMARY_VARIANT2
    case WALLPAPER_BACKGROUND
    case WALLPAPER_TINT

    public func fromColors(_ colors: Colors, _ appColors: AppColors, _ appWallpaper: AppWallpaper) -> Color? {
        switch (self) {
        case .PRIMARY: colors.primary
        case .PRIMARY_VARIANT: colors.primaryVariant
        case .SECONDARY: colors.secondary
        case .SECONDARY_VARIANT: colors.secondaryVariant
        case .BACKGROUND: colors.background
        case .SURFACE: colors.surface
        case .TITLE: appColors.title
        case .PRIMARY_VARIANT2: appColors.primaryVariant2
        case .SENT_MESSAGE: appColors.sentMessage
        case .SENT_QUOTE: appColors.sentQuote
        case .RECEIVED_MESSAGE: appColors.receivedMessage
        case .RECEIVED_QUOTE: appColors.receivedQuote
        case .WALLPAPER_BACKGROUND: appWallpaper.background
        case .WALLPAPER_TINT: appWallpaper.tint
        }
    }

    public var text: LocalizedStringKey {
        switch (self) {
        case .PRIMARY: "Accent"
        case .PRIMARY_VARIANT: "Additional accent"
        case .SECONDARY: "Secondary"
        case .SECONDARY_VARIANT: "Additional secondary"
        case .BACKGROUND: "Background"
        case .SURFACE: "Menus & alerts"
        case .TITLE: "Title"
        case .PRIMARY_VARIANT2: "Additional accent 2"
        case .SENT_MESSAGE: "Sent message"
        case .SENT_QUOTE: "Sent reply"
        case .RECEIVED_MESSAGE: "Received message"
        case .RECEIVED_QUOTE: "Received reply"
        case .WALLPAPER_BACKGROUND: "Wallpaper background"
        case .WALLPAPER_TINT: "Wallpaper accent"
        }
    }
}

public struct ThemeColors: Codable, Equatable{
    public var primary: String? = nil
    public var primaryVariant: String? = nil
    public var secondary: String? = nil
    public var secondaryVariant: String? = nil
    public var background: String? = nil
    public var surface: String? = nil
    public var title: String? = nil
    public var primaryVariant2: String? = nil
    public var sentMessage: String? = nil
    public var sentQuote: String? = nil
    public var receivedMessage: String? = nil
    public var receivedQuote: String? = nil

    public init(primary: String? = nil, primaryVariant: String? = nil, secondary: String? = nil, secondaryVariant: String? = nil, background: String? = nil, surface: String? = nil, title: String? = nil, primaryVariant2: String? = nil, sentMessage: String? = nil, sentQuote: String? = nil, receivedMessage: String? = nil, receivedQuote: String? = nil) {
        self.primary = primary
        self.primaryVariant = primaryVariant
        self.secondary = secondary
        self.secondaryVariant = secondaryVariant
        self.background = background
        self.surface = surface
        self.title = title
        self.primaryVariant2 = primaryVariant2
        self.sentMessage = sentMessage
        self.sentQuote = sentQuote
        self.receivedMessage = receivedMessage
        self.receivedQuote = receivedQuote
    }

    public enum CodingKeys: String, CodingKey, CaseIterable {
        case primary = "accent"
        case primaryVariant = "accentVariant"
        case secondary
        case secondaryVariant
        case background
        case surface = "menus"
        case title
        case primaryVariant2 = "accentVariant2"
        case sentMessage
        case sentQuote = "sentReply"
        case receivedMessage
        case receivedQuote = "receivedReply"
    }

    public static func from(sentMessage: String, sentQuote: String, receivedMessage: String, receivedQuote: String) -> ThemeColors {
        var c = ThemeColors()
        c.sentMessage = sentMessage
        c.sentQuote = sentQuote
        c.receivedMessage = receivedMessage
        c.receivedQuote = receivedQuote
        return c
    }

    public static func from(_ colors: Colors, _ appColors: AppColors) -> ThemeColors {
        ThemeColors(
            primary: colors.primary.toReadableHex(),
            primaryVariant: colors.primaryVariant.toReadableHex(),
            secondary: colors.secondary.toReadableHex(),
            secondaryVariant: colors.secondaryVariant.toReadableHex(),
            background: colors.background.toReadableHex(),
            surface: colors.surface.toReadableHex(),
            title: appColors.title.toReadableHex(),
            primaryVariant2: appColors.primaryVariant2.toReadableHex(),
            sentMessage: appColors.sentMessage.toReadableHex(),
            sentQuote: appColors.sentQuote.toReadableHex(),
            receivedMessage: appColors.receivedMessage.toReadableHex(),
            receivedQuote: appColors.receivedQuote.toReadableHex()
        )
    }
}

public struct ThemeWallpaper: Codable, Equatable {
    public var preset: String?
    public var scale: Float?
    public var scaleType: WallpaperScaleType?
    public var background: String?
    public var tint: String?
    public var image: String?
    public var imageFile: String?

    public init(preset: String? = nil, scale: Float? = nil, scaleType: WallpaperScaleType? = nil, background: String? = nil, tint: String? = nil, image: String? = nil, imageFile: String? = nil) {
        self.preset = preset
        self.scale = scale
        self.scaleType = scaleType
        self.background = background
        self.tint = tint
        self.image = image
        self.imageFile = imageFile
    }

    public enum CodingKeys: String, CodingKey, CaseIterable {
        case preset
        case scale
        case scaleType
        case background
        case tint
        case image
        case imageFile
    }

    public func toAppWallpaper() -> AppWallpaper {
        AppWallpaper (
            background: background?.colorFromReadableHex(),
            tint: tint?.colorFromReadableHex(),
            type: WallpaperType.from(self) ?? WallpaperType.Empty
        )
    }

    public func withFilledWallpaperPath() -> ThemeWallpaper {
        let aw = toAppWallpaper()
        let type = aw.type
        let preset: String? = if case let WallpaperType.Preset(filename, _) = type { filename } else { nil }
        let scale: Float? = if scale == nil { nil } else {
            if case let WallpaperType.Preset(_, scale) = type {
                scale
            } else if case let WallpaperType.Image(_, scale, _) = type {
                scale
            } else {
                nil
            }
        }
        let scaleType: WallpaperScaleType? = if scaleType == nil { nil } else if case let WallpaperType.Image(_, _, scaleType) = type { scaleType } else { nil }
        let imageFile: String? = if case let WallpaperType.Image(filename, _, _) = type { filename } else { nil }
        return ThemeWallpaper (
            preset: preset,
            scale: scale,
            scaleType: scaleType,
            background: aw.background?.toReadableHex(),
            tint: aw.tint?.toReadableHex(),
            image: nil,
            imageFile: imageFile
        )
    }

    public static func from(_ type: WallpaperType, _ background: String?, _ tint: String?) -> ThemeWallpaper {
        let preset: String? = if case let WallpaperType.Preset(filename, _) = type { filename } else { nil }
        let scale: Float? = if case let WallpaperType.Preset(_, scale) = type { scale } else if case let WallpaperType.Image(_, scale, _) = type { scale } else { nil }
        let scaleType: WallpaperScaleType? = if case let WallpaperType.Image(_, _, scaleType) = type  { scaleType } else { nil }
        let imageFile: String? = if case let WallpaperType.Image(filename, _, _) = type { filename } else { nil }
        return ThemeWallpaper(
            preset: preset,
            scale: scale,
            scaleType: scaleType,
            background: background,
            tint: tint,
            image: nil,
            imageFile: imageFile
        )
    }
}

/// If you add new properties, make sure they serialized to YAML correctly, see:
/// encodeThemeOverrides()
public struct ThemeOverrides: Codable, Equatable {
    public var themeId: String = UUID().uuidString
    public var base: DefaultTheme
    public var colors: ThemeColors = ThemeColors()
    public var wallpaper: ThemeWallpaper? = nil

    public init(themeId: String = UUID().uuidString, base: DefaultTheme, colors: ThemeColors = ThemeColors(), wallpaper: ThemeWallpaper? = nil) {
        self.themeId = themeId
        self.base = base
        self.colors = colors
        self.wallpaper = wallpaper
    }

    public func isSame(_ type: WallpaperType?, _ themeName: String) -> Bool {
        if base.themeName != themeName {
            return false
        }
        return if let preset = wallpaper?.preset, let type, case let WallpaperType.Preset(filename, _) = type, preset == filename {
            true
        } else if wallpaper?.imageFile != nil, let type, case WallpaperType.Image = type {
            true
        } else if wallpaper?.preset == nil && wallpaper?.imageFile == nil && type == nil {
            true
        } else if wallpaper?.preset == nil && wallpaper?.imageFile == nil, let type, case WallpaperType.Empty = type {
            true
        } else {
            false
        }
    }

    public func withUpdatedColor(_ name: ThemeColor, _ color: String?) -> ThemeOverrides {
        var c = colors
        var w = wallpaper
        switch name {
        case ThemeColor.PRIMARY: c.primary = color
        case ThemeColor.PRIMARY_VARIANT: c.primaryVariant = color
        case ThemeColor.SECONDARY: c.secondary = color
        case ThemeColor.SECONDARY_VARIANT: c.secondaryVariant = color
        case ThemeColor.BACKGROUND: c.background = color
        case ThemeColor.SURFACE: c.surface = color
        case ThemeColor.TITLE: c.title = color
        case ThemeColor.PRIMARY_VARIANT2: c.primaryVariant2 = color
        case ThemeColor.SENT_MESSAGE: c.sentMessage = color
        case ThemeColor.SENT_QUOTE: c.sentQuote = color
        case ThemeColor.RECEIVED_MESSAGE: c.receivedMessage = color
        case ThemeColor.RECEIVED_QUOTE: c.receivedQuote = color
        case ThemeColor.WALLPAPER_BACKGROUND: w?.background = color
        case ThemeColor.WALLPAPER_TINT: w?.tint = color
        }
        return ThemeOverrides(themeId: themeId, base: base, colors: c, wallpaper: w)
    }

    public func toColors(_ base: DefaultTheme, _ perChatTheme: ThemeColors?, _ perUserTheme: ThemeColors?, _ presetWallpaperTheme: ThemeColors?) -> Colors {
        let baseColors = switch base {
            case DefaultTheme.LIGHT: LightColorPalette
            case DefaultTheme.DARK: DarkColorPalette
            case DefaultTheme.SIMPLEX: SimplexColorPalette
            case DefaultTheme.BLACK: BlackColorPalette
        }
        let c = baseColors.clone()
        c.primary = perChatTheme?.primary?.colorFromReadableHex() ?? perUserTheme?.primary?.colorFromReadableHex() ?? colors.primary?.colorFromReadableHex() ?? presetWallpaperTheme?.primary?.colorFromReadableHex() ?? baseColors.primary
        c.primaryVariant = perChatTheme?.primaryVariant?.colorFromReadableHex() ?? perUserTheme?.primaryVariant?.colorFromReadableHex() ?? colors.primaryVariant?.colorFromReadableHex() ?? presetWallpaperTheme?.primaryVariant?.colorFromReadableHex() ?? baseColors.primaryVariant
        c.secondary = perChatTheme?.secondary?.colorFromReadableHex() ?? perUserTheme?.secondary?.colorFromReadableHex() ?? colors.secondary?.colorFromReadableHex() ?? presetWallpaperTheme?.secondary?.colorFromReadableHex() ?? baseColors.secondary
        c.secondaryVariant = perChatTheme?.secondaryVariant?.colorFromReadableHex() ?? perUserTheme?.secondaryVariant?.colorFromReadableHex() ?? colors.secondaryVariant?.colorFromReadableHex() ?? presetWallpaperTheme?.secondaryVariant?.colorFromReadableHex() ?? baseColors.secondaryVariant
        c.background = perChatTheme?.background?.colorFromReadableHex() ?? perUserTheme?.background?.colorFromReadableHex() ?? colors.background?.colorFromReadableHex() ?? presetWallpaperTheme?.background?.colorFromReadableHex() ?? baseColors.background
        c.surface = perChatTheme?.surface?.colorFromReadableHex() ?? perUserTheme?.surface?.colorFromReadableHex() ?? colors.surface?.colorFromReadableHex() ?? presetWallpaperTheme?.surface?.colorFromReadableHex() ?? baseColors.surface
        return c
    }

    public func toAppColors(_ base: DefaultTheme, _ perChatTheme: ThemeColors?, _ perChatWallpaperType: WallpaperType?, _ perUserTheme: ThemeColors?, _ perUserWallpaperType: WallpaperType?, _ presetWallpaperTheme: ThemeColors?) -> AppColors {
        let baseColors = switch base {
        case DefaultTheme.LIGHT: LightColorPaletteApp
        case DefaultTheme.DARK: DarkColorPaletteApp
        case DefaultTheme.SIMPLEX: SimplexColorPaletteApp
        case DefaultTheme.BLACK: BlackColorPaletteApp
        }

        let sentMessageFallback = colors.sentMessage?.colorFromReadableHex() ?? presetWallpaperTheme?.sentMessage?.colorFromReadableHex() ?? baseColors.sentMessage
        let sentQuoteFallback = colors.sentQuote?.colorFromReadableHex() ?? presetWallpaperTheme?.sentQuote?.colorFromReadableHex() ?? baseColors.sentQuote
        let receivedMessageFallback = colors.receivedMessage?.colorFromReadableHex() ?? presetWallpaperTheme?.receivedMessage?.colorFromReadableHex() ?? baseColors.receivedMessage
        let receivedQuoteFallback = colors.receivedQuote?.colorFromReadableHex() ?? presetWallpaperTheme?.receivedQuote?.colorFromReadableHex() ?? baseColors.receivedQuote
        
        let c = baseColors.clone()
        c.title = perChatTheme?.title?.colorFromReadableHex() ?? perUserTheme?.title?.colorFromReadableHex() ?? colors.title?.colorFromReadableHex() ?? presetWallpaperTheme?.title?.colorFromReadableHex() ?? baseColors.title
        c.primaryVariant2 = perChatTheme?.primaryVariant2?.colorFromReadableHex() ?? perUserTheme?.primaryVariant2?.colorFromReadableHex() ?? colors.primaryVariant2?.colorFromReadableHex() ?? presetWallpaperTheme?.primaryVariant2?.colorFromReadableHex() ?? baseColors.primaryVariant2
        c.sentMessage = if let c = perChatTheme?.sentMessage { c.colorFromReadableHex() } else if let perUserTheme, (perChatWallpaperType == nil || perUserWallpaperType == nil || perChatWallpaperType!.sameType(perUserWallpaperType)) { perUserTheme.sentMessage?.colorFromReadableHex() ?? sentMessageFallback } else { sentMessageFallback }
        c.sentQuote = if let c = perChatTheme?.sentQuote { c.colorFromReadableHex() } else if let perUserTheme, (perChatWallpaperType == nil || perUserWallpaperType == nil || perChatWallpaperType!.sameType(perUserWallpaperType)) { perUserTheme.sentQuote?.colorFromReadableHex() ?? sentQuoteFallback } else { sentQuoteFallback }
        c.receivedMessage = if let c = perChatTheme?.receivedMessage { c.colorFromReadableHex() } else if let perUserTheme, (perChatWallpaperType == nil || perUserWallpaperType == nil || perChatWallpaperType!.sameType(perUserWallpaperType)) { perUserTheme.receivedMessage?.colorFromReadableHex() ?? receivedMessageFallback }
        else { receivedMessageFallback }
        c.receivedQuote = if let c = perChatTheme?.receivedQuote { c.colorFromReadableHex() } else if let perUserTheme, (perChatWallpaperType == nil || perUserWallpaperType == nil || perChatWallpaperType!.sameType(perUserWallpaperType)) { perUserTheme.receivedQuote?.colorFromReadableHex() ?? receivedQuoteFallback } else { receivedQuoteFallback }
        return c
    }

    public func toAppWallpaper(_ themeOverridesForType: WallpaperType?, _ perChatTheme: ThemeModeOverride?, _ perUserTheme: ThemeModeOverride?, _ themeBackgroundColor: Color) -> AppWallpaper {
        let mainType: WallpaperType
        if let t = themeOverridesForType { mainType = t }
        // type can be nil if override is empty `"wallpaper": "{}"`, in this case no wallpaper is needed, empty.
        // It's not nil to override upper level wallpaper
        else if let w = perChatTheme?.wallpaper { mainType = w.toAppWallpaper().type }
        else if let w = perUserTheme?.wallpaper { mainType = w.toAppWallpaper().type }
        else if let w = wallpaper { mainType = w.toAppWallpaper().type }
        else { return AppWallpaper(background: nil, tint: nil, type: WallpaperType.Empty) }

        let first: ThemeWallpaper? = if mainType.sameType(perChatTheme?.wallpaper?.toAppWallpaper().type) { perChatTheme?.wallpaper } else { nil }
        let second: ThemeWallpaper? = if mainType.sameType(perUserTheme?.wallpaper?.toAppWallpaper().type) { perUserTheme?.wallpaper } else { nil }
        let third: ThemeWallpaper? = if mainType.sameType(self.wallpaper?.toAppWallpaper().type) { self.wallpaper } else { nil }

        let wallpaper: WallpaperType
        switch mainType {
        case let WallpaperType.Preset(preset, scale):
            let scale = if themeOverridesForType == nil { scale ?? first?.scale ?? second?.scale ?? third?.scale } else { second?.scale ?? third?.scale ?? scale }
            wallpaper = WallpaperType.Preset(preset, scale)
        case let WallpaperType.Image(filename, scale, scaleType):
            let scale = if themeOverridesForType == nil { scale ?? first?.scale ?? second?.scale ?? third?.scale } else { second?.scale ?? third?.scale ?? scale }
            let scaleType = if themeOverridesForType == nil { scaleType ?? first?.scaleType ?? second?.scaleType ?? third?.scaleType } else { second?.scaleType ?? third?.scaleType ?? scaleType }
            let imageFile = if themeOverridesForType == nil { filename } else { first?.imageFile ?? second?.imageFile ?? third?.imageFile ?? filename }
            wallpaper = WallpaperType.Image(imageFile, scale, scaleType)
        case WallpaperType.Empty:
            wallpaper = WallpaperType.Empty
        }
        let background = (first?.background ?? second?.background ?? third?.background)?.colorFromReadableHex() ?? mainType.defaultBackgroundColor(base, themeBackgroundColor)
        let tint = (first?.tint ?? second?.tint ?? third?.tint)?.colorFromReadableHex() ?? mainType.defaultTintColor(base)

        return AppWallpaper(background: background, tint: tint, type: wallpaper)
    }

    public func withFilledColors(_ base: DefaultTheme, _ perChatTheme: ThemeColors?, _ perChatWallpaperType: WallpaperType?, _ perUserTheme: ThemeColors?, _ perUserWallpaperType: WallpaperType?, _ presetWallpaperTheme: ThemeColors?) -> ThemeColors {
        let c = toColors(base, perChatTheme, perUserTheme, presetWallpaperTheme)
        let ac = toAppColors(base, perChatTheme, perChatWallpaperType, perUserTheme, perUserWallpaperType, presetWallpaperTheme)
        return ThemeColors(
            primary: c.primary.toReadableHex(),
            primaryVariant: c.primaryVariant.toReadableHex(),
            secondary: c.secondary.toReadableHex(),
            secondaryVariant: c.secondaryVariant.toReadableHex(),
            background: c.background.toReadableHex(),
            surface: c.surface.toReadableHex(),
            title: ac.title.toReadableHex(),
            primaryVariant2: ac.primaryVariant2.toReadableHex(),
            sentMessage: ac.sentMessage.toReadableHex(),
            sentQuote: ac.sentQuote.toReadableHex(),
            receivedMessage: ac.receivedMessage.toReadableHex(),
            receivedQuote: ac.receivedQuote.toReadableHex()
        )
    }
}

extension [ThemeOverrides] {
    public func getTheme(_ themeId: String?) -> ThemeOverrides? {
        self.first { $0.themeId == themeId }
    }

    public func getTheme(_ themeId: String?, _ type: WallpaperType?, _ base: DefaultTheme) -> ThemeOverrides? {
        self.first { $0.themeId == themeId || $0.isSame(type, base.themeName) }
    }

    public func replace(_ theme: ThemeOverrides) -> [ThemeOverrides] {
        let index = self.firstIndex { $0.themeId == theme.themeId ||
            // prevent situation when two themes has the same type but different theme id (maybe something was changed in prefs by hand)
            $0.isSame(WallpaperType.from(theme.wallpaper), theme.base.themeName)
        }
        var a = self.map { $0 }
        if let index {
            a[index] = theme
        } else {
            a.append(theme)
        }
        return a
    }

    public func sameTheme(_ type: WallpaperType?, _ themeName: String) -> ThemeOverrides? { first { $0.isSame(type, themeName) } }

    public func skipDuplicates() -> [ThemeOverrides] {
        var res: [ThemeOverrides] = []
        self.forEach { theme in
            let themeType = WallpaperType.from(theme.wallpaper)
            if !res.contains(where: { $0.themeId == theme.themeId || $0.isSame(themeType, theme.base.themeName) }) {
                res.append(theme)
            }
        }
        return res
    }

}

public struct ThemeModeOverrides: Codable {
    public var light: ThemeModeOverride? = nil
    public var dark: ThemeModeOverride? = nil

    public init(light: ThemeModeOverride? = nil, dark: ThemeModeOverride? = nil) {
        self.light = light
        self.dark = dark
    }

    public func preferredMode(_ darkTheme: Bool) -> ThemeModeOverride? {
        darkTheme ? dark : light
    }
}

public struct ThemeModeOverride: Codable, Equatable {
    public var mode: DefaultThemeMode// = CurrentColors.base.mode
    public var colors: ThemeColors = ThemeColors()
    public var wallpaper: ThemeWallpaper? = nil

    public init(mode: DefaultThemeMode, colors: ThemeColors = ThemeColors(), wallpaper: ThemeWallpaper? = nil) {
        self.mode = mode
        self.colors = colors
        self.wallpaper = wallpaper
    }

    public var type: WallpaperType? { WallpaperType.from(wallpaper) }

    public func withUpdatedColor(_ name: ThemeColor, _ color: String?) -> ThemeModeOverride {
        var c = colors
        var w = wallpaper
        switch (name) {
        case ThemeColor.PRIMARY: c.primary = color
        case ThemeColor.PRIMARY_VARIANT: c.primaryVariant = color
        case ThemeColor.SECONDARY: c.secondary = color
        case ThemeColor.SECONDARY_VARIANT: c.secondaryVariant = color
        case ThemeColor.BACKGROUND: c.background = color
        case ThemeColor.SURFACE: c.surface = color
        case ThemeColor.TITLE: c.title = color
        case ThemeColor.PRIMARY_VARIANT2: c.primaryVariant2 = color
        case ThemeColor.SENT_MESSAGE: c.sentMessage = color
        case ThemeColor.SENT_QUOTE: c.sentQuote = color
        case ThemeColor.RECEIVED_MESSAGE: c.receivedMessage = color
        case ThemeColor.RECEIVED_QUOTE: c.receivedQuote = color
        case ThemeColor.WALLPAPER_BACKGROUND: w?.background = color
        case ThemeColor.WALLPAPER_TINT: w?.tint = color
        }
        return ThemeModeOverride(mode: mode, colors: c, wallpaper: w)
    }

    public static func withFilledAppDefaults(_ mode: DefaultThemeMode, _ base: DefaultTheme) -> ThemeModeOverride {
        ThemeModeOverride(
            mode: mode,
            colors: ThemeOverrides(base: base).withFilledColors(base, nil, nil, nil, nil, nil),
            wallpaper: ThemeWallpaper(preset: PresetWallpaper.school.filename)
        )
    }
}

public let DarkColorPalette = Colors(
    primary: SimplexBlue,
    primaryVariant: SimplexBlue,
    secondary: HighOrLowlight,
    secondaryVariant: DarkGray,
    background: Color.black,
    surface: Color(0xFF222222),
    error: Color.red,
    onBackground: Color.white,
    onSurface: Color.white,
    isLight: false
)
public let DarkColorPaletteApp = AppColors(
    title: .white,
    primaryVariant2: Color(0xFF18262E),
    sentMessage: Color(0xFF18262E),
    sentQuote: Color(0xFF1D3847),
    receivedMessage: Color(0xff262627),
    receivedQuote: Color(0xff373739)
)

public let LightColorPalette = Colors (
    primary: SimplexBlue,
    primaryVariant: SimplexBlue,
    secondary: HighOrLowlight,
    secondaryVariant: LightGray,
    background: Color.white,
    surface: Color.white,
    error: Color.red,
    onBackground: Color.black,
    onSurface: Color.black,
    isLight: true
)
public let LightColorPaletteApp = AppColors(
    title: .black,
    primaryVariant2: Color(0xFFE9F7FF),
    sentMessage: Color(0xFFE9F7FF),
    sentQuote: Color(0xFFD6F0FF),
    receivedMessage: Color(0xfff5f5f6),
    receivedQuote: Color(0xffececee)
)

public let SimplexColorPalette = Colors(
    primary: Color(0xFF70F0F9),
    primaryVariant: Color(0xFF1298A5),
    secondary: HighOrLowlight,
    secondaryVariant: Color(0xFF2C464D),
    background: Color(0xFF111528),
    surface: Color(0xFF121C37),
    error: Color.red,
    onBackground: Color.white,
    onSurface: Color.white,
    isLight: false
)
public let SimplexColorPaletteApp = AppColors(
    title: .white,
    primaryVariant2: Color(0xFF172941),
    sentMessage: Color(0xFF172941),
    sentQuote: Color(0xFF1C3A57),
    receivedMessage: Color(0xff25283a),
    receivedQuote: Color(0xff36394a)
)

public let BlackColorPalette = Colors(
    primary: Color(0xff0077e0),
    primaryVariant: Color(0xff0077e0),
    secondary: HighOrLowlight,
    secondaryVariant: DarkGray,
    background: Color(0xff070707),
    surface: Color(0xff161617),
    error: Color.red,
    onBackground: Color.white,
    onSurface: Color.white,
    isLight: false
)
public let BlackColorPaletteApp = AppColors(
    title: .white,
    primaryVariant2: Color(0xff243747),
    sentMessage: Color(0xFF18262E),
    sentQuote: Color(0xFF1D3847),
    receivedMessage: Color(0xff1b1b1b),
    receivedQuote: Color(0xff29292b)
)

public var systemInDarkThemeCurrently: Bool = false

//func isSystemInDarkTheme(): Bool

extension Colors {
    public func updateColorsFrom(_ other: Colors) {
        primary = other.primary
        primaryVariant = other.primaryVariant
        secondary = other.secondary
        secondaryVariant = other.secondaryVariant
        background = other.background
        surface = other.surface
        error = other.error
        onBackground = other.onBackground
        onSurface = other.onSurface
        isLight = other.isLight
    }
}

extension AppColors {
    public func updateColorsFrom(_ other: AppColors) {
        title = other.title
        primaryVariant2 = other.primaryVariant2
        sentMessage = other.sentMessage
        sentQuote = other.sentQuote
        receivedMessage = other.receivedMessage
        receivedQuote = other.receivedQuote
    }
}

extension AppWallpaper {
    public func updateWallpaperFrom(_ other: AppWallpaper) {
        background = other.background
        tint = other.tint
        type = other.type
    }
}
