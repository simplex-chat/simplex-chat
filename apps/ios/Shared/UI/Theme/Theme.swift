//
//  Theme.swift
//  SimpleX (iOS)
//
//  Created by Avently on 03.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import Foundation
import SimpleXChat

enum DefaultTheme: String, Codable {
    case LIGHT
    case DARK
    case SIMPLEX
    case BLACK

    static let SYSTEM_THEME_NAME: String = "SYSTEM"

    var themeName: String { self.rawValue }

    var mode: DefaultThemeMode {
        self == .LIGHT
        ? DefaultThemeMode.light
        : DefaultThemeMode.dark
    }

    // Call it only with base theme, not SYSTEM
    func hasChangedAnyColor(_ overrides: ThemeOverrides?) -> Bool {
        if let overrides {
            overrides.colors != ThemeColors() || overrides.wallpaper != nil && (overrides.wallpaper?.background != nil || overrides.wallpaper?.tint != nil)
        } else {
            false
        }
    }
}

enum DefaultThemeMode: String, Codable {
    case light
    case dark
}

protocol Copying {
    init(instance: Self)
}

extension Copying {
    func copy() -> Self {
        Self.init(instance: self)
    }
}

class Colors: ObservableObject {
    @Published var primary: Color
    @Published var primaryVariant: Color
    @Published var secondary: Color
    @Published var secondaryVariant: Color
    @Published var background: Color
    @Published var surface: Color
    @Published var error: Color
    @Published var onBackground: Color
    @Published var onSurface: Color
    @Published var isLight: Bool

    init(primary: Color, primaryVariant: Color, secondary: Color, secondaryVariant: Color, background: Color, surface: Color, error: Color, onBackground: Color, onSurface: Color, isLight: Bool) {
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
}

class AppColors: ObservableObject {
    @Published var title: Color
    @Published var primaryVariant2: Color
    @Published var sentMessage: Color
    @Published var sentQuote: Color
    @Published var receivedMessage: Color
    @Published var receivedQuote: Color

    init(title: Color, primaryVariant2: Color, sentMessage: Color, sentQuote: Color, receivedMessage: Color, receivedQuote: Color) {
        self.title = title
        self.primaryVariant2 = primaryVariant2
        self.sentMessage = sentMessage
        self.sentQuote = sentQuote
        self.receivedMessage = receivedMessage
        self.receivedQuote = receivedQuote
    }

    func copy(
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

class AppWallpaper: ObservableObject {
    @Published var background: Color? = nil
    @Published var tint: Color? = nil
    @Published var type: WallpaperType = WallpaperType.Empty

    init(background: Color?, tint: Color?, type: WallpaperType) {
        self.background = background
        self.tint = tint
        self.type = type
    }

    func copyWithoutDefault(_ background: Color?, _ tint: Color?, _ type: WallpaperType) -> AppWallpaper {
        AppWallpaper(
            background: background,
            tint: tint,
            type: type
        )
    }
}

enum ThemeColor {
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

    func fromColors(_ colors: Colors, _ appColors: AppColors, _ appWallpaper: AppWallpaper) -> Color? {
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

    var text: LocalizedStringKey {
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

struct ThemeColors: Codable, Equatable{
    var accent: String? = nil
    var primary: String? { accent }
    var accentVariant: String? = nil
    var primaryVariant: String? { accentVariant }
    var secondary: String? = nil
    var secondaryVariant: String? = nil
    var background: String? = nil
    var menus: String? = nil
    var surface: String? { menus }
    var title: String? = nil
    var accentVariant2: String? = nil
    var primaryVariant2: String? { accentVariant2 }
    var sentMessage: String? = nil
    var sentReply: String? = nil
    var sentQuote: String? { sentReply }
    var receivedMessage: String? = nil
    var receivedReply: String? = nil
    var receivedQuote: String? { receivedReply }

    static func from(sentMessage: String, sentQuote: String, receivedMessage: String, receivedQuote: String) -> ThemeColors {
        var c = ThemeColors()
        c.sentMessage = sentMessage
        c.sentReply = sentQuote
        c.receivedMessage = receivedMessage
        c.receivedReply = receivedQuote
        return c
    }

    static func from(_ colors: Colors, _ appColors: AppColors) -> ThemeColors {
        ThemeColors(
            accent: colors.primary.toReadableHex(),
            accentVariant: colors.primaryVariant.toReadableHex(),
            secondary: colors.secondary.toReadableHex(),
            secondaryVariant: colors.secondaryVariant.toReadableHex(),
            background: colors.background.toReadableHex(),
            menus: colors.surface.toReadableHex(),
            title: appColors.title.toReadableHex(),
            accentVariant2: appColors.primaryVariant2.toReadableHex(),
            sentMessage: appColors.sentMessage.toReadableHex(),
            sentReply: appColors.sentQuote.toReadableHex(),
            receivedMessage: appColors.receivedMessage.toReadableHex(),
            receivedReply: appColors.receivedQuote.toReadableHex()
        )
    }
}

public struct ThemeWallpaper: Codable {
    public var preset: String?
    public var scale: Float?
    public var scaleType: WallpaperScaleType?
    public var background: String?
    public var tint: String?
    public var image: String?
    public var imageFile: String?

    func toAppWallpaper() -> AppWallpaper {
        AppWallpaper (
            background: background?.colorFromReadableHex(),
            tint: tint?.colorFromReadableHex(),
            type: WallpaperType.from(self) ?? WallpaperType.Empty
        )
    }

    func withFilledWallpaperBase64() -> ThemeWallpaper {
        let aw = toAppWallpaper()
        let type = aw.type
        let preset: String? = if case let WallpaperType.Preset(filename, _) = type { filename } else { nil }
        let scale: Float? = if case let WallpaperType.Preset(_, scale) = type { scale } else { if case let WallpaperType.Image(_, scale, _) = type { scale } else { 1.0 } }
        let scaleType: WallpaperScaleType? = if case let WallpaperType.Image(_, _, scaleType) = type { scaleType } else { nil }
        // LALAL
        let image: String? = ""// = if case WallpaperType.Image = type, let image = type.image { resizeImageToStrSize(image, 5_000_000) } else { nil }
        return ThemeWallpaper (
            preset: preset,
            scale: scale,
            scaleType: scaleType,
            background: aw.background?.toReadableHex(),
            tint: aw.tint?.toReadableHex(),
            image: image,
            imageFile: nil
        )
    }

    func withFilledWallpaperPath() -> ThemeWallpaper {
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
        let scaleType: WallpaperScaleType? = if scaleType == nil { nil } else if let WallpaperType.Image(_, _, scaleType) = type { scaleType } else { nil }
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

    func importFromString() -> ThemeWallpaper {
        self
        //if preset == nil, let image {
            // Need to save image from string and to save its path
//            do {
//                let parsed = base64ToBitmap(image)
//                let filename = saveWallpaperFile(parsed)
//                return copy(image = nil, imageFile = filename)
//            } catch let e {
//                logger.error("Error while parsing/copying the image: \(e)")
//                return ThemeWallpaper()
//            }
//        } else {
//            self
//        }
    }

    static func from(_ type: WallpaperType, _ background: String?, _ tint: String?) -> ThemeWallpaper {
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

public struct ThemesFile: Codable {
    var themes: [ThemeOverrides] = []
}

public struct ThemeOverrides: Codable {
    var themeId: String = UUID().uuidString
    var base: DefaultTheme
    var colors: ThemeColors = ThemeColors()
    var wallpaper: ThemeWallpaper? = nil

    func isSame(_ type: WallpaperType?, _ themeName: String) -> Bool {
        (
            (wallpaper?.preset != nil && type is WallpaperType.Preset && wallpaper.preset == type.filename) ||
            (wallpaper?.imageFile != nil && type is WallpaperType.Image) ||
            (wallpaper?.preset == nil && wallpaper?.imageFile == nil && (type == WallpaperType.Empty || type == nil))
        ) && base.themeName == themeName
    }

    func withUpdatedColor(_ name: ThemeColor, _ color: String?) -> ThemeOverrides {
        var c = colors
        var w = wallpaper
            switch name {
            case ThemeColor.PRIMARY: c.accent = color
            case ThemeColor.PRIMARY_VARIANT: c.accentVariant = color
            case ThemeColor.SECONDARY: c.secondary = color
            case ThemeColor.SECONDARY_VARIANT: c.secondaryVariant = color
            case ThemeColor.BACKGROUND: c.background = color
            case ThemeColor.SURFACE: c.menus = color
            case ThemeColor.TITLE: c.title = color
            case ThemeColor.PRIMARY_VARIANT2: c.accentVariant2 = color
            case ThemeColor.SENT_MESSAGE: c.sentMessage = color
            case ThemeColor.SENT_QUOTE: c.sentReply = color
            case ThemeColor.RECEIVED_MESSAGE: c.receivedMessage = color
            case ThemeColor.RECEIVED_QUOTE: c.receivedReply = color
            case ThemeColor.WALLPAPER_BACKGROUND: w.background = color
            case ThemeColor.WALLPAPER_TINT: w.tint = color
            }
        )
        return ThemeOverrides(themeId: themeId, base: base, colors: c, wallpaper: w)
    }

    func toColors(_ base: DefaultTheme, _ perChatTheme: ThemeColors?, _ perUserTheme: ThemeColors?, _ presetWallpaperTheme: ThemeColors?) -> Colors {
        let baseColors = switch base {
            case DefaultTheme.LIGHT: LightColorPalette
            case DefaultTheme.DARK: DarkColorPalette
            case DefaultTheme.SIMPLEX: SimplexColorPalette
            case DefaultTheme.BLACK: BlackColorPalette
        }
        let c = baseColors
        c.primary = perChatTheme?.primary?.colorFromReadableHex() ?? perUserTheme?.primary?.colorFromReadableHex() ?? colors.primary?.colorFromReadableHex() ?? presetWallpaperTheme?.primary?.colorFromReadableHex() ?? baseColors.primary
        c.primaryVariant = perChatTheme?.primaryVariant?.colorFromReadableHex() ?? perUserTheme?.primaryVariant?.colorFromReadableHex() ?? colors.primaryVariant?.colorFromReadableHex() ?? presetWallpaperTheme?.primaryVariant?.colorFromReadableHex() ?? baseColors.primaryVariant
        c.secondary = perChatTheme?.secondary?.colorFromReadableHex() ?? perUserTheme?.secondary?.colorFromReadableHex() ?? colors.secondary?.colorFromReadableHex() ?? presetWallpaperTheme?.secondary?.colorFromReadableHex() ?? baseColors.secondary
        c.secondaryVariant = perChatTheme?.secondaryVariant?.colorFromReadableHex() ?? perUserTheme?.secondaryVariant?.colorFromReadableHex() ?? colors.secondaryVariant?.colorFromReadableHex() ?? presetWallpaperTheme?.secondaryVariant?.colorFromReadableHex() ?? baseColors.secondaryVariant
        c.background = perChatTheme?.background?.colorFromReadableHex() ?? perUserTheme?.background?.colorFromReadableHex() ?? colors.background?.colorFromReadableHex() ?? presetWallpaperTheme?.background?.colorFromReadableHex() ?? baseColors.background
        c.surface = perChatTheme?.surface?.colorFromReadableHex() ?? perUserTheme?.surface?.colorFromReadableHex() ?? colors.surface?.colorFromReadableHex() ?? presetWallpaperTheme?.surface?.colorFromReadableHex() ?? baseColors.surface
        )
        return c
    }

    func toAppColors(_ base: DefaultTheme, _ perChatTheme: ThemeColors?, _ perChatWallpaperType: WallpaperType?, _ perUserTheme: ThemeColors?, _ perUserWallpaperType: WallpaperType?, _ presetWallpaperTheme: ThemeColors?) -> AppColors {
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
        var c = baseColors
        c.title = perChatTheme?.title?.colorFromReadableHex() ?? perUserTheme?.title?.colorFromReadableHex() ?? colors.title?.colorFromReadableHex() ?? presetWallpaperTheme?.title?.colorFromReadableHex() ?? baseColors.title
        c.primaryVariant2 = perChatTheme?.primaryVariant2?.colorFromReadableHex() ?? perUserTheme?.primaryVariant2?.colorFromReadableHex() ?? colors.primaryVariant2?.colorFromReadableHex() ?? presetWallpaperTheme?.primaryVariant2?.colorFromReadableHex() ?? baseColors.primaryVariant2
        c.sentMessage = if let c = perChatTheme?.sentMessage { c.colorFromReadableHex() } else if perUserTheme != nil && (perChatWallpaperType == nil || perUserWallpaperType == nil || perChatWallpaperType!.sameType(perUserWallpaperType)) { perUserTheme?.sentMessage?.colorFromReadableHex() ?? sentMessageFallback } else { sentMessageFallback }
        c.sentQuote = if let c = perChatTheme?.sentQuote { c.colorFromReadableHex() } else if let perUserTheme, (perChatWallpaperType == nil || perUserWallpaperType == nil || perChatWallpaperType!.sameType(perUserWallpaperType)) { perUserTheme.sentQuote?.colorFromReadableHex() ?? sentQuoteFallback } else { sentQuoteFallback }
        c.receivedMessage = if let c = perChatTheme?.receivedMessage { c.colorFromReadableHex() } else if let perUserTheme, (perChatWallpaperType == nil || perUserWallpaperType == nil || perChatWallpaperType!.sameType(perUserWallpaperType)) { perUserTheme.receivedMessage?.colorFromReadableHex() ?? receivedMessageFallback }
        else { receivedMessageFallback }
        c.receivedQuote = if let c = perChatTheme?.receivedQuote { c.colorFromReadableHex() } else if let perUserTheme, (perChatWallpaperType == nil || perUserWallpaperType == nil || perChatWallpaperType?.sameType(perUserWallpaperType)) { perUserTheme.receivedQuote?.colorFromReadableHex() ?? receivedQuoteFallback } else { receivedQuoteFallback }
    }

    func toAppWallpaper(_ themeOverridesForType: WallpaperType?, _ perChatTheme: ThemeModeOverride?, _ perUserTheme: ThemeModeOverride?, _ materialBackgroundColor: Color) -> AppWallpaper {
        let mainType = if themeOverridesForType != nil { themeOverridesForType }
        // type can be nil if override is empty `"wallpaper": "{}"`, in this case no wallpaper is needed, empty.
        // It's not nil to override upper level wallpaper
        else if let w = perChatTheme?.wallpaper { w.toAppWallpaper().type }
        else if let w = perUserTheme?.wallpaper { w.toAppWallpaper().type }
        else if let w = wallpaper { w.toAppWallpaper().type }
        else { return AppWallpaper() }

        let first: ThemeWallpaper? = if mainType.sameType(perChatTheme?.wallpaper?.toAppWallpaper()?.type) { perChatTheme?.wallpaper } else { nil }
        let second: ThemeWallpaper? = if mainType.sameType(perUserTheme?.wallpaper?.toAppWallpaper()?.type) { perUserTheme?.wallpaper } else { nil }
        let third: ThemeWallpaper? = if mainType.sameType(self.wallpaper?.toAppWallpaper()?.type) { self.wallpaper } else { nil }

        switch mainType {
        case WallpaperType.Preset:
            mainType.scale = mainType.scale ?? first?.scale ?? second?.scale ?? third?.scale
        case WallpaperType.Image:
            mainType.scale = if themeOverridesForType == nil { mainType.scale ?? first?.scale ?? second?.scale ?? third?.scale } else { second?.scale ?? third?.scale ?? mainType.scale }
            mainType.scaleType = if themeOverridesForType == nil { mainType.scaleType ?? first?.scaleType ?? second?.scaleType ?? third?.scaleType } else { second?.scaleType ?? third?.scaleType ?? mainType.scaleType }
            mainType.filename = if themeOverridesForType == nil { mainType.filename } else { first?.imageFile ?? second?.imageFile ?? third?.imageFile ?? mainType.filename }
        case WallpaperType.Empty: ()
        }
        mainType.background = (first?.background ?? second?.background ?? third?.background)?.colorFromReadableHex() ?? mainType.defaultBackgroundColor(base, materialBackgroundColor)
        mainType.tint = (first?.tint ?? second?.tint ?? third?.tint)?.colorFromReadableHex() ?? mainType.defaultTintColor(base)

        return AppWallpaper(background: nil, tint: nil, type: mainType)
    }

    func withFilledColors(_ base: DefaultTheme, _ perChatTheme: ThemeColors?, _ perChatWallpaperType: WallpaperType?, _ perUserTheme: ThemeColors?, _ perUserWallpaperType: WallpaperType?, _ presetWallpaperTheme: ThemeColors?) -> ThemeColors {
        let c = toColors(base, perChatTheme, perUserTheme, presetWallpaperTheme)
        let ac = toAppColors(base, perChatTheme, perChatWallpaperType, perUserTheme, perUserWallpaperType, presetWallpaperTheme)
        return ThemeColors(
            accent: c.primary.toReadableHex(),
            accentVariant: c.primaryVariant.toReadableHex(),
            secondary: c.secondary.toReadableHex(),
            secondaryVariant: c.secondaryVariant.toReadableHex(),
            background: c.background.toReadableHex(),
            menus: c.surface.toReadableHex(),
            title: ac.title.toReadableHex(),
            accentVariant2: ac.primaryVariant2.toReadableHex(),
            sentMessage: ac.sentMessage.toReadableHex(),
            sentReply: ac.sentQuote.toReadableHex(),
            receivedMessage: ac.receivedMessage.toReadableHex(),
            receivedReply: ac.receivedQuote.toReadableHex()
        )
    }
}

extension [ThemeOverrides] {
    func getTheme(_ themeId: String?) -> ThemeOverrides? {
        self.first { $0.themeId == themeId }
    }

    func getTheme(_ themeId: String?, _ type: WallpaperType?, _ base: DefaultTheme) -> ThemeOverrides? {
        self.first { $0.themeId == themeId || $0.isSame(type, base.themeName) }
    }

    func replace(_ theme: ThemeOverrides) -> [ThemeOverrides] {
        let index = self.firstIndex { $0.themeId == theme.themeId ||
            // prevent situation when two themes has the same type but different theme id (maybe something was changed in prefs by hand)
            $0.isSame(WallpaperType.from(theme.wallpaper), theme.base.themeName)
        }
        var a = self // LALAL SHOULD BE A CLONE
        if let index {
            a[index] = theme
        } else {
            a.append(theme)
        }
        return a
    }

    func sameTheme(_ type: WallpaperType?, _ themeName: String) -> ThemeOverrides? { first { $0.isSame(type, themeName) } }

    func skipDuplicates() -> [ThemeOverrides] {
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

struct ThemeModeOverrides: Codable {
    var light: ThemeModeOverride? = nil
    var dark: ThemeModeOverride? = nil

    func preferredMode(_ darkTheme: Bool) -> ThemeModeOverride? {
        darkTheme ? dark : light
    }
}

struct ThemeModeOverride: Codable {
    var mode: DefaultThemeMode = CurrentColors.base.mode
    var colors: ThemeColors = ThemeColors()
    var wallpaper: ThemeWallpaper? = nil

    var type: WallpaperType? { WallpaperType.from(wallpaper) }

    func withUpdatedColor(_ name: ThemeColor, _ color: String?) -> ThemeModeOverride {
        var c = colors
        var w = wallpaper
        switch (name) {
        case ThemeColor.PRIMARY: c.accent = color
        case ThemeColor.PRIMARY_VARIANT: c.accentVariant = color
        case ThemeColor.SECONDARY: c.secondary = color
        case ThemeColor.SECONDARY_VARIANT: c.secondaryVariant = color
        case ThemeColor.BACKGROUND: c.background = color
        case ThemeColor.SURFACE: c.menus = color
        case ThemeColor.TITLE: c.title = color
        case ThemeColor.PRIMARY_VARIANT2: c.accentVariant2 = color
        case ThemeColor.SENT_MESSAGE: c.sentMessage = color
        case ThemeColor.SENT_QUOTE: c.sentReply = color
        case ThemeColor.RECEIVED_MESSAGE: c.receivedMessage = color
        case ThemeColor.RECEIVED_QUOTE: c.receivedReply = color
        case ThemeColor.WALLPAPER_BACKGROUND: w?.background = color
        case ThemeColor.WALLPAPER_TINT: w?.tint = color
        }
        return ThemeModeOverride(mode: mode, colors: c, wallpaper: w)
    }

    static func withFilledAppDefaults(_ mode: DefaultThemeMode, _ base: DefaultTheme) -> ThemeModeOverride {
        ThemeModeOverride(
            mode: mode,
            colors: ThemeOverrides(base: base).withFilledColors(base, nil, nil, nil, nil, nil),
            wallpaper: ThemeWallpaper(preset: PresetWallpaper.school.filename)
        )
    }
}

struct ThemedBackground: ViewModifier {
    var baseTheme: DefaultTheme = CurrentColors.base

    func body(content: Content) -> some View {
        if baseTheme == DefaultTheme.SIMPLEX {
            content.background(
/*
 brush = Brush.linearGradient(
 listOf(
 CurrentColors.value.colors.background.darker(0.4f),
 CurrentColors.value.colors.background.lighter(0.4f)
 ),
 Offset(0f, Float.POSITIVE_INFINITY),
 Offset(Float.POSITIVE_INFINITY, 0f)
 */
            )
        } else {
            content.background(CurrentColors.colors.background)
        }
    }
}

let DEFAULT_PADDING = 20
let DEFAULT_SPACE_AFTER_ICON = 4
let DEFAULT_PADDING_HALF = DEFAULT_PADDING / 2
let DEFAULT_BOTTOM_PADDING = 48
let DEFAULT_BOTTOM_BUTTON_PADDING = 20

let DEFAULT_START_MODAL_WIDTH = 388
let DEFAULT_MIN_CENTER_MODAL_WIDTH = 590
let DEFAULT_END_MODAL_WIDTH = 388
let DEFAULT_MAX_IMAGE_WIDTH = 500

let DarkColorPalette = Colors(
    primary: SimplexBlue,
    primaryVariant: SimplexBlue,
    secondary: HighOrLowlight,
    secondaryVariant: DarkGray,
    background: Color(0xFF121212),
    surface: Color(0xFF222222),
    error: Color.red,
    onBackground: Color(0xFFFFFBFA),
    onSurface: Color(0xFFFFFBFA),
    isLight: false
)
let DarkColorPaletteApp = AppColors(
    title: SimplexBlue,
    primaryVariant2: Color(0xFF18262E),
    sentMessage: Color(0xFF18262E),
    sentQuote: Color(0xFF1D3847),
    receivedMessage: Color(0xff262627),
    receivedQuote: Color(0xff373739)
)

let LightColorPalette = Colors (
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
let LightColorPaletteApp = AppColors(
    title: SimplexBlue,
    primaryVariant2: Color(0xFFE9F7FF),
    sentMessage: Color(0xFFE9F7FF),
    sentQuote: Color(0xFFD6F0FF),
    receivedMessage: Color(0xfff5f5f6),
    receivedQuote: Color(0xffececee)
)

let SimplexColorPalette = Colors(
    primary: Color(0xFF70F0F9),
    primaryVariant: Color(0xFF1298A5),
    secondary: HighOrLowlight,
    secondaryVariant: Color(0xFF2C464D),
    background: Color(0xFF111528),
    surface: Color(0xFF121C37),
    error: Color.red,
    onBackground: Color(0xFFFFFBFA),
    onSurface: Color(0xFFFFFBFA),
    isLight: false
)
let SimplexColorPaletteApp = AppColors(
    title: Color(0xFF267BE5),
    primaryVariant2: Color(0xFF172941),
    sentMessage: Color(0xFF172941),
    sentQuote: Color(0xFF1C3A57),
    receivedMessage: Color(0xff25283a),
    receivedQuote: Color(0xff36394a)
)

let BlackColorPalette = Colors(
    primary: Color(0xff0077e0),
    primaryVariant: Color(0xff0077e0),
    secondary: HighOrLowlight,
    secondaryVariant: DarkGray,
    background: Color(0xff070707),
    surface: Color(0xff161617),
    error: Color.red,
    onBackground: Color(0xFFFFFBFA),
    onSurface: Color(0xFFFFFBFA),
    isLight: false
)
let BlackColorPaletteApp = AppColors(
    title: Color(0xff0077e0),
    primaryVariant2: Color(0xff243747),
    sentMessage: Color(0xFF18262E),
    sentQuote: Color(0xFF1D3847),
    receivedMessage: Color(0xff1b1b1b),
    receivedQuote: Color(0xff29292b)
)

var systemInDarkThemeCurrently: Bool = false

extension User {
    var uiThemes: ThemeModeOverrides {
        ThemeModeOverrides() // LALAL remove it
    }
}

var CurrentColors: ThemeManager.ActiveTheme = ThemeManager.currentColors(nil, nil, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get()) {
    didSet {
        MaterialTheme.shared.name = CurrentColors.name
        MaterialTheme.shared.base = CurrentColors.base
        MaterialTheme.shared.colors.updateColorsFrom(CurrentColors.colors)
        MaterialTheme.shared.appColors.updateColorsFrom(CurrentColors.appColors)
        MaterialTheme.shared.wallpaper.updateWallpaperFrom(CurrentColors.wallpaper)
    }
}

func isInDarkTheme() -> Bool { !CurrentColors.colors.isLight }

//func isSystemInDarkTheme(): Bool

class MaterialTheme: ObservableObject {
    static let shared = MaterialTheme(name: CurrentColors.name, base: CurrentColors.base, colors: CurrentColors.colors, appColors: CurrentColors.appColors, wallpaper: CurrentColors.wallpaper)

    var name: String
    var base: DefaultTheme
    @ObservedObject var colors: Colors
    @ObservedObject var appColors: AppColors
    @ObservedObject var wallpaper: AppWallpaper

    init(name: String, base: DefaultTheme, colors: Colors, appColors: AppColors, wallpaper: AppWallpaper) {
        self.name = name
        self.base = base
        self.colors = colors
        self.appColors = appColors
        self.wallpaper = wallpaper
    }
}

extension Colors {
    func updateColorsFrom(_ other: Colors) {
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
    func updateColorsFrom(_ other: AppColors) {
        title = other.title
        primaryVariant2 = other.primaryVariant2
        sentMessage = other.sentMessage
        sentQuote = other.sentQuote
        receivedMessage = other.receivedMessage
        receivedQuote = other.receivedQuote
    }
}

extension AppWallpaper {
    func updateWallpaperFrom(_ other: AppWallpaper) {
        background = other.background
        tint = other.tint
        type = other.type
    }
}

func reactOnDarkThemeChanges(_ isDark: Bool) {
    systemInDarkThemeCurrently = isDark
    //sceneDelegate.window?.overrideUserInterfaceStyle == .unspecified
    if (currentThemeDefault.get() == DefaultTheme.SYSTEM_THEME_NAME && CurrentColors.colors.isLight == isDark) {
        // Change active colors from light to dark and back based on system theme
        ThemeManager.applyTheme(DefaultTheme.SYSTEM_THEME_NAME)
    }
}

//@Composable
//func SimpleXTheme(darkTheme: Bool? = nil, content: @Composable () -> Void) {
//    val systemDark = rememberUpdatedState(isSystemInDarkTheme())
//    LaunchedEffect(Void) {
//        // snapshotFlow vs LaunchedEffect reduce number of recomposes
//        snapshotFlow { systemDark.value }
//            .collect {
//                reactOnDarkThemeChanges(systemDark.value)
//            }
//    }
//    val theme by CurrentColors.collectAsState()
//    LaunchedEffect(Void) {
//        // snapshotFlow vs LaunchedEffect reduce number of recomposes when user is changed or it's themes
//        snapshotFlow { chatModel.currentUser.value?.uiThemes }
//            .collect {
//                ThemeManager.applyTheme(appPrefs.currentTheme.get()!!)
//            }
//    }
//    MaterialTheme(
//        colors = theme.colors,
//        typography = Typography,
//        shapes = Shapes,
//        content = {
//            val rememberedAppColors = remember {
//                // Explicitly creating a new object here so we don't mutate the initial [appColors]
//                // provided, and overwrite the values set in it.
//                theme.appColors.copy()
//            }.apply { updateColorsFrom(theme.appColors) }
//            val rememberedWallpaper = remember {
//                // Explicitly creating a new object here so we don't mutate the initial [wallpaper]
//                // provided, and overwrite the values set in it.
//                theme.wallpaper.copy()
//            }.apply { updateWallpaperFrom(theme.wallpaper) }
//            CompositionLocalProvider(
//                LocalContentColor provides MaterialTheme.colors.onBackground,
//                LocalAppColors provides rememberedAppColors,
//                LocalAppWallpaper provides rememberedWallpaper,
//                content = content)
//        }
//    )
//}
//
//@Composable
//func SimpleXThemeOverride(theme: ThemeManager.ActiveTheme, content: @Composable () -> Void) {
//    MaterialTheme(
//        colors = theme.colors,
//        typography = Typography,
//        shapes = Shapes,
//        content = {
//            val rememberedAppColors = remember {
//                // Explicitly creating a new object here so we don't mutate the initial [appColors]
//                // provided, and overwrite the values set in it.
//                theme.appColors.copy()
//            }.apply { updateColorsFrom(theme.appColors) }
//            val rememberedWallpaper = remember {
//                // Explicitly creating a new object here so we don't mutate the initial [wallpaper]
//                // provided, and overwrite the values set in it.
//                theme.wallpaper.copy()
//            }.apply { updateWallpaperFrom(theme.wallpaper) }
//            CompositionLocalProvider(
//                LocalContentColor provides MaterialTheme.colors.onBackground,
//                LocalAppColors provides rememberedAppColors,
//                LocalAppWallpaper provides rememberedWallpaper,
//                content = content)
//        }
//    )
//}
