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

var MenuTextColor: Color { if isInDarkTheme() { MaterialTheme.shared.colors.onBackground.opacity(0.8) } else { Color.black } }
var NoteFolderIconColor: Color { MaterialTheme.shared.appColors.primaryVariant2 }
