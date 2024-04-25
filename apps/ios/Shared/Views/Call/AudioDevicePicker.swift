//
//  MPVolumeView.swift
//  SimpleX (iOS)
//
//  Created by Stanislav on 24.04.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import UIKit
import MediaPlayer

struct AudioDevicePicker: UIViewRepresentable {
    func makeUIView(context: Context) -> some UIView {
        let v = MPVolumeView(frame: .zero)
        v.showsVolumeSlider = false
        return v
    }

    func updateUIView(_ uiView: UIViewType, context: Context) {

    }
}
