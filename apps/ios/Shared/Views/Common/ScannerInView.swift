//
//  ScannerInView.swift
//  SimpleX (iOS)
//
//  Created by Diogo Cunha on 07/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import CodeScanner
import AVFoundation

struct ScannerInView: View {
    @State private var cameraAuthorizationStatus: AVAuthorizationStatus?

    @Binding var showQRCodeScanner: Bool
    let processQRCode: (_ resp: Result<ScanResult, ScanError>) -> Void
    var scanMode: ScanMode = .continuous

    var body: some View {
        Group {
            if showQRCodeScanner, case .authorized = cameraAuthorizationStatus {
                CodeScannerView(codeTypes: [.qr], scanMode: scanMode, completion: processQRCode)
                    .aspectRatio(1, contentMode: .fit)
                    .cornerRadius(12)
                    .listRowBackground(Color.clear)
                    .listRowSeparator(.hidden)
                    .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                    .padding(.horizontal)
            } else {
                Button {
                    switch cameraAuthorizationStatus {
                    case .notDetermined: askCameraAuthorization { showQRCodeScanner = true }
                    case .restricted: ()
                    case .denied: UIApplication.shared.open(appSettingsURL)
                    case .authorized: showQRCodeScanner = true
                    default: askCameraAuthorization { showQRCodeScanner = true }
                    }
                } label: {
                    ZStack {
                        Rectangle()
                            .aspectRatio(contentMode: .fill)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                            .foregroundColor(Color.clear)
                        switch cameraAuthorizationStatus {
                        case .restricted: Text("Camera not available")
                        case .denied:  Label("Enable camera access", systemImage: "camera")
                        default: Label("Tap to scan", systemImage: "qrcode")
                        }
                    }
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .center)
                .padding()
                .background(
                    RoundedRectangle(cornerRadius: 12, style: .continuous)
                        .fill(Color(uiColor: .secondarySystemGroupedBackground))
                )
                .padding(.horizontal)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .disabled(cameraAuthorizationStatus == .restricted)
            }
        }
        .onAppear {
            let status = AVCaptureDevice.authorizationStatus(for: .video)
            cameraAuthorizationStatus = status
            if showQRCodeScanner {
                switch status {
                case .notDetermined: askCameraAuthorization()
                case .restricted: showQRCodeScanner = false
                case .denied: showQRCodeScanner = false
                case .authorized: ()
                @unknown default: askCameraAuthorization()
                }
            }
        }
    }

    func askCameraAuthorization(_ cb: (() -> Void)? = nil) {
        AVCaptureDevice.requestAccess(for: .video) { allowed in
            cameraAuthorizationStatus = AVCaptureDevice.authorizationStatus(for: .video)
            if allowed { cb?() }
        }
    }
}
