//
//  CIReportView.swift
//  SimpleX (iOS)
//
//  Created by Diogo Cunha on 02/01/2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIReportView: View {
    @EnvironmentObject var theme: AppTheme
    let text: String
    let reason: ReportReason
    
    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            reportReason()
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
    }
    
    @ViewBuilder func reportReason() -> some View {
        HStack(spacing: 6) {
            Image(systemName: "flag")
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 14, height: 14)
            Text(text.isEmpty ? reason.text : "\(reason.text): \(text)")
                .font(.caption)
        }
        .foregroundColor(theme.colors.secondary)
    }
}
